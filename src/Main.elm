module Main exposing (main)

import Api.Lottery
import Browser
import Entity.Attendee exposing (Attendee)
import Html exposing (Html, button, div, h1, h2, h3, i, input, label, li, p, span, text, ul)
import Html.Attributes exposing (class, disabled, min, placeholder, step, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Misc.RemoteData as RemoteData exposing (RemoteData(..))



-- MAIN


type alias Config =
    { lotteryUrl : String
    }


main : Program Config Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- INIT


type alias Model =
    { config : Config
    , desired : Result String Int
    , inputValue : String
    , validating : Maybe ( Attendee, RemoteData Http.Error () )
    , winners : RemoteData Http.Error (List Attendee)
    }


init : Config -> ( Model, Cmd Msg )
init config =
    ( Model config (Ok 1) "1" Nothing NotAsked
    , Cmd.none
    )



-- UPDATE


type Msg
    = ConfirmWinner Attendee
    | ConfirmWinnerResponse Attendee (Result Http.Error ())
    | ListWinners
    | ListWinnersResponse (Result Http.Error (List Attendee))
    | SetDesired String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { config, desired, inputValue, validating, winners } =
            model
    in
    case msg of
        ConfirmWinner att ->
            ( { model | validating = Just ( att, Loading ) }, confirmWinner config.lotteryUrl att )

        ConfirmWinnerResponse att res ->
            case res of
                Err err ->
                    ( { model | validating = Just ( att, Failure err ) }, Cmd.none )

                Ok val ->
                    ( { model
                        | desired = Result.map decrementDesired desired
                        , inputValue = Result.map decrementDesired desired |> Result.map String.fromInt |> Result.withDefault inputValue
                        , validating = Just ( att, Success val )
                        , winners = RemoteData.map (removeAttendee att) winners
                      }
                    , Cmd.none
                    )

        ListWinners ->
            ( { model | validating = Nothing, winners = Loading }, Result.toMaybe desired |> listWinners config.lotteryUrl )

        ListWinnersResponse res ->
            case res of
                Err err ->
                    ( { model | winners = Failure err }, Cmd.none )

                Ok val ->
                    ( { model | winners = Success val }, Cmd.none )

        SetDesired str ->
            ( { model | desired = parseDesired str, inputValue = str }, Cmd.none )


listWinners : String -> Maybe Int -> Cmd Msg
listWinners url =
    Maybe.withDefault 1 >> Api.Lottery.winners url >> Http.send ListWinnersResponse


confirmWinner : String -> Attendee -> Cmd Msg
confirmWinner url att =
    Http.send (ConfirmWinnerResponse att) (Api.Lottery.record url att)


removeAttendee : Attendee -> List Attendee -> List Attendee
removeAttendee =
    (/=) >> List.filter


decrementDesired : Int -> Int
decrementDesired =
    (+) -1 >> max 1


parseDesired : String -> Result String Int
parseDesired str =
    case String.toInt str of
        Just n ->
            if n >= 1 then
                Ok n

            else
                Err "Should be >= 1"

        Nothing ->
            Err "Should be a valid integer"



-- VIEW


view : Model -> Html Msg
view { desired, inputValue, validating, winners } =
    let
        canSubmit =
            case ( desired, winners ) of
                ( Err _, _ ) ->
                    False

                ( _, Loading ) ->
                    False

                _ ->
                    True
    in
    div [ class "container is-fluid" ]
        [ h1 [ class "title is-1" ]
            [ text "Lottery JUG" ]
        , div [ class "field" ]
            [ label [ class "label" ]
                [ text "Number of winners" ]
            , input [ class "input", type_ "number", min "1", step "1", value inputValue, placeholder "Number of winners", onInput SetDesired ] []
            , case desired of
                Err err ->
                    p [ class "help is-danger" ]
                        [ text err ]

                _ ->
                    text ""
            ]
        , div [ class "field" ]
            [ div [ class "control" ]
                [ button [ class "button is-info", disabled (not canSubmit), onClick ListWinners ]
                    [ span [ class "icon" ]
                        [ i [ class "fas fa-dice" ] [] ]
                    , span []
                        [ text "GO !" ]
                    ]
                ]
            ]
        , case winners of
            NotAsked ->
                text ""

            Loading ->
                div [ class "notification" ]
                    [ text "Loading..." ]

            Failure e ->
                div [ class "notification is-danger" ]
                    [ viewHttpError e ]

            Success v ->
                div []
                    [ h2 [ class "title is-2" ]
                        [ text "Winners" ]
                    , case v of
                        [] ->
                            p []
                                [ text "All winners have been confirmed" ]

                        _ ->
                            div [ class "columns" ]
                                (List.map (viewWinnerItem validating) v)
                    ]
        ]


viewHttpError : Http.Error -> Html Msg
viewHttpError err =
    case err of
        Http.BadStatus { body, status } ->
            if status.code == 503 then
                text "No live events"

            else
                text ("Bad status " ++ String.fromInt status.code ++ ": " ++ body)

        Http.NetworkError ->
            text "Network error"

        _ ->
            text "An error occurred"


viewWinnerItem : Maybe ( Attendee, RemoteData Http.Error () ) -> Attendee -> Html Msg
viewWinnerItem validating att =
    let
        canConfirm =
            case validating of
                Just ( _, Loading ) ->
                    False

                _ ->
                    True
    in
    div [ class "column is-one-third" ]
        [ div [ class "box" ]
            [ h3 [ class "title is-3" ]
                [ text (att.firstName ++ " " ++ att.lastName) ]
            , button [ class "button is-primary", disabled (not canConfirm), onClick (ConfirmWinner att) ]
                [ span [ class "icon" ]
                    [ i [ class "fas fa-check" ] [] ]
                , span []
                    [ text "Confirm" ]
                ]
            ]
        ]
