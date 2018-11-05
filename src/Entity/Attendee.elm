module Entity.Attendee exposing (Attendee, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)


type alias Attendee =
    { firstName : String
    , lastName : String
    }


decoder : Decoder Attendee
decoder =
    Decode.succeed Attendee
        |> required "first_name" Decode.string
        |> required "last_name" Decode.string


encode : Attendee -> Value
encode { firstName, lastName } =
    Encode.object
        [ ( "first_name", Encode.string firstName )
        , ( "last_name", Encode.string lastName )
        ]
