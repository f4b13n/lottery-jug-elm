module Api.Lottery exposing (record, winners)

import Entity.Attendee exposing (Attendee)
import Http
import Json.Decode as Decode
import Url.Builder as Url


record : String -> Attendee -> Http.Request ()
record url attendee =
    Http.post
        (Url.crossOrigin url [ "record" ] [])
        (Entity.Attendee.encode attendee |> Http.jsonBody)
        (Decode.succeed ())


winners : String -> Int -> Http.Request (List Attendee)
winners url nb =
    Http.get
        (Url.crossOrigin url [ "winners" ] [ Url.int "nb" nb ])
        (Decode.list Entity.Attendee.decoder)
