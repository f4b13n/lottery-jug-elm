module Misc.RemoteData exposing (RemoteData(..), andThen, ap, map, map2, map3, map4, map5, mapFailure, pure, toMaybe, withDefault)

import List exposing (..)
import Maybe exposing (..)


type RemoteData e a
    = NotAsked
    | Loading
    | Failure e
    | Success a


pure : a -> RemoteData e a
pure =
    Success


mapFailure : (e -> f) -> RemoteData e value -> RemoteData f value
mapFailure f r =
    case r of
        NotAsked ->
            NotAsked

        Loading ->
            Loading

        Failure error ->
            Failure (f error)

        Success value ->
            Success value


map : (a -> b) -> RemoteData error a -> RemoteData error b
map f =
    andThen (f >> pure)


map2 : (a -> b -> c) -> RemoteData error a -> RemoteData error b -> RemoteData error c
map2 f ra =
    ap (map f ra)


map3 : (a -> b -> c -> d) -> RemoteData error a -> RemoteData error b -> RemoteData error c -> RemoteData error d
map3 f ra rb =
    ap (map2 f ra rb)


map4 : (a -> b -> c -> d -> e) -> RemoteData error a -> RemoteData error b -> RemoteData error c -> RemoteData error d -> RemoteData error e
map4 f ra rb rc =
    ap (map3 f ra rb rc)


map5 : (a -> b -> c -> d -> e -> f) -> RemoteData error a -> RemoteData error b -> RemoteData error c -> RemoteData error d -> RemoteData error e -> RemoteData error f
map5 f ra rb rc rd =
    ap (map4 f ra rb rc rd)


ap : RemoteData error (a -> b) -> RemoteData error a -> RemoteData error b
ap rf r =
    andThen (\f -> map f r) rf


andThen : (a -> RemoteData error b) -> RemoteData error a -> RemoteData error b
andThen f r =
    case r of
        NotAsked ->
            NotAsked

        Loading ->
            Loading

        Failure error ->
            Failure error

        Success value ->
            f value


toMaybe : RemoteData error a -> Maybe a
toMaybe r =
    case r of
        Success a ->
            Just a

        _ ->
            Nothing


withDefault : a -> RemoteData error a -> a
withDefault a =
    toMaybe >> Maybe.withDefault a
