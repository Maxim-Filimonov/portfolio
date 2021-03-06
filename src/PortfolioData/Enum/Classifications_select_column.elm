-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module PortfolioData.Enum.Classifications_select_column exposing (Classifications_select_column(..), decoder, fromString, list, toString)

import Json.Decode as Decode exposing (Decoder)


{-| select columns of table "classifications"

  - Id - column name
  - Label - column name
  - Name - column name

-}
type Classifications_select_column
    = Id
    | Label
    | Name


list : List Classifications_select_column
list =
    [ Id, Label, Name ]


decoder : Decoder Classifications_select_column
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "id" ->
                        Decode.succeed Id

                    "label" ->
                        Decode.succeed Label

                    "name" ->
                        Decode.succeed Name

                    _ ->
                        Decode.fail ("Invalid Classifications_select_column type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representating the Enum to a string that the GraphQL server will recognize.
-}
toString : Classifications_select_column -> String
toString enum =
    case enum of
        Id ->
            "id"

        Label ->
            "label"

        Name ->
            "name"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe Classifications_select_column
fromString enumString =
    case enumString of
        "id" ->
            Just Id

        "label" ->
            Just Label

        "name" ->
            Just Name

        _ ->
            Nothing
