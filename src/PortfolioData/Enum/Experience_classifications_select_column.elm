-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module PortfolioData.Enum.Experience_classifications_select_column exposing (Experience_classifications_select_column(..), decoder, fromString, list, toString)

import Json.Decode as Decode exposing (Decoder)


{-| select columns of table "experience\_classifications"

  - Experience\_id - column name
  - Tag\_id - column name

-}
type Experience_classifications_select_column
    = Experience_id
    | Tag_id


list : List Experience_classifications_select_column
list =
    [ Experience_id, Tag_id ]


decoder : Decoder Experience_classifications_select_column
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "experience_id" ->
                        Decode.succeed Experience_id

                    "tag_id" ->
                        Decode.succeed Tag_id

                    _ ->
                        Decode.fail ("Invalid Experience_classifications_select_column type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representating the Enum to a string that the GraphQL server will recognize.
-}
toString : Experience_classifications_select_column -> String
toString enum =
    case enum of
        Experience_id ->
            "experience_id"

        Tag_id ->
            "tag_id"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe Experience_classifications_select_column
fromString enumString =
    case enumString of
        "experience_id" ->
            Just Experience_id

        "tag_id" ->
            Just Tag_id

        _ ->
            Nothing
