-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module PortfolioData.Query exposing (ClassificationsByPkRequiredArguments, ClassificationsOptionalArguments, ExperienceClassificationsByPkRequiredArguments, ExperienceClassificationsOptionalArguments, ExperiencesByClassificationOptionalArguments, ExperiencesByClassificationRequiredArguments, ExperiencesByPkRequiredArguments, ExperiencesOptionalArguments, classifications, classifications_by_pk, experience_classifications, experience_classifications_by_pk, experiences, experiences_by_classification, experiences_by_pk)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode exposing (Decoder)
import PortfolioData.Enum.Classifications_select_column
import PortfolioData.Enum.Experience_classifications_select_column
import PortfolioData.Enum.Experiences_select_column
import PortfolioData.InputObject
import PortfolioData.Interface
import PortfolioData.Object
import PortfolioData.Scalar
import PortfolioData.ScalarCodecs
import PortfolioData.Union


type alias ClassificationsOptionalArguments =
    { distinct_on : OptionalArgument (List PortfolioData.Enum.Classifications_select_column.Classifications_select_column)
    , limit : OptionalArgument Int
    , offset : OptionalArgument Int
    , order_by : OptionalArgument (List PortfolioData.InputObject.Classifications_order_by)
    , where_ : OptionalArgument PortfolioData.InputObject.Classifications_bool_exp
    }


{-| fetch data from the table: "classifications"

  - distinct\_on - distinct select on columns
  - limit - limit the nuber of rows returned
  - offset - skip the first n rows. Use only with order\_by
  - order\_by - sort the rows by one or more columns
  - where\_ - filter the rows returned

-}
classifications : (ClassificationsOptionalArguments -> ClassificationsOptionalArguments) -> SelectionSet decodesTo PortfolioData.Object.Classifications -> SelectionSet (List decodesTo) RootQuery
classifications fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { distinct_on = Absent, limit = Absent, offset = Absent, order_by = Absent, where_ = Absent }

        optionalArgs =
            [ Argument.optional "distinct_on" filledInOptionals.distinct_on (Encode.enum PortfolioData.Enum.Classifications_select_column.toString |> Encode.list), Argument.optional "limit" filledInOptionals.limit Encode.int, Argument.optional "offset" filledInOptionals.offset Encode.int, Argument.optional "order_by" filledInOptionals.order_by (PortfolioData.InputObject.encodeClassifications_order_by |> Encode.list), Argument.optional "where" filledInOptionals.where_ PortfolioData.InputObject.encodeClassifications_bool_exp ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "classifications" optionalArgs object_ (identity >> Decode.list)


type alias ClassificationsByPkRequiredArguments =
    { id : Int }


{-| fetch data from the table: "classifications" using primary key columns
-}
classifications_by_pk : ClassificationsByPkRequiredArguments -> SelectionSet decodesTo PortfolioData.Object.Classifications -> SelectionSet (Maybe decodesTo) RootQuery
classifications_by_pk requiredArgs object_ =
    Object.selectionForCompositeField "classifications_by_pk" [ Argument.required "id" requiredArgs.id Encode.int ] object_ (identity >> Decode.nullable)


type alias ExperienceClassificationsOptionalArguments =
    { distinct_on : OptionalArgument (List PortfolioData.Enum.Experience_classifications_select_column.Experience_classifications_select_column)
    , limit : OptionalArgument Int
    , offset : OptionalArgument Int
    , order_by : OptionalArgument (List PortfolioData.InputObject.Experience_classifications_order_by)
    , where_ : OptionalArgument PortfolioData.InputObject.Experience_classifications_bool_exp
    }


{-| fetch data from the table: "experience\_classifications"

  - distinct\_on - distinct select on columns
  - limit - limit the nuber of rows returned
  - offset - skip the first n rows. Use only with order\_by
  - order\_by - sort the rows by one or more columns
  - where\_ - filter the rows returned

-}
experience_classifications : (ExperienceClassificationsOptionalArguments -> ExperienceClassificationsOptionalArguments) -> SelectionSet decodesTo PortfolioData.Object.Experience_classifications -> SelectionSet (List decodesTo) RootQuery
experience_classifications fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { distinct_on = Absent, limit = Absent, offset = Absent, order_by = Absent, where_ = Absent }

        optionalArgs =
            [ Argument.optional "distinct_on" filledInOptionals.distinct_on (Encode.enum PortfolioData.Enum.Experience_classifications_select_column.toString |> Encode.list), Argument.optional "limit" filledInOptionals.limit Encode.int, Argument.optional "offset" filledInOptionals.offset Encode.int, Argument.optional "order_by" filledInOptionals.order_by (PortfolioData.InputObject.encodeExperience_classifications_order_by |> Encode.list), Argument.optional "where" filledInOptionals.where_ PortfolioData.InputObject.encodeExperience_classifications_bool_exp ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "experience_classifications" optionalArgs object_ (identity >> Decode.list)


type alias ExperienceClassificationsByPkRequiredArguments =
    { experience_id : Int
    , tag_id : Int
    }


{-| fetch data from the table: "experience\_classifications" using primary key columns
-}
experience_classifications_by_pk : ExperienceClassificationsByPkRequiredArguments -> SelectionSet decodesTo PortfolioData.Object.Experience_classifications -> SelectionSet (Maybe decodesTo) RootQuery
experience_classifications_by_pk requiredArgs object_ =
    Object.selectionForCompositeField "experience_classifications_by_pk" [ Argument.required "experience_id" requiredArgs.experience_id Encode.int, Argument.required "tag_id" requiredArgs.tag_id Encode.int ] object_ (identity >> Decode.nullable)


type alias ExperiencesOptionalArguments =
    { distinct_on : OptionalArgument (List PortfolioData.Enum.Experiences_select_column.Experiences_select_column)
    , limit : OptionalArgument Int
    , offset : OptionalArgument Int
    , order_by : OptionalArgument (List PortfolioData.InputObject.Experiences_order_by)
    , where_ : OptionalArgument PortfolioData.InputObject.Experiences_bool_exp
    }


{-| fetch data from the table: "experiences"

  - distinct\_on - distinct select on columns
  - limit - limit the nuber of rows returned
  - offset - skip the first n rows. Use only with order\_by
  - order\_by - sort the rows by one or more columns
  - where\_ - filter the rows returned

-}
experiences : (ExperiencesOptionalArguments -> ExperiencesOptionalArguments) -> SelectionSet decodesTo PortfolioData.Object.Experiences -> SelectionSet (List decodesTo) RootQuery
experiences fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { distinct_on = Absent, limit = Absent, offset = Absent, order_by = Absent, where_ = Absent }

        optionalArgs =
            [ Argument.optional "distinct_on" filledInOptionals.distinct_on (Encode.enum PortfolioData.Enum.Experiences_select_column.toString |> Encode.list), Argument.optional "limit" filledInOptionals.limit Encode.int, Argument.optional "offset" filledInOptionals.offset Encode.int, Argument.optional "order_by" filledInOptionals.order_by (PortfolioData.InputObject.encodeExperiences_order_by |> Encode.list), Argument.optional "where" filledInOptionals.where_ PortfolioData.InputObject.encodeExperiences_bool_exp ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "experiences" optionalArgs object_ (identity >> Decode.list)


type alias ExperiencesByClassificationOptionalArguments =
    { distinct_on : OptionalArgument (List PortfolioData.Enum.Experiences_select_column.Experiences_select_column)
    , limit : OptionalArgument Int
    , offset : OptionalArgument Int
    , order_by : OptionalArgument (List PortfolioData.InputObject.Experiences_order_by)
    , where_ : OptionalArgument PortfolioData.InputObject.Experiences_bool_exp
    }


type alias ExperiencesByClassificationRequiredArguments =
    { args : PortfolioData.InputObject.Experiences_by_classification_args }


{-| execute function "experiences\_by\_classification" which returns "experiences"

  - args - input parameters for function "experiences\_by\_classification"
  - distinct\_on - distinct select on columns
  - limit - limit the nuber of rows returned
  - offset - skip the first n rows. Use only with order\_by
  - order\_by - sort the rows by one or more columns
  - where\_ - filter the rows returned

-}
experiences_by_classification : (ExperiencesByClassificationOptionalArguments -> ExperiencesByClassificationOptionalArguments) -> ExperiencesByClassificationRequiredArguments -> SelectionSet decodesTo PortfolioData.Object.Experiences -> SelectionSet (List decodesTo) RootQuery
experiences_by_classification fillInOptionals requiredArgs object_ =
    let
        filledInOptionals =
            fillInOptionals { distinct_on = Absent, limit = Absent, offset = Absent, order_by = Absent, where_ = Absent }

        optionalArgs =
            [ Argument.optional "distinct_on" filledInOptionals.distinct_on (Encode.enum PortfolioData.Enum.Experiences_select_column.toString |> Encode.list), Argument.optional "limit" filledInOptionals.limit Encode.int, Argument.optional "offset" filledInOptionals.offset Encode.int, Argument.optional "order_by" filledInOptionals.order_by (PortfolioData.InputObject.encodeExperiences_order_by |> Encode.list), Argument.optional "where" filledInOptionals.where_ PortfolioData.InputObject.encodeExperiences_bool_exp ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "experiences_by_classification" (optionalArgs ++ [ Argument.required "args" requiredArgs.args PortfolioData.InputObject.encodeExperiences_by_classification_args ]) object_ (identity >> Decode.list)


type alias ExperiencesByPkRequiredArguments =
    { id : Int }


{-| fetch data from the table: "experiences" using primary key columns
-}
experiences_by_pk : ExperiencesByPkRequiredArguments -> SelectionSet decodesTo PortfolioData.Object.Experiences -> SelectionSet (Maybe decodesTo) RootQuery
experiences_by_pk requiredArgs object_ =
    Object.selectionForCompositeField "experiences_by_pk" [ Argument.required "id" requiredArgs.id Encode.int ] object_ (identity >> Decode.nullable)
