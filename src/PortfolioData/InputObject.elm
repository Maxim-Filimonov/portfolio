-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module PortfolioData.InputObject exposing (Classifications_bool_exp(..), Classifications_bool_expOptionalFields, Classifications_bool_expRaw, Classifications_order_by, Classifications_order_byOptionalFields, Date_comparison_exp, Date_comparison_expOptionalFields, Experience_classifications_bool_exp(..), Experience_classifications_bool_expOptionalFields, Experience_classifications_bool_expRaw, Experience_classifications_order_by, Experience_classifications_order_byOptionalFields, Experiences_bool_exp(..), Experiences_bool_expOptionalFields, Experiences_bool_expRaw, Experiences_by_classification_args, Experiences_by_classification_argsOptionalFields, Experiences_order_by, Experiences_order_byOptionalFields, Int_comparison_exp, Int_comparison_expOptionalFields, String_comparison_exp, String_comparison_expOptionalFields, buildClassifications_bool_exp, buildClassifications_order_by, buildDate_comparison_exp, buildExperience_classifications_bool_exp, buildExperience_classifications_order_by, buildExperiences_bool_exp, buildExperiences_by_classification_args, buildExperiences_order_by, buildInt_comparison_exp, buildString_comparison_exp, encodeClassifications_bool_exp, encodeClassifications_order_by, encodeDate_comparison_exp, encodeExperience_classifications_bool_exp, encodeExperience_classifications_order_by, encodeExperiences_bool_exp, encodeExperiences_by_classification_args, encodeExperiences_order_by, encodeInt_comparison_exp, encodeString_comparison_exp)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode
import PortfolioData.Enum.Order_by
import PortfolioData.Interface
import PortfolioData.Object
import PortfolioData.Scalar
import PortfolioData.ScalarCodecs
import PortfolioData.Union


buildClassifications_bool_exp : (Classifications_bool_expOptionalFields -> Classifications_bool_expOptionalFields) -> Classifications_bool_exp
buildClassifications_bool_exp fillOptionals =
    let
        optionals =
            fillOptionals
                { and_ = Absent, not_ = Absent, or_ = Absent, experience_classifications = Absent, id = Absent, label = Absent, name = Absent }
    in
    Classifications_bool_exp { and_ = optionals.and_, not_ = optionals.not_, or_ = optionals.or_, experience_classifications = optionals.experience_classifications, id = optionals.id, label = optionals.label, name = optionals.name }


type alias Classifications_bool_expOptionalFields =
    { and_ : OptionalArgument (List (Maybe Classifications_bool_exp))
    , not_ : OptionalArgument Classifications_bool_exp
    , or_ : OptionalArgument (List (Maybe Classifications_bool_exp))
    , experience_classifications : OptionalArgument Experience_classifications_bool_exp
    , id : OptionalArgument Int_comparison_exp
    , label : OptionalArgument String_comparison_exp
    , name : OptionalArgument String_comparison_exp
    }


{-| Type alias for the `Classifications_bool_exp` attributes. Note that this type
needs to use the `Classifications_bool_exp` type (not just a plain type alias) because it has
references to itself either directly (recursive) or indirectly (circular). See
<https://github.com/dillonkearns/elm-graphql/issues/33>.
-}
type alias Classifications_bool_expRaw =
    { and_ : OptionalArgument (List (Maybe Classifications_bool_exp))
    , not_ : OptionalArgument Classifications_bool_exp
    , or_ : OptionalArgument (List (Maybe Classifications_bool_exp))
    , experience_classifications : OptionalArgument Experience_classifications_bool_exp
    , id : OptionalArgument Int_comparison_exp
    , label : OptionalArgument String_comparison_exp
    , name : OptionalArgument String_comparison_exp
    }


{-| Type for the Classifications\_bool\_exp input object.
-}
type Classifications_bool_exp
    = Classifications_bool_exp Classifications_bool_expRaw


{-| Encode a Classifications\_bool\_exp into a value that can be used as an argument.
-}
encodeClassifications_bool_exp : Classifications_bool_exp -> Value
encodeClassifications_bool_exp (Classifications_bool_exp input) =
    Encode.maybeObject
        [ ( "_and", (encodeClassifications_bool_exp |> Encode.maybe |> Encode.list) |> Encode.optional input.and_ ), ( "_not", encodeClassifications_bool_exp |> Encode.optional input.not_ ), ( "_or", (encodeClassifications_bool_exp |> Encode.maybe |> Encode.list) |> Encode.optional input.or_ ), ( "experience_classifications", encodeExperience_classifications_bool_exp |> Encode.optional input.experience_classifications ), ( "id", encodeInt_comparison_exp |> Encode.optional input.id ), ( "label", encodeString_comparison_exp |> Encode.optional input.label ), ( "name", encodeString_comparison_exp |> Encode.optional input.name ) ]


buildClassifications_order_by : (Classifications_order_byOptionalFields -> Classifications_order_byOptionalFields) -> Classifications_order_by
buildClassifications_order_by fillOptionals =
    let
        optionals =
            fillOptionals
                { id = Absent, label = Absent, name = Absent }
    in
    { id = optionals.id, label = optionals.label, name = optionals.name }


type alias Classifications_order_byOptionalFields =
    { id : OptionalArgument PortfolioData.Enum.Order_by.Order_by
    , label : OptionalArgument PortfolioData.Enum.Order_by.Order_by
    , name : OptionalArgument PortfolioData.Enum.Order_by.Order_by
    }


{-| Type for the Classifications\_order\_by input object.
-}
type alias Classifications_order_by =
    { id : OptionalArgument PortfolioData.Enum.Order_by.Order_by
    , label : OptionalArgument PortfolioData.Enum.Order_by.Order_by
    , name : OptionalArgument PortfolioData.Enum.Order_by.Order_by
    }


{-| Encode a Classifications\_order\_by into a value that can be used as an argument.
-}
encodeClassifications_order_by : Classifications_order_by -> Value
encodeClassifications_order_by input =
    Encode.maybeObject
        [ ( "id", Encode.enum PortfolioData.Enum.Order_by.toString |> Encode.optional input.id ), ( "label", Encode.enum PortfolioData.Enum.Order_by.toString |> Encode.optional input.label ), ( "name", Encode.enum PortfolioData.Enum.Order_by.toString |> Encode.optional input.name ) ]


buildDate_comparison_exp : (Date_comparison_expOptionalFields -> Date_comparison_expOptionalFields) -> Date_comparison_exp
buildDate_comparison_exp fillOptionals =
    let
        optionals =
            fillOptionals
                { eq_ = Absent, gt_ = Absent, gte_ = Absent, in_ = Absent, is_null_ = Absent, lt_ = Absent, lte_ = Absent, neq_ = Absent, nin_ = Absent }
    in
    { eq_ = optionals.eq_, gt_ = optionals.gt_, gte_ = optionals.gte_, in_ = optionals.in_, is_null_ = optionals.is_null_, lt_ = optionals.lt_, lte_ = optionals.lte_, neq_ = optionals.neq_, nin_ = optionals.nin_ }


type alias Date_comparison_expOptionalFields =
    { eq_ : OptionalArgument PortfolioData.ScalarCodecs.Date
    , gt_ : OptionalArgument PortfolioData.ScalarCodecs.Date
    , gte_ : OptionalArgument PortfolioData.ScalarCodecs.Date
    , in_ : OptionalArgument (List PortfolioData.ScalarCodecs.Date)
    , is_null_ : OptionalArgument Bool
    , lt_ : OptionalArgument PortfolioData.ScalarCodecs.Date
    , lte_ : OptionalArgument PortfolioData.ScalarCodecs.Date
    , neq_ : OptionalArgument PortfolioData.ScalarCodecs.Date
    , nin_ : OptionalArgument (List PortfolioData.ScalarCodecs.Date)
    }


{-| Type for the Date\_comparison\_exp input object.
-}
type alias Date_comparison_exp =
    { eq_ : OptionalArgument PortfolioData.ScalarCodecs.Date
    , gt_ : OptionalArgument PortfolioData.ScalarCodecs.Date
    , gte_ : OptionalArgument PortfolioData.ScalarCodecs.Date
    , in_ : OptionalArgument (List PortfolioData.ScalarCodecs.Date)
    , is_null_ : OptionalArgument Bool
    , lt_ : OptionalArgument PortfolioData.ScalarCodecs.Date
    , lte_ : OptionalArgument PortfolioData.ScalarCodecs.Date
    , neq_ : OptionalArgument PortfolioData.ScalarCodecs.Date
    , nin_ : OptionalArgument (List PortfolioData.ScalarCodecs.Date)
    }


{-| Encode a Date\_comparison\_exp into a value that can be used as an argument.
-}
encodeDate_comparison_exp : Date_comparison_exp -> Value
encodeDate_comparison_exp input =
    Encode.maybeObject
        [ ( "_eq", (PortfolioData.ScalarCodecs.codecs |> PortfolioData.Scalar.unwrapEncoder .codecDate) |> Encode.optional input.eq_ ), ( "_gt", (PortfolioData.ScalarCodecs.codecs |> PortfolioData.Scalar.unwrapEncoder .codecDate) |> Encode.optional input.gt_ ), ( "_gte", (PortfolioData.ScalarCodecs.codecs |> PortfolioData.Scalar.unwrapEncoder .codecDate) |> Encode.optional input.gte_ ), ( "_in", ((PortfolioData.ScalarCodecs.codecs |> PortfolioData.Scalar.unwrapEncoder .codecDate) |> Encode.list) |> Encode.optional input.in_ ), ( "_is_null", Encode.bool |> Encode.optional input.is_null_ ), ( "_lt", (PortfolioData.ScalarCodecs.codecs |> PortfolioData.Scalar.unwrapEncoder .codecDate) |> Encode.optional input.lt_ ), ( "_lte", (PortfolioData.ScalarCodecs.codecs |> PortfolioData.Scalar.unwrapEncoder .codecDate) |> Encode.optional input.lte_ ), ( "_neq", (PortfolioData.ScalarCodecs.codecs |> PortfolioData.Scalar.unwrapEncoder .codecDate) |> Encode.optional input.neq_ ), ( "_nin", ((PortfolioData.ScalarCodecs.codecs |> PortfolioData.Scalar.unwrapEncoder .codecDate) |> Encode.list) |> Encode.optional input.nin_ ) ]


buildExperience_classifications_bool_exp : (Experience_classifications_bool_expOptionalFields -> Experience_classifications_bool_expOptionalFields) -> Experience_classifications_bool_exp
buildExperience_classifications_bool_exp fillOptionals =
    let
        optionals =
            fillOptionals
                { and_ = Absent, not_ = Absent, or_ = Absent, classification = Absent, experience = Absent, experience_id = Absent, tag_id = Absent }
    in
    Experience_classifications_bool_exp { and_ = optionals.and_, not_ = optionals.not_, or_ = optionals.or_, classification = optionals.classification, experience = optionals.experience, experience_id = optionals.experience_id, tag_id = optionals.tag_id }


type alias Experience_classifications_bool_expOptionalFields =
    { and_ : OptionalArgument (List (Maybe Experience_classifications_bool_exp))
    , not_ : OptionalArgument Experience_classifications_bool_exp
    , or_ : OptionalArgument (List (Maybe Experience_classifications_bool_exp))
    , classification : OptionalArgument Classifications_bool_exp
    , experience : OptionalArgument Experiences_bool_exp
    , experience_id : OptionalArgument Int_comparison_exp
    , tag_id : OptionalArgument Int_comparison_exp
    }


{-| Type alias for the `Experience_classifications_bool_exp` attributes. Note that this type
needs to use the `Experience_classifications_bool_exp` type (not just a plain type alias) because it has
references to itself either directly (recursive) or indirectly (circular). See
<https://github.com/dillonkearns/elm-graphql/issues/33>.
-}
type alias Experience_classifications_bool_expRaw =
    { and_ : OptionalArgument (List (Maybe Experience_classifications_bool_exp))
    , not_ : OptionalArgument Experience_classifications_bool_exp
    , or_ : OptionalArgument (List (Maybe Experience_classifications_bool_exp))
    , classification : OptionalArgument Classifications_bool_exp
    , experience : OptionalArgument Experiences_bool_exp
    , experience_id : OptionalArgument Int_comparison_exp
    , tag_id : OptionalArgument Int_comparison_exp
    }


{-| Type for the Experience\_classifications\_bool\_exp input object.
-}
type Experience_classifications_bool_exp
    = Experience_classifications_bool_exp Experience_classifications_bool_expRaw


{-| Encode a Experience\_classifications\_bool\_exp into a value that can be used as an argument.
-}
encodeExperience_classifications_bool_exp : Experience_classifications_bool_exp -> Value
encodeExperience_classifications_bool_exp (Experience_classifications_bool_exp input) =
    Encode.maybeObject
        [ ( "_and", (encodeExperience_classifications_bool_exp |> Encode.maybe |> Encode.list) |> Encode.optional input.and_ ), ( "_not", encodeExperience_classifications_bool_exp |> Encode.optional input.not_ ), ( "_or", (encodeExperience_classifications_bool_exp |> Encode.maybe |> Encode.list) |> Encode.optional input.or_ ), ( "classification", encodeClassifications_bool_exp |> Encode.optional input.classification ), ( "experience", encodeExperiences_bool_exp |> Encode.optional input.experience ), ( "experience_id", encodeInt_comparison_exp |> Encode.optional input.experience_id ), ( "tag_id", encodeInt_comparison_exp |> Encode.optional input.tag_id ) ]


buildExperience_classifications_order_by : (Experience_classifications_order_byOptionalFields -> Experience_classifications_order_byOptionalFields) -> Experience_classifications_order_by
buildExperience_classifications_order_by fillOptionals =
    let
        optionals =
            fillOptionals
                { classification = Absent, experience = Absent, experience_id = Absent, tag_id = Absent }
    in
    { classification = optionals.classification, experience = optionals.experience, experience_id = optionals.experience_id, tag_id = optionals.tag_id }


type alias Experience_classifications_order_byOptionalFields =
    { classification : OptionalArgument Classifications_order_by
    , experience : OptionalArgument Experiences_order_by
    , experience_id : OptionalArgument PortfolioData.Enum.Order_by.Order_by
    , tag_id : OptionalArgument PortfolioData.Enum.Order_by.Order_by
    }


{-| Type for the Experience\_classifications\_order\_by input object.
-}
type alias Experience_classifications_order_by =
    { classification : OptionalArgument Classifications_order_by
    , experience : OptionalArgument Experiences_order_by
    , experience_id : OptionalArgument PortfolioData.Enum.Order_by.Order_by
    , tag_id : OptionalArgument PortfolioData.Enum.Order_by.Order_by
    }


{-| Encode a Experience\_classifications\_order\_by into a value that can be used as an argument.
-}
encodeExperience_classifications_order_by : Experience_classifications_order_by -> Value
encodeExperience_classifications_order_by input =
    Encode.maybeObject
        [ ( "classification", encodeClassifications_order_by |> Encode.optional input.classification ), ( "experience", encodeExperiences_order_by |> Encode.optional input.experience ), ( "experience_id", Encode.enum PortfolioData.Enum.Order_by.toString |> Encode.optional input.experience_id ), ( "tag_id", Encode.enum PortfolioData.Enum.Order_by.toString |> Encode.optional input.tag_id ) ]


buildExperiences_bool_exp : (Experiences_bool_expOptionalFields -> Experiences_bool_expOptionalFields) -> Experiences_bool_exp
buildExperiences_bool_exp fillOptionals =
    let
        optionals =
            fillOptionals
                { and_ = Absent, not_ = Absent, or_ = Absent, company_name = Absent, end_date = Absent, experience_classifications = Absent, id = Absent, role = Absent, start_date = Absent }
    in
    Experiences_bool_exp { and_ = optionals.and_, not_ = optionals.not_, or_ = optionals.or_, company_name = optionals.company_name, end_date = optionals.end_date, experience_classifications = optionals.experience_classifications, id = optionals.id, role = optionals.role, start_date = optionals.start_date }


type alias Experiences_bool_expOptionalFields =
    { and_ : OptionalArgument (List (Maybe Experiences_bool_exp))
    , not_ : OptionalArgument Experiences_bool_exp
    , or_ : OptionalArgument (List (Maybe Experiences_bool_exp))
    , company_name : OptionalArgument String_comparison_exp
    , end_date : OptionalArgument Date_comparison_exp
    , experience_classifications : OptionalArgument Experience_classifications_bool_exp
    , id : OptionalArgument Int_comparison_exp
    , role : OptionalArgument String_comparison_exp
    , start_date : OptionalArgument Date_comparison_exp
    }


{-| Type alias for the `Experiences_bool_exp` attributes. Note that this type
needs to use the `Experiences_bool_exp` type (not just a plain type alias) because it has
references to itself either directly (recursive) or indirectly (circular). See
<https://github.com/dillonkearns/elm-graphql/issues/33>.
-}
type alias Experiences_bool_expRaw =
    { and_ : OptionalArgument (List (Maybe Experiences_bool_exp))
    , not_ : OptionalArgument Experiences_bool_exp
    , or_ : OptionalArgument (List (Maybe Experiences_bool_exp))
    , company_name : OptionalArgument String_comparison_exp
    , end_date : OptionalArgument Date_comparison_exp
    , experience_classifications : OptionalArgument Experience_classifications_bool_exp
    , id : OptionalArgument Int_comparison_exp
    , role : OptionalArgument String_comparison_exp
    , start_date : OptionalArgument Date_comparison_exp
    }


{-| Type for the Experiences\_bool\_exp input object.
-}
type Experiences_bool_exp
    = Experiences_bool_exp Experiences_bool_expRaw


{-| Encode a Experiences\_bool\_exp into a value that can be used as an argument.
-}
encodeExperiences_bool_exp : Experiences_bool_exp -> Value
encodeExperiences_bool_exp (Experiences_bool_exp input) =
    Encode.maybeObject
        [ ( "_and", (encodeExperiences_bool_exp |> Encode.maybe |> Encode.list) |> Encode.optional input.and_ ), ( "_not", encodeExperiences_bool_exp |> Encode.optional input.not_ ), ( "_or", (encodeExperiences_bool_exp |> Encode.maybe |> Encode.list) |> Encode.optional input.or_ ), ( "company_name", encodeString_comparison_exp |> Encode.optional input.company_name ), ( "end_date", encodeDate_comparison_exp |> Encode.optional input.end_date ), ( "experience_classifications", encodeExperience_classifications_bool_exp |> Encode.optional input.experience_classifications ), ( "id", encodeInt_comparison_exp |> Encode.optional input.id ), ( "role", encodeString_comparison_exp |> Encode.optional input.role ), ( "start_date", encodeDate_comparison_exp |> Encode.optional input.start_date ) ]


buildExperiences_by_classification_args : (Experiences_by_classification_argsOptionalFields -> Experiences_by_classification_argsOptionalFields) -> Experiences_by_classification_args
buildExperiences_by_classification_args fillOptionals =
    let
        optionals =
            fillOptionals
                { search = Absent }
    in
    { search = optionals.search }


type alias Experiences_by_classification_argsOptionalFields =
    { search : OptionalArgument String }


{-| Type for the Experiences\_by\_classification\_args input object.
-}
type alias Experiences_by_classification_args =
    { search : OptionalArgument String }


{-| Encode a Experiences\_by\_classification\_args into a value that can be used as an argument.
-}
encodeExperiences_by_classification_args : Experiences_by_classification_args -> Value
encodeExperiences_by_classification_args input =
    Encode.maybeObject
        [ ( "search", Encode.string |> Encode.optional input.search ) ]


buildExperiences_order_by : (Experiences_order_byOptionalFields -> Experiences_order_byOptionalFields) -> Experiences_order_by
buildExperiences_order_by fillOptionals =
    let
        optionals =
            fillOptionals
                { company_name = Absent, end_date = Absent, id = Absent, role = Absent, start_date = Absent }
    in
    { company_name = optionals.company_name, end_date = optionals.end_date, id = optionals.id, role = optionals.role, start_date = optionals.start_date }


type alias Experiences_order_byOptionalFields =
    { company_name : OptionalArgument PortfolioData.Enum.Order_by.Order_by
    , end_date : OptionalArgument PortfolioData.Enum.Order_by.Order_by
    , id : OptionalArgument PortfolioData.Enum.Order_by.Order_by
    , role : OptionalArgument PortfolioData.Enum.Order_by.Order_by
    , start_date : OptionalArgument PortfolioData.Enum.Order_by.Order_by
    }


{-| Type for the Experiences\_order\_by input object.
-}
type alias Experiences_order_by =
    { company_name : OptionalArgument PortfolioData.Enum.Order_by.Order_by
    , end_date : OptionalArgument PortfolioData.Enum.Order_by.Order_by
    , id : OptionalArgument PortfolioData.Enum.Order_by.Order_by
    , role : OptionalArgument PortfolioData.Enum.Order_by.Order_by
    , start_date : OptionalArgument PortfolioData.Enum.Order_by.Order_by
    }


{-| Encode a Experiences\_order\_by into a value that can be used as an argument.
-}
encodeExperiences_order_by : Experiences_order_by -> Value
encodeExperiences_order_by input =
    Encode.maybeObject
        [ ( "company_name", Encode.enum PortfolioData.Enum.Order_by.toString |> Encode.optional input.company_name ), ( "end_date", Encode.enum PortfolioData.Enum.Order_by.toString |> Encode.optional input.end_date ), ( "id", Encode.enum PortfolioData.Enum.Order_by.toString |> Encode.optional input.id ), ( "role", Encode.enum PortfolioData.Enum.Order_by.toString |> Encode.optional input.role ), ( "start_date", Encode.enum PortfolioData.Enum.Order_by.toString |> Encode.optional input.start_date ) ]


buildInt_comparison_exp : (Int_comparison_expOptionalFields -> Int_comparison_expOptionalFields) -> Int_comparison_exp
buildInt_comparison_exp fillOptionals =
    let
        optionals =
            fillOptionals
                { eq_ = Absent, gt_ = Absent, gte_ = Absent, in_ = Absent, is_null_ = Absent, lt_ = Absent, lte_ = Absent, neq_ = Absent, nin_ = Absent }
    in
    { eq_ = optionals.eq_, gt_ = optionals.gt_, gte_ = optionals.gte_, in_ = optionals.in_, is_null_ = optionals.is_null_, lt_ = optionals.lt_, lte_ = optionals.lte_, neq_ = optionals.neq_, nin_ = optionals.nin_ }


type alias Int_comparison_expOptionalFields =
    { eq_ : OptionalArgument Int
    , gt_ : OptionalArgument Int
    , gte_ : OptionalArgument Int
    , in_ : OptionalArgument (List Int)
    , is_null_ : OptionalArgument Bool
    , lt_ : OptionalArgument Int
    , lte_ : OptionalArgument Int
    , neq_ : OptionalArgument Int
    , nin_ : OptionalArgument (List Int)
    }


{-| Type for the Int\_comparison\_exp input object.
-}
type alias Int_comparison_exp =
    { eq_ : OptionalArgument Int
    , gt_ : OptionalArgument Int
    , gte_ : OptionalArgument Int
    , in_ : OptionalArgument (List Int)
    , is_null_ : OptionalArgument Bool
    , lt_ : OptionalArgument Int
    , lte_ : OptionalArgument Int
    , neq_ : OptionalArgument Int
    , nin_ : OptionalArgument (List Int)
    }


{-| Encode a Int\_comparison\_exp into a value that can be used as an argument.
-}
encodeInt_comparison_exp : Int_comparison_exp -> Value
encodeInt_comparison_exp input =
    Encode.maybeObject
        [ ( "_eq", Encode.int |> Encode.optional input.eq_ ), ( "_gt", Encode.int |> Encode.optional input.gt_ ), ( "_gte", Encode.int |> Encode.optional input.gte_ ), ( "_in", (Encode.int |> Encode.list) |> Encode.optional input.in_ ), ( "_is_null", Encode.bool |> Encode.optional input.is_null_ ), ( "_lt", Encode.int |> Encode.optional input.lt_ ), ( "_lte", Encode.int |> Encode.optional input.lte_ ), ( "_neq", Encode.int |> Encode.optional input.neq_ ), ( "_nin", (Encode.int |> Encode.list) |> Encode.optional input.nin_ ) ]


buildString_comparison_exp : (String_comparison_expOptionalFields -> String_comparison_expOptionalFields) -> String_comparison_exp
buildString_comparison_exp fillOptionals =
    let
        optionals =
            fillOptionals
                { eq_ = Absent, gt_ = Absent, gte_ = Absent, ilike_ = Absent, in_ = Absent, is_null_ = Absent, like_ = Absent, lt_ = Absent, lte_ = Absent, neq_ = Absent, nilike_ = Absent, nin_ = Absent, nlike_ = Absent, nsimilar_ = Absent, similar_ = Absent }
    in
    { eq_ = optionals.eq_, gt_ = optionals.gt_, gte_ = optionals.gte_, ilike_ = optionals.ilike_, in_ = optionals.in_, is_null_ = optionals.is_null_, like_ = optionals.like_, lt_ = optionals.lt_, lte_ = optionals.lte_, neq_ = optionals.neq_, nilike_ = optionals.nilike_, nin_ = optionals.nin_, nlike_ = optionals.nlike_, nsimilar_ = optionals.nsimilar_, similar_ = optionals.similar_ }


type alias String_comparison_expOptionalFields =
    { eq_ : OptionalArgument String
    , gt_ : OptionalArgument String
    , gte_ : OptionalArgument String
    , ilike_ : OptionalArgument String
    , in_ : OptionalArgument (List String)
    , is_null_ : OptionalArgument Bool
    , like_ : OptionalArgument String
    , lt_ : OptionalArgument String
    , lte_ : OptionalArgument String
    , neq_ : OptionalArgument String
    , nilike_ : OptionalArgument String
    , nin_ : OptionalArgument (List String)
    , nlike_ : OptionalArgument String
    , nsimilar_ : OptionalArgument String
    , similar_ : OptionalArgument String
    }


{-| Type for the String\_comparison\_exp input object.
-}
type alias String_comparison_exp =
    { eq_ : OptionalArgument String
    , gt_ : OptionalArgument String
    , gte_ : OptionalArgument String
    , ilike_ : OptionalArgument String
    , in_ : OptionalArgument (List String)
    , is_null_ : OptionalArgument Bool
    , like_ : OptionalArgument String
    , lt_ : OptionalArgument String
    , lte_ : OptionalArgument String
    , neq_ : OptionalArgument String
    , nilike_ : OptionalArgument String
    , nin_ : OptionalArgument (List String)
    , nlike_ : OptionalArgument String
    , nsimilar_ : OptionalArgument String
    , similar_ : OptionalArgument String
    }


{-| Encode a String\_comparison\_exp into a value that can be used as an argument.
-}
encodeString_comparison_exp : String_comparison_exp -> Value
encodeString_comparison_exp input =
    Encode.maybeObject
        [ ( "_eq", Encode.string |> Encode.optional input.eq_ ), ( "_gt", Encode.string |> Encode.optional input.gt_ ), ( "_gte", Encode.string |> Encode.optional input.gte_ ), ( "_ilike", Encode.string |> Encode.optional input.ilike_ ), ( "_in", (Encode.string |> Encode.list) |> Encode.optional input.in_ ), ( "_is_null", Encode.bool |> Encode.optional input.is_null_ ), ( "_like", Encode.string |> Encode.optional input.like_ ), ( "_lt", Encode.string |> Encode.optional input.lt_ ), ( "_lte", Encode.string |> Encode.optional input.lte_ ), ( "_neq", Encode.string |> Encode.optional input.neq_ ), ( "_nilike", Encode.string |> Encode.optional input.nilike_ ), ( "_nin", (Encode.string |> Encode.list) |> Encode.optional input.nin_ ), ( "_nlike", Encode.string |> Encode.optional input.nlike_ ), ( "_nsimilar", Encode.string |> Encode.optional input.nsimilar_ ), ( "_similar", Encode.string |> Encode.optional input.similar_ ) ]
