module Data exposing (Experience, Model, Msg, fetchExperiences, init, update)

-- import PortfolioData.Object.Experiences as Experiences

import GraphQLClient exposing (makeGraphQLQuery)
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import PortfolioData.InputObject exposing (buildClassifications_bool_exp, buildExperiences_bool_exp)
import PortfolioData.Object
import PortfolioData.Object.Classifications
import PortfolioData.Object.Experience_classifications
import PortfolioData.Object.Experiences
import PortfolioData.Query as Query
import RemoteData exposing (RemoteData)


type Msg
    = GotResponse ExperienceModel


type alias ExperienceModel =
    RemoteData (Graphql.Http.Error Response) Response


type alias Model =
    { experiences : Response
    }


type alias Response =
    List Experience


type alias Experience =
    { company_name : Maybe String
    , role : Maybe String
    , tags : List Classification
    }


type alias Classification =
    { name : String
    , label : Maybe String
    }


classificationsSelection : SelectionSet (List Classification) PortfolioData.Object.Experiences
classificationsSelection =
    PortfolioData.Object.Experiences.experience_classifications (\args -> args)
        classificationSelection


classificationSelection : SelectionSet Classification PortfolioData.Object.Experience_classifications
classificationSelection =
    PortfolioData.Object.Experience_classifications.classification <|
        SelectionSet.map2 Classification
            PortfolioData.Object.Classifications.name
            PortfolioData.Object.Classifications.label


experienceSelection : SelectionSet Experience PortfolioData.Object.Experiences
experienceSelection =
    SelectionSet.map3 Experience
        PortfolioData.Object.Experiences.company_name
        PortfolioData.Object.Experiences.role
        classificationsSelection


classificationFilter : String -> Query.ExperiencesByClassificationRequiredArguments
classificationFilter classification =
    { args = { search = Present classification } }


queryExperiences : SelectionSet Response RootQuery
queryExperiences =
    Query.experiences (\args -> args) experienceSelection


fetchExperiences : Cmd Msg
fetchExperiences =
    makeGraphQLQuery queryExperiences (RemoteData.fromResult >> GotResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotResponse (RemoteData.Success experiences) ->
            ( { model | experiences = experiences }, Cmd.none )

        GotResponse _ ->
            ( model, Cmd.none )


init : Model
init =
    { experiences = [] }
