module Data exposing (Model, Msg, fetchExperiences, init, update)

-- import PortfolioData.Object.Experiences as Experiences

import GraphQLClient exposing (makeGraphQLQuery)
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
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
    List Int


queryExperiences : SelectionSet Response RootQuery
queryExperiences =
    Query.experiences (\args -> args) PortfolioData.Object.Experiences.id


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



-- import Hasura.Enum.Order_by exposing (Order_by(..))
-- import Hasura.InputObject
--     exposing
--         ( Boolean_comparison_exp
--         , Todos_bool_exp
--         , Todos_order_by
--         , buildBoolean_comparison_exp
--         , buildTodos_bool_exp
--         , buildTodos_order_by
--         )
-- import Hasura.Object
-- import Hasura.Object.Todos as Todos
-- import Hasura.Object.Users as Users
-- import Hasura.Query as Query exposing (TodosOptionalArguments)
