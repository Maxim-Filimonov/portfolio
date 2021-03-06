module GraphQLClient exposing (makeGraphQLQuery)

import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet exposing (SelectionSet)


graphql_url : String
graphql_url =
    "https://portfolio-data.herokuapp.com/v1/graphql"


makeGraphQLQuery : SelectionSet decodesTo RootQuery -> (Result (Graphql.Http.Error decodesTo) decodesTo -> msg) -> Cmd msg
makeGraphQLQuery query decodesTo =
    query
        |> Graphql.Http.queryRequest graphql_url
        |> Graphql.Http.send decodesTo
