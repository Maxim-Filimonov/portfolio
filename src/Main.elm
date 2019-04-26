module Main exposing (main)

import Browser exposing (Document, UrlRequest, application)
import Browser.Navigation exposing (Key, load, pushUrl)
import Element exposing (..)
import Element.Font exposing (underline)
import Url exposing (Url)



-- MODEL


type alias Flags =
    {}


type Page
    = Home


type alias Model =
    { key : Key
    , url : Url
    , page : Page
    }


type Msg
    = UrlChanged Url
    | LinkClicked UrlRequest



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )



-- VIEW


viewPage : Page -> { title : String }
viewPage page =
    case page of
        Home ->
            { title = "About Maxim Filimonov" }


view : Model -> Document Msg
view model =
    { title = viewPage model.page |> .title
    , body =
        [ Element.layout [] <|
            row [ centerY, spacingXY 10 0, padding 10 ]
                [ column [ width <| fillPortion 1 ]
                    [ image [] { src = "https://via.placeholder.com/350", description = "Portfolio photo" } ]
                , column
                    [ spacing 7, alignTop, paddingXY 0 10, width <| fillPortion 3 ]
                    [ paragraph []
                        [ el [] (text "I am a full stack software engineer with over a decade of experience in various industries. I have been part of large enterprises, world renown consultancies, and core team member of multiple startups. I focus on writing maintainable software aiming to help my clients to have long-lived products which are easy to adapt to future requirements. \nBesides programming,  I frequently do infrastructure work such as setting up build systems, servers and make CSS styling changes all within one day. \n")
                        , link [ underline ]
                            { url = "http://google.com"
                            , label = text "External link"
                            }
                        , link [ underline ]
                            { url = "/contact"
                            , label = text "Contact"
                            }
                        ]
                    ]
                ]
        ]
    }


main =
    application
        { init = init
        , view = view
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , subscriptions = \_ -> Sub.none
        , update = update
        }


init : Flags -> Url -> Key -> ( Model, Cmd msg )
init flags url key =
    ( Model key url Home, Cmd.none )
