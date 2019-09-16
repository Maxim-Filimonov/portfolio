module Main exposing (main)

import Browser exposing (Document, UrlRequest, application)
import Browser.Dom
import Browser.Events
import Browser.Navigation exposing (Key, load, pushUrl)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font exposing (underline)
import Task
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
    , viewPort : Maybe WindowSize
    }


type alias WindowSize =
    { height : Int
    , width : Int
    }


type Msg
    = UrlChanged Url
    | LinkClicked UrlRequest
    | GetViewport Browser.Dom.Viewport
    | ViewPortChanged Int Int



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

        GetViewport viewPort ->
            ( { model | viewPort = Just <| WindowSize (round viewPort.scene.height) (round viewPort.scene.width) }, Cmd.none )

        ViewPortChanged width height ->
            ( { model
                | viewPort = Just <| WindowSize height width
              }
            , Cmd.none
            )



-- VIEW


viewPage : Page -> { title : String }
viewPage page =
    case page of
        Home ->
            { title = "About Maxim Filimonov" }


viewPhotoUrl : Element msg
viewPhotoUrl =
    image [] { src = "https://via.placeholder.com/350", description = "Portfolio photo" }


viewDescription : Element msg
viewDescription =
    el [] (text "I am a full stack software engineer with over a decade of experience in various industries. I have been part of large enterprises, world renown consultancies, and core team member of multiple startups. I focus on writing maintainable software aiming to help my clients to have long-lived products which are easy to adapt to future requirements. \nBesides programming,  I frequently do infrastructure work such as setting up build systems, servers and make CSS styling changes all within one day. \n")


viewLinks : List (Element msg)
viewLinks =
    [ link [ underline ]
        { url = "http://google.com"
        , label = text "External link"
        }
    , link [ underline ]
        { url = "/contact"
        , label = text "Contact"
        }
    ]


menuSeparator : Element msg
menuSeparator =
    el
        [ Border.width 1
        , padding 3
        , Background.color <| rgb255 0 0 0
        , Border.rounded 5
        ]
    <|
        text ""


menu : Element msg
menu =
    Element.row [ centerX, spacing 7 ]
        [ link [] { url = "/", label = text "Home" }
        , menuSeparator
        , link [] { url = "/experience", label = text "Experience" }
        , menuSeparator
        , link [] { url = "#contact", label = text "Contact" }
        ]


view : Model -> Document Msg
view model =
    { title = viewPage model.page |> .title
    , body =
        [ Element.layout [] <|
            column
                [ width fill
                ]
                [ menu
                , row [ centerY, spacingXY 10 0, padding 10 ]
                    [ column [ width <| fillPortion 1 ] [ viewPhotoUrl ]
                    , column
                        [ spacing 7, alignTop, paddingXY 0 10, width <| fillPortion 3 ]
                      <|
                        List.append [ paragraph [] [ viewDescription ] ]
                            viewLinks
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
        , subscriptions = subscriptions
        , update = update
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onResize ViewPortChanged


getInitialWindowSize : Cmd Msg
getInitialWindowSize =
    Task.perform (\viewport -> GetViewport viewport) Browser.Dom.getViewport


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url Home Nothing, getInitialWindowSize )
