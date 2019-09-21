module Main exposing (main)

import Browser exposing (Document, UrlRequest, application)
import Browser.Dom
import Browser.Events
import Browser.Navigation exposing (Key, load, pushUrl)
import Data exposing (Experience, fetchExperiences)
import Element exposing (Element, alignRight, alignTop, centerX, centerY, column, el, fill, fillPortion, image, link, padding, paddingXY, paragraph, px, rgb255, row, spacing, spacingXY, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font exposing (underline)
import Gravatar
import Task
import Url exposing (Url)



-- MODEL


type alias Flags =
    {}


type Page
    = HomePage
    | ExperiencesPage


type alias Model =
    { key : Key
    , url : Url
    , page : Page
    , viewPort : Maybe WindowSize
    , data : Data.Model
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
    | Data Data.Msg



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
            ( { model | url = url, page = determinePage url }
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

        Data dataMsg ->
            let
                ( innerModel, innerCmd ) =
                    Data.update dataMsg model.data
            in
            ( { model | data = innerModel }, Cmd.map Data innerCmd )



-- VIEW


viewPage : Model -> { title : String, body : List (Element.Element Msg) }
viewPage model =
    let
        filterFeaturedExperience : Experience -> Bool
        filterFeaturedExperience experience =
            List.any (String.contains "featured") <| List.map .name experience.tags

        featuredExperiences =
            List.filter filterFeaturedExperience model.data.experiences

        determineLayoutType : { a | width : Int } -> List (Element.Attribute msg) -> List (Element msg) -> Element msg
        determineLayoutType viewPort =
            if viewPort.width > 670 then
                row

            else
                column

        layoutType =
            Maybe.withDefault row <| Maybe.map determineLayoutType model.viewPort
    in
    case model.page of
        HomePage ->
            { title = "About Maxim Filimonov"
            , body =
                [ layoutType [ spacingXY 10 0, padding 10 ]
                    [ column [ width <| fillPortion 1 ] [ viewPhotoUrl ]
                    , column
                        [ spacing 7, alignTop, paddingXY 0 10, width <| fillPortion 3 ]
                      <|
                        List.append [ paragraph [] [ viewDescription ] ]
                            viewLinks
                    ]
                , viewExperiences featuredExperiences
                ]
            }

        ExperiencesPage ->
            { title = "Experience of Maxim Filimonov"
            , body = [ Element.text "Experiences will be here" ]
            }


determinePage : Url -> Page
determinePage url =
    if String.endsWith "experience" url.path then
        ExperiencesPage

    else
        HomePage


viewPhotoUrl : Element msg
viewPhotoUrl =
    let
        gravatarOptions =
            Gravatar.defaultOptions |> Gravatar.withSize (Just 250)
    in
    image [ Border.rounded 30, Element.clip, centerX ]
        { src = Gravatar.url gravatarOptions "tpaktopsp@gmail.com"
        , description = "Portfolio photo"
        }


viewDescription : Element msg
viewDescription =
    el [] (text "I am a full stack software engineer with over a decade of experience in various industries. I have been part of large enterprises, world renown consultancies, and core team member of multiple startups. I focus on writing maintainable software aiming to help my clients to have long-lived products which are easy to adapt to future requirements. \nBesides programming,  I frequently do infrastructure work such as setting up build systems, servers and make CSS styling changes all within one day. \n")


viewLinks : List (Element msg)
viewLinks =
    [ row [ spacingXY 10 0, alignRight ]
        [ link [ underline ]
            { url = "https://github.com/Maxim-Filimonov"
            , label =
                image []
                    { src = "https://res.cloudinary.com/tpaktop/image/upload/w_90/github.jpg"
                    , description = "Github Profile"
                    }
            }
        , link [ underline ]
            { url = "https://twitter.com/TPAKTOP"
            , label =
                image []
                    { src = "https://res.cloudinary.com/tpaktop/image/upload/w_30/v1568623686/twitter.png"
                    , description = "Twitter Profile"
                    }
            }
        , link [ underline ]
            { url = "https://medium.com/@TPAKTOP"
            , label =
                image []
                    { src = "https://res.cloudinary.com/tpaktop/image/upload/c_scale,w_60/v1568623841/medium.png"
                    , description = "Medium Blog"
                    }
            }
        ]
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


viewExperience : Experience -> Element Msg
viewExperience experience =
    let
        unknownDefault =
            Maybe.withDefault "Unknown"

        companyName =
            unknownDefault experience.company_name

        role =
            unknownDefault experience.role
    in
    row [ Element.explain <| Debug.todo, width fill ]
        [ column [ spacing 10 ]
            [ el [] <| text companyName
            , el [] <| text role
            , column [] <| List.map (\t -> el [] <| text t.name) experience.tags
            ]
        ]


viewExperiences : List Experience -> Element Msg
viewExperiences experiences =
    column [ centerX ] <| List.map viewExperience experiences



-- column []
--     ([ el [] <| text "Experiences" ]
--     )


view : Model -> Document Msg
view model =
    let
        currentPage =
            viewPage model
    in
    { title = currentPage.title
    , body =
        [ Element.layout [ paddingXY 0 10 ] <|
            column
                [ width (fill |> Element.maximum 960)
                , centerX
                ]
                ([ menu ]
                    ++ currentPage.body
                )
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
    ( { key = key
      , url = url
      , page = HomePage
      , viewPort = Nothing
      , data = Data.init
      }
    , Cmd.batch [ getInitialWindowSize, Cmd.map Data fetchExperiences ]
    )
