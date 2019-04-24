module Main exposing (main)

import Browser exposing (Document, UrlRequest, application)
import Browser.Navigation exposing (Key, load, pushUrl)
import Element exposing (Element, column, el, link, padding, row, spacing, text)
import Element.Font exposing (underline)
import Url exposing (Url)


type alias Flags =
    {}


type alias Model =
    { key : Key
    , url : Url
    }


init : Flags -> Url -> Key -> ( Model, Cmd msg )
init flags url key =
    ( Model key url, Cmd.none )


type Msg
    = UrlChanged Url
    | LinkClicked UrlRequest


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


view : Model -> Document Msg
view model =
    { title = "Current url: " ++ model.url.path
    , body =
        [ Element.layout [] <|
            column [ spacing 7, padding 10 ]
                [ el [] (text model.url.path)
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
