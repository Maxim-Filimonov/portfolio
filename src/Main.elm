module Main exposing (main)

import Browser exposing (Document, UrlRequest, application)
import Browser.Navigation exposing (Key, load, pushUrl)
import Html exposing (a, div, p, text)
import Html.Attributes exposing (href)
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
    { title = "I will update with Url"
    , body =
        [ div []
            [ text <| "Hello"
            , p []
                [ a [ href "http://google.com" ] [ text "External link" ]
                ]
            , p []
                [ a [ href "/contact" ] [ text "Contact" ]
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
