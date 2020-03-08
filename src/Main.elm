module Main exposing (..)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (..)
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>), (<?>), Parser, top)
import Url.Parser.Query as Query


type Msg
    = ChangeUrl Url
    | ClickLink UrlRequest


type ApplicationRoute
    = Greetings
    | About
    | Items (Maybe Int)
    | Item String
    | NewItem


type alias Model =
    { navKey : Nav.Key
    , route : Maybe ApplicationRoute
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    let
        _ = Debug.log "url" url
        _ = Debug.log "urlParser" (UrlParser.parse urlParser url)
    in
    ( { navKey = navKey, route = UrlParser.parse urlParser url }, Cmd.none )


urlParser : UrlParser.Parser (ApplicationRoute -> a) a
urlParser =
    UrlParser.oneOf
        [ UrlParser.map Greetings top
        , UrlParser.map About (UrlParser.s "about")
        , UrlParser.map NewItem (UrlParser.s "items-new")
        , UrlParser.map Items (UrlParser.s "items" <?> Query.int "page")
        , UrlParser.map Item (UrlParser.s "items" </> UrlParser.string)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ = Debug.log "Update" msg
    in
    case msg of
        ChangeUrl url ->
            let
                _ = Debug.log "urlParser" (UrlParser.parse urlParser url)
            in
            ( { model | route = UrlParser.parse urlParser url }, Cmd.none )

        ClickLink urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model, Nav.pushUrl model.navKey <| Url.toString url )

                External url ->
                    ( model, Nav.load url )


view : Model -> Document Msg
view model =
    let
        inline =
            style "display" "inline-block"

        padded =
            style "padding" "10px"

        menu =
            div [ style "padding" "10px", style "border-bottom" "1px solid #c0c0c0" ]
                [ a [ inline, padded, href "/Basics" ] [ text "Basics" ]
                , a [ inline, padded, href "/Maybe" ] [ text "Maybe" ]
                , a [ inline, padded, href "/List" ] [ text "List" ]
                , a [ inline, padded, href "/List#map" ] [ text "List.map" ]
                , a [ inline, padded, href "/List#filter" ] [ text "List.filter" ]
                , a [ inline, padded, href "/items" ] [ text "Items" ]
                , a [ inline, padded, href "/items-new" ] [ text "New Item" ]
                ]

    in
    { title = "URL handling example"
    , body =
        [ menu
        , showView model
        ]
    }

showView : Model -> Html msg
showView model =
    case model.route of
        Just Greetings ->  text "Greetings page"

        _ -> text "other page"

main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = ClickLink
        , onUrlChange = ChangeUrl
        }