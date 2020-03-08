module UrlParserTest exposing (..)

import Expect exposing (Expectation)
import Main exposing (ApplicationRoute(..), urlParser)
import Test exposing (..)
import Url exposing (Protocol(..), Url)
import Url.Parser as UrlParser

createUrl : String -> Url
createUrl path =
    { protocol = Http
    , host = "localhost"
    , port_ = Just 8000
    , path = path
    , query = Nothing
    , fragment = Nothing
    }

suite : Test
suite =
    describe "Testing Url Parser"
        [ test "when empty string" <|
            \_ ->
                let
                    url = ""
                in
                Expect.equal (Just Greetings) <|
                    (UrlParser.parse urlParser) <| (createUrl url)
        , test "when path is 'items-new'" <|
            \_ ->
                let
                    url = "items-new"
                in
                Expect.equal (Just NewItem) <|
                    (UrlParser.parse urlParser) <| (createUrl url)
        , test "when path is 'items'" <|
            \_ ->
              let
                  url = "items"
              in
              Expect.equal (Just Items) <|
                  (UrlParser.parse urlParser) <| (createUrl url)
        ]
