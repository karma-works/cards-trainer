module HintsTest exposing (..)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Hints exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (containing, tag, text)


suite : Test
suite = test "given some words, when show words with hints, there is the word and the word in the title" <|
    \_ -> let
                testWords = [{ text="Nationality",  pronunciation="pinyin",  hint="It begins with B" }, {text = "Geburtsort", pronunciation = "pinyin", hint = "It begins with B" }]
                popoverState = Dict.fromList []
            in  (Hints.withWordElements popoverState testWords)
            |> Query.fromHtml
            |> Query.findAll [text "Nationality"]
            |> Query.count (Expect.equal 2)

