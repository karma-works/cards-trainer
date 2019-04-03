module Hints exposing (Model, Msg(..), initialState, main, wordElementView, subscriptions, update, view, withWordElements)

import Bootstrap.CDN as CDN
import Bootstrap.Form as Form
import Bootstrap.Popover as Popover
import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, pre, span, text)
import Html.Attributes exposing (class)
import List
import Main exposing (Word)



-- MAIN


main =
    Browser.element
        { init = initialState
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias Model =
    { popoverState : Dict Int Popover.State
    , popoverCounter : Int
    }



-- Define a message to handle popover state changes


type Msg
    = PopoverMsg Int Popover.State



-- Initialize the popover state


initialState : () -> ( Model, Cmd Msg )
initialState _ =
    ( { popoverState = Dict.fromList [], popoverCounter = 0 }, Cmd.none )



-- Step the popover state forward in your update function


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PopoverMsg index state ->
            ( { model | popoverState = setPopoverState index state model.popoverState }, Cmd.none )


setPopoverState : Int -> Popover.State -> Dict Int Popover.State -> Dict Int Popover.State
setPopoverState index record records =
    Dict.update index (mapRecords record) (Dict.map (\_ _ -> Popover.initialState) records)


mapRecords : Popover.State -> Maybe Popover.State -> Maybe Popover.State
mapRecords record previousRecord =
    case previousRecord of
        _ ->
            Just record


getPopoverState : Int -> Dict Int Popover.State -> Popover.State
getPopoverState id records =
    Maybe.withDefault Popover.initialState (Dict.get id records)



-- Compose a popover in your view (or a view helper function)


view : Model -> Html Msg
view model =
    Form.group []
        [ CDN.stylesheet
        , Form.label []
            [ withWordElements model testWords
            ]
        ]



type alias Phrase = List Word

testWords = [{ text="Nationalität",  pronunciation="pinyin",  hint="It begins with B" }, {text = "Geburtsort", pronunciation = "pinyin", hint = "It begins with B" }]


withWordElements : Model -> Phrase -> Html Msg
withWordElements model phrase  =
    div [] <| List.map2 (wordElementView model) phrase <| List.range 0 <| List.length phrase


wordElementView : Model -> Word -> Int -> Html Msg
wordElementView model word index =
    if String.isEmpty word.hint then
        pre [] [ div [] [ text word.pronunciation ], div [] [ text word.text ] ]

    else
        Popover.config
            (pre []
                [ div [] [ text word.pronunciation ]
                , div (class "fa fa-question-circle" :: Popover.onClick (getPopoverState index model.popoverState) (PopoverMsg index))
                    [ text <| word.text ++ " " ]

                ]
            )
            |> Popover.bottom
            |> Popover.titleH4 [] [ text word.text ]
            |> Popover.content []
                [ text word.hint ]
            |> Popover.view (getPopoverState index model.popoverState)
