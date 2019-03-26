module Hints exposing (Model, Msg(..), initialState, main, mapHint, subscriptions, testHints, testPhrase, update, view, withHints)

import Basics as Math
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button exposing (..)
import Bootstrap.CDN as CDN
import Bootstrap.Form as Form
import Bootstrap.Form.Input as BSImput
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar exposing (..)
import Bootstrap.Popover as Popover
import Bootstrap.Progress as Progress
import Bootstrap.Utilities.Display as Display
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import Debug exposing (toString)
import Dict exposing (Dict)
import Html as Input exposing (Html, div, pre, span, text)
import Html.Attributes as Input exposing (attribute, class, for, href, placeholder, readonly, value)
import Http exposing (Error(..))
import Json.Decode exposing (Decoder, Error(..), field, map3, string)
import List exposing (head)
import Maybe.Extra
import String.Extra as Extra exposing (softBreak)



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
            [ withHints model testPhrase testHints
            ]
        ]


testPhrase =
    "Your username, must not contain numbers."


testHints =
    [ "1", "2", "3", "4", "5", "" ]


withHints : Model -> String -> List String -> Html Msg
withHints model phrase hints =
    let
        listOfWords =
            String.words phrase
    in
    div [] <| List.map3 (mapHint model) listOfWords hints <| List.range 0 <| List.length hints


mapHint : Model -> String -> String -> Int -> Html Msg
mapHint model word hint index =
    if String.isEmpty hint then
        pre [] [ span [] [ text word ] ]

    else
        Popover.config
            (pre []
                [ span (class "fa fa-question-circle" :: Popover.onClick (getPopoverState index model.popoverState) (PopoverMsg index))
                    [ text <| word ++ " " ]
                ]
            )
            |> Popover.bottom
            |> Popover.titleH4 [] [ text "Word Help" ]
            |> Popover.content []
                [ text hint ]
            |> Popover.view (getPopoverState index model.popoverState)
