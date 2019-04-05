module Hints exposing (setPopoverState, initialModelValues, HintMsgModel, Word, HintModel, Msg(..), initialState, main, wordElementView, subscriptions, update, view, withWordElements)

import Array
import Bootstrap.CDN as CDN
import Bootstrap.Form as Form
import Bootstrap.Popover as Popover
import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, pre, span, text)
import Html.Attributes exposing (class)
import List



type alias Word = {
    text: String
    , pronunciation: String
    , hint: String
    }

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

type HintModel = HintModel {
    popoverState : Dict Int Popover.State
    , phrase: Phrase
    }

initialModelValues:  HintModel
initialModelValues = setModelValues (Dict.fromList []) []
getModelValues: HintModel -> (Dict Int Popover.State, Phrase)
getModelValues (HintModel {popoverState, phrase }) = (popoverState, phrase)

getModelPopoverStates: HintModel -> Dict Int Popover.State
getModelPopoverStates (HintModel model) = model.popoverState

getModelPhrase: HintModel -> Phrase
getModelPhrase  (HintModel model) = model.phrase

setModelPopupStates: HintModel -> Dict Int Popover.State -> HintModel
setModelPopupStates model newState = setModelValues newState <| getModelPhrase model

setModelValues: Dict Int Popover.State -> Phrase -> HintModel
setModelValues state phrase = HintModel {popoverState = state, phrase = phrase}

type HintMsgModel = HintMsgModel {
    index: Int
    , state: Popover.State
    }

createHintMsgModel: Int -> Popover.State -> HintMsgModel
createHintMsgModel index state = HintMsgModel { index = index,  state = state }

type alias Model =
    { popoverState : HintModel
    }



-- Define a message to handle popover state changes


type Msg
    = PopoverMsg HintMsgModel



-- Initialize the popover state


initialState : () -> ( Model, Cmd Msg )
initialState _ =
    ( Model ( initialModelValues ), Cmd.none )



-- Step the popover state forward in your update function


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PopoverMsg hintMsgModel ->
            ( { model | popoverState = setPopoverState hintMsgModel <| model.popoverState }, Cmd.none )


setPopoverState : HintMsgModel -> HintModel -> HintModel
setPopoverState (HintMsgModel hintMsgModel) hintModel =
    let
        records = getModelPopoverStates hintModel
        updatedStates = Dict.update hintMsgModel.index (mapRecords hintMsgModel.state) (Dict.map (\_ _ -> Popover.initialState) records)
    in
        setModelPopupStates hintModel updatedStates


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
            [ withWordElements model.popoverState
--             words go here as last parameter
            ]
        ]



type alias Phrase = List Word



withWordElements : HintModel -> Html Msg
withWordElements hintModel  =
    let
        (_, phrase) = getModelValues hintModel
    in
        div [] <| (List.length phrase |> List.range 0 |> List.map (wordElementView hintModel))


wordElementView : HintModel -> Int -> Html Msg
wordElementView model index =
    let
        (popoverState, phrase) = getModelValues model
        word = Array.fromList phrase |> Array.get index |> Maybe.withDefault emptyWord
    in
        if String.isEmpty word.hint then
            pre [] [ div [] [ text word.pronunciation ], div [] [ text word.text ] ]

        else
            Popover.config
                (pre []
                    [ div [] [ text word.pronunciation ]
                    , div (class "fa fa-question-circle" :: Popover.onClick (getPopoverState index popoverState) (\state -> PopoverMsg <| createHintMsgModel index state))
                        [ text <| word.text ++ " " ]
                    ]
                )
                |> Popover.bottom
                |> Popover.titleH4 [] [ text word.text ]
                |> Popover.content []
                    [ text word.hint ]
                |> Popover.view (getPopoverState index popoverState)


emptyWord = {
    text =  ""
    , pronunciation =  ""
    , hint =  ""
    }