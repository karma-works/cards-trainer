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
    { popoverState : Popover.State }



-- Define a message to handle popover state changes


type Msg
    = PopoverMsg Popover.State



-- Initialize the popover state


initialState : () -> ( Model, Cmd Msg )
initialState _ =
    ( { popoverState = Popover.initialState }, Cmd.none )



-- Step the popover state forward in your update function


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PopoverMsg state ->
            ( { model | popoverState = state }, Cmd.none )



-- Compose a popover in your view (or a view helper function)


view : Model -> Html Msg
view model =
    Form.group []
        [ CDN.stylesheet
        , Form.label []
            [ text "Username "
            , Popover.config
                (Button.button
                    [ Button.small
                    , Button.primary
                    , Button.attrs <|
                        Popover.onClick model.popoverState PopoverMsg
                    ]
                    [ span [ class "fa fa-question-circle" ]
                        []
                    ]
                )
                |> Popover.right
                |> Popover.titleH4 [] [ text "Username help" ]
                |> Popover.content []
                    [ text "Your username must not contain numbers..." ]
                |> Popover.view model.popoverState
            , withHints model testPhrase testHints
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
    div [] <| List.map2 (mapHint model) listOfWords hints


mapHint : Model -> String -> String -> Html Msg
mapHint model word hint =
    if String.isEmpty hint then
        pre [] [ span [] [ text word ] ]

    else
        Popover.config
            (pre []
                [ span (class "fa fa-question-circle" :: Popover.onClick model.popoverState PopoverMsg)
                    [ text <| word ++ " " ]
                ]
            )
            |> Popover.bottom
            |> Popover.titleH4 [] [ text "Word Help" ]
            |> Popover.content []
                [ text hint ]
            |> Popover.view model.popoverState
