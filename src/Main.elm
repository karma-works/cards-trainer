module Main exposing (..)

import Basics as Math
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button exposing (..)
import Bootstrap.CDN as CDN
import Bootstrap.Form as Form
import Bootstrap.Form.Input as BSInput
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar exposing (..)
import Bootstrap.Progress as Progress
import Bootstrap.Utilities.Display as Display
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import Debug exposing (toString)
import Hints exposing (HintModel, HintMsgModel, Word)
import Html as Input exposing (Html, div, h1, h2, text)
import Html.Attributes exposing (for, href, placeholder, readonly, value)
import Http exposing (Error(..))
import Json.Decode exposing (Decoder, Error(..), field, map3, string)
import List exposing (head)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- Model


type alias Card =
    { question : List Word
      , answer: List Word
    , acceptedAnswer : List String
    }


type InteractionState
    = AnswerCorrect String
    | AnswerIncorrect String
    | Question Card
    | Error String
    | Loading


type alias Model =
    { cards : List Card, state : InteractionState, answer : String, navbarState : Navbar.State, valid : Bool, progress : Int, hintState : HintModel}


loadCards : Cmd Msg
loadCards =
    Http.get
        { url = "http://localhost:3000/questions"
        , expect = Http.expectJson ReceivedCards decodeCards
        }


decodeWord : Json.Decode.Decoder Word
decodeWord = map3 Word (field "text" string) (field "pronunciation" string) (field "hint" string)

decodeWords : Json.Decode.Decoder (List Word)
decodeWords = Json.Decode.list decodeWord

decodeCard : Json.Decode.Decoder Card
decodeCard =
    map3 Card (field "question" decodeWords) (field "answer" decodeWords) (field "acceptedAnswer" (Json.Decode.list string) )


decodeCards : Json.Decode.Decoder (List Card)
decodeCards =
    Json.Decode.list decodeCard



-- The navbar needs to know the initial window size, so the inital state for a navbar requires a command to be run by the Elm runtime


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( navbarState, _ ) =
            Navbar.initialState NavbarMsg
    in
    ( { cards = [], state = Loading, answer = "", navbarState = navbarState, valid = False, progress = 0, hintState = Hints.initialModelValues }, loadCards )


type Msg
    = ChangeAnswer String
    | CheckAnswer String
    | AskNewQuestion
    | NavbarMsg Navbar.State
    | ReceivedCards (Result Http.Error (List Card))
    | HintMsg HintMsgModel



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )

        ReceivedCards result ->
            case result of
                Ok cards ->
                    case head cards of
                        Just card ->
                            ( { model | cards = cards, state = Question card}, Cmd.none )

                        _ ->
                            ( { model | state = Error "We can't show you new questions at the moment, probably a connectivity issue. Try again later" }, Cmd.none )

                Err failure ->
                    --                    Debug.todo "crash"
                    ( { model | state = Error <| httpErrorString failure }, Cmd.none )

        ChangeAnswer userAnswer ->
            ( { model | answer = userAnswer, valid = String.length userAnswer > 0 }, Cmd.none )

        CheckAnswer userAnswer ->
            case head model.cards of
                Just card ->
                    if List.member userAnswer card.acceptedAnswer then
                        ( { model | state = AnswerCorrect "Correct! ", progress = model.progress + 1 }, Cmd.none )

                    else
                        ( { model | state = AnswerIncorrect <| "Incorrect. The correct answer is «" ++ (Maybe.withDefault "" (head card.acceptedAnswer) ) ++ "».", progress = max 0 (model.progress - 1) }, Cmd.none )

                _ ->
                    ( { model | state = AnswerCorrect "Could not load new questions. Probably this is because I can't connect to our service" }, Cmd.none )

        AskNewQuestion ->
            case head model.cards of
                Just card ->
                    ( { model | state = Question card, cards = List.drop 1 model.cards, answer = "", valid = False }, if List.length model.cards > 1 then
                    Cmd.none
                    else
                    loadCards
                    )
                _ -> ( { model | state =  AnswerCorrect "Could not load new questions. Probably this is because I can't connect to our service"  }, Cmd.none )

        HintMsg (state)->  ( { model | hintState = Hints.setPopoverState state <| model.hintState }, Cmd.none )




-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navbarState NavbarMsg



-- View


view : Model -> Html Msg
view model =
    div [] <|
        pageGrid model
            :: []


pageGrid : Model -> Html Msg
pageGrid model =
    Grid.containerFluid []
        [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
        , Grid.row []
            [ Grid.col []
                [ navigationBar model.navbarState ]
            ]
        , Grid.row []
            [ Grid.col []
                [ progressBar model.progress ]
            ]
        , Grid.row []
            [ Grid.col []
                [ quizSection model ]
            ]
        ]


progressBar : Int -> Html Msg
progressBar progress =
    div []
        [ Progress.progress [ Progress.value <| Math.toFloat <| Math.modBy 100 progress * 10 ]
        ]


quizSection : Model -> Html Msg
quizSection model =
    Form.form
        []
        [ h1 [] [ text "Quiz" ]
        , Form.group []
            [ Form.row [] [ Form.col [] [ h2 [] [ text <| questionText model.state] ] ]
            , Form.row []
                [ Form.col [] <|
                    quizFreeText model
                ]
            , Form.row []
                [ Form.col []
                    [ submitButton model, continueButton model.state ]
                ]
            , Form.row [] [ Form.col [] [ resultAlert model.state ] ]
            ]
        ]

questionText : InteractionState -> String
questionText state = case state of
    Question card ->
           card.question |> List.map .text |> List.foldl (++) ""
    _ -> ""

quizFreeText : Model -> List (Html Msg)
quizFreeText model =
    [ Form.label [ for "your-answer" ] [ text "Your Answer " ]
    , Textarea.textarea [ Textarea.id "your-answer", Textarea.onInput ChangeAnswer, Textarea.attrs [ readonly (not (questionState model)), placeholder "Type your answer here", value model.answer ] ]
    ]

questionState: Model -> Bool
questionState model = case model.state of
    Question _-> True
    _ -> False


submitButton : Model -> Html Msg
submitButton model =
    button
        [ disabled (not model.valid)
        , Button.primary
        , Button.attrs [ Spacing.ml2Sm, submitVisible model.state ]
        , onClick <| CheckAnswer model.answer
        ]
        [ text "submit" ]


submitVisible : InteractionState -> Input.Attribute msg
submitVisible state =
    case state of
        Question _->
            Display.inline

        _ ->
            Display.none


continueButton : InteractionState -> Html Msg
continueButton state =
    button
        [ Button.primary
        , Button.attrs
            [ Spacing.ml2Sm
            , if submitVisible state == Display.none then
                Display.inline

              else
                Display.none
            ]
        , onClick AskNewQuestion
        ]
        [ text "Continue" ]


resultAlert : InteractionState -> Html Msg
resultAlert state =
    case state of
        AnswerCorrect message ->
            Alert.simpleSuccess [] [ text message ]

        Error message ->
            Alert.simpleWarning [] [ text message ]

        AnswerIncorrect message ->
            Alert.simpleDanger [] [ text message ]

        _ ->
            Alert.simpleLight [] [ text "" ]


navigationBar : Navbar.State -> Html Msg
navigationBar navbarState =
    Navbar.config NavbarMsg
        |> Navbar.withAnimation
        |> Navbar.collapseMedium
        -- Collapse menu at the medium breakpoint
        |> Navbar.info
        -- Customize coloring
        |> Navbar.brand
            -- Add logo to your brand with a little styling to align nicely
            [ href "#" ]
            [ text "Elm Quiz"
            ]
        |> Navbar.items
            [ Navbar.itemLink
                [ href "#" ]
                [ text "Menu Item" ]
            , Navbar.dropdown
                -- Adding dropdowns is pretty simple
                { id = "mydropdown"
                , toggle = Navbar.dropdownToggle [] [ text "Drop Down Menu" ]
                , items =
                    [ Navbar.dropdownHeader [ text "Heading" ]
                    , Navbar.dropdownItem
                        [ href "#" ]
                        [ text "Drop item 1" ]
                    , Navbar.dropdownItem
                        [ href "#" ]
                        [ text "Drop item 2" ]
                    , Navbar.dropdownDivider
                    , Navbar.dropdownItem
                        [ href "#" ]
                        [ text "Drop item 3" ]
                    ]
                }
            ]
        |> Navbar.customItems
            [ Navbar.formItem []
                [ BSInput.text [ BSInput.attrs [ placeholder "Search" ] ]
                , Button.button
                    [ Button.success
                    , Button.attrs [ Spacing.ml2Sm ]
                    ]
                    [ text "Search" ]
                ]
            ]
        |> Navbar.view navbarState


httpErrorString : Http.Error -> String
httpErrorString error =
    case error of
        BadUrl text ->
            "Bad Url: " ++ text

        Timeout ->
            "Http Timeout"

        NetworkError ->
            "Network Error"

        BadStatus response ->
            "Bad Http Status: " ++ toString response
        BadBody response ->
            "Bad Http Body: " ++ response
--        _ ->
--            "An unknown http error occurred"
