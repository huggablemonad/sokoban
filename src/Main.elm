module Main exposing (main)

{-| Module containing the entry point to the `sokoban` program.

@docs main

-}

import Html exposing (Html)
import Html.Attributes as Html
import Keyboard
import Level


{-| Main entry point.
-}
main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


{-| Application state.
-}
type alias Model =
    { level : Level.Model
    }


{-| Initialize the application state.
-}
init : ( Model, Cmd Msg )
init =
    ( Level.init 0
        |> Model
    , Cmd.none
    )


{-| Messages handled by the application.
-}
type Msg
    = Key Keyboard.KeyCode


{-| Update the application state.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Key keyCode ->
            if keyCode == 78 then  -- 'n'
                control NextLevel model
            else if keyCode == 80 then  -- 'p'
                control PrevLevel model
            else if keyCode == 82 then  -- 'r'
                control ReloadLevel model
            else
                let
                    newModel =
                        Level.update keyCode model.level
                in
                    ( { model | level = newModel }, Cmd.none )


{-| Used for changing or resetting levels.
-}
type Control
    = NextLevel
    | PrevLevel
    | ReloadLevel


{-| Return a new level or reset the current one.
-}
control : Control -> Model -> ( Model, Cmd Msg )
control ctrl model =
    let
        f =
            case ctrl of
                NextLevel ->
                    (+) 1

                PrevLevel ->
                    (+) -1

                ReloadLevel ->
                    identity
    in
        ( model.level
            |> Level.currentLevel
            |> f
            |> Level.init
            |> Model
        , Cmd.none
        )


{-| Event sources that the application subscribes to.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.downs Key


{-| View the application state as HTML.
-}
view : Model -> Html Msg
view model =
    Html.div []
        [ Html.span
            [ Html.style
                [ ( "background", "#53bce8" )
                , ( "border-radius", "50%" )
                , ( "font-size", "x-large" )
                , ( "font-weight", "bold" )
                , ( "margin-top", "0" )
                , ( "padding", "0.2em" )
                , ( "position", "absolute" )
                , ( "right", "1em" )
                , ( "text-align", "center" )
                , ( "vertical-align", "middle" )
                , ( "width", "2em" )
                ]
            ]
            [ Html.text << toString << (+) 1 <| Level.currentLevel model.level ]
        , Html.div
            [ Html.style
                [ ( "margin-left", "2em" )
                , ( "margin-top", "2em" )
                ]
            ]
            [ Level.view model.level ]
        ]
