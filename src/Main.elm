module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import Html exposing (..)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode exposing (Decoder, andThen, fail, field, int, map, oneOf, succeed)
import Svg exposing (circle, svg)
import Svg.Attributes exposing (..)


type alias KeyState =
    { up : Bool
    , down : Bool
    , right : Bool
    , left : Bool
    }


type alias Model =
    { player :
        { x : Int
        , y : Int
        }
    , keyState : KeyState
    }


type Msg
    = KeyChange Bool String
    | Tick Float


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \() -> init
        , subscriptions = subscriptions
        , update = update
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown (Decode.map (KeyChange True) keyDecoder)
        , onKeyUp (Decode.map (KeyChange False) keyDecoder)
        , if anyIsDown model then
            onAnimationFrameDelta Tick

          else
            Sub.none
        ]


init : ( Model, Cmd Msg )
init =
    ( { player = { x = 30, y = 30 }
      , keyState =
            { up = False
            , down = False
            , right = False
            , left = False
            }
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyChange status key ->
            if status then
                ( { model
                    | keyState =
                        { up = key == ","
                        , down = key == "o"
                        , left = key == "a"
                        , right = key == "e"
                        }
                  }
                , Cmd.none
                )

            else
                ( { model
                    | keyState =
                        { up = not (key /= "," || model.keyState.up)
                        , down = not (key /= "o" || model.keyState.down)
                        , left = not (key /= "a" || model.keyState.left)
                        , right = not (key /= "e" || model.keyState.right)
                        }
                  }
                , Cmd.none
                )

        Tick delta ->
            let
                player =
                    model.player

                dx =
                    if model.keyState.left then
                        -1

                    else if model.keyState.right then
                        1

                    else
                        0

                dy =
                    if model.keyState.up then
                        -1

                    else if model.keyState.down then
                        1

                    else
                        0

                newModel =
                    { model
                        | player =
                            { x = player.x + dx
                            , y = player.y + dy
                            }
                    }
            in
            ( newModel, Cmd.none )


view : Model -> Html Msg
view model =
    svg
        [ width "600"
        , height "600"
        ]
        [ circle
            [ cx (String.fromInt model.player.x)
            , cy (String.fromInt model.player.y)
            , r "10"
            ]
            []
        ]


keyDecoder : Decoder String
keyDecoder =
    Decode.field "key" Decode.string


anyIsDown : Model -> Bool
anyIsDown model =
    model.keyState.up
        || model.keyState.down
        || model.keyState.left
        || model.keyState.right
