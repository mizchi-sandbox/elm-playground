port module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import GameModel exposing (Bullet, Model, Player, screenHeight, screenWidth, updateGameState)
import Json.Decode as Decode exposing (Decoder, andThen, fail, field, int, map, oneOf, succeed)
import Random
import Svg exposing (circle, g, svg)
import Svg.Attributes exposing (..)
import Html exposing (Html)
import KeyState exposing (KeyState)


port keyPress : String -> Cmd msg


type Msg
    = KeyChange Bool String
    | Tick Float


main : Program Int Model Msg
main =
    Browser.element
        { view = view
        , init = \flags -> init flags
        , subscriptions = subscriptions
        , update = update
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown (Decode.map (KeyChange True) keyDecoder)
        , onKeyUp (Decode.map (KeyChange False) keyDecoder)
        , onAnimationFrameDelta Tick
        ]


init : Int -> ( Model, Cmd Msg )
init flags =
    ( { player = { x = truncate (screenWidth / 2), y = truncate (screenHeight - 10) }
      , bullets = []
      , keyState =
            { up = False
            , down = False
            , right = False
            , left = False
            }
      , timerCount = 0
      , score = 0
      , seed = Random.initialSeed flags
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyChange status key ->
            ( { model | keyState = updateKeyState status key model.keyState }
            , Cmd.batch [ keyPress key ]
            )

        Tick delta ->
            ( updateGameState model
            , Cmd.none
            )


updateKeyState : Bool -> String -> KeyState -> KeyState
updateKeyState status key keyState =
    if status then
        { up = isUpKey key
        , down = isDownKey key
        , left = isLeftKey key
        , right = isRightKey key
        }

    else
        { up =
            if isUpKey key && keyState.up then
                False

            else
                keyState.up
        , down =
            if isDownKey key && keyState.down then
                False

            else
                keyState.down
        , left =
            if isLeftKey key && keyState.left then
                False

            else
                keyState.left
        , right =
            if isRightKey key && keyState.right then
                False

            else
                keyState.right
        }


view : Model -> Html Msg
view { player, bullets, score } =
    svg
        [ width (String.fromInt screenWidth)
        , height (String.fromInt screenHeight)
        , style "background: #aaa;"
        ]
        [ viewPlayer player
        , viewScore score
        , g [] (List.map viewBullet bullets)
        ]


viewPlayer : Player -> Html Msg
viewPlayer player =
    circle
        [ cx (String.fromInt player.x)
        , cy (String.fromInt player.y)
        , r "7"
        , stroke "red"
        ]
        []


viewScore : Int -> Html Msg
viewScore n =
    Svg.text_
        [ x "5"
        , y "20"
        ]
        [ Svg.text (String.fromInt n)
        ]


viewBullet : Bullet -> Html Msg
viewBullet bullet =
    circle
        [ cx (String.fromInt (truncate bullet.x))
        , cy (String.fromInt (truncate bullet.y))
        , r "3"
        , stroke "black"
        ]
        []


keyDecoder : Decoder String
keyDecoder =
    Decode.field "key" Decode.string


isUpKey : String -> Bool
isUpKey key =
    List.member key [ ",", "w" ]


isDownKey : String -> Bool
isDownKey key =
    List.member key [ "o", "s" ]


isLeftKey : String -> Bool
isLeftKey key =
    List.member key [ "a" ]


isRightKey : String -> Bool
isRightKey key =
    List.member key [ "e", "d" ]
