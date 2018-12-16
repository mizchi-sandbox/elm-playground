port module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import Html exposing (..)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode exposing (Decoder, andThen, fail, field, int, map, oneOf, succeed)
import Random
import Svg exposing (circle, g, svg)
import Svg.Attributes exposing (..)


port toJs : String -> Cmd msg


screenWidth =
    300


screenHeight =
    300


type alias Player =
    { x : Int
    , y : Int
    }


type alias Bullet =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    }


type alias KeyState =
    { up : Bool
    , down : Bool
    , right : Bool
    , left : Bool
    }


type alias Model =
    { player : Player
    , bullets : List Bullet
    , keyState : KeyState
    , timerCount : Int
    , score : Int
    }


type Msg
    = KeyChange Bool String
    | Tick Float
    | Update Int


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
        , onAnimationFrameDelta Tick
        ]


init : ( Model, Cmd Msg )
init =
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
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyChange status key ->
            ( { model | keyState = updateKeyState status key model.keyState }
            , Cmd.batch [ toJs "aaa" ]
            )

        Tick delta ->
            ( model
            , Random.generate Update (Random.int 1 screenWidth)
            )

        Update n ->
            ( updateGameState model n
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


updateGameState : Model -> Int -> Model
updateGameState model num =
    let
        moveSpeed =
            3

        dx =
            if model.keyState.left then
                -moveSpeed

            else if model.keyState.right then
                moveSpeed

            else
                0

        dy =
            if model.keyState.up then
                -moveSpeed

            else if model.keyState.down then
                moveSpeed

            else
                0

        newBullets =
            model.bullets
                |> killBullets model.player
                |> spawnBullets num
                |> List.map
                    (\bullet ->
                        { bullet
                            | y = bullet.y + bullet.vy
                            , vy = bullet.vy + 0.1
                        }
                    )

        addingScore =
            model.bullets
                |> List.filter
                    (\bullet -> nearEnough model.player bullet)
                |> List.length
    in
    { model
        | player =
            { x = model.player.x + dx
            , y = model.player.y + dy
            }
        , bullets = newBullets
        , timerCount = model.timerCount + 1
        , score = model.score + addingScore
    }


killBullets : Player -> List Bullet -> List Bullet
killBullets player bullets =
    bullets
        |> List.filter (\b -> b.y < screenHeight)
        |> List.filter (\bullet -> not (nearEnough player bullet))


nearEnough : Player -> Bullet -> Bool
nearEnough player bullet =
    (bullet.x - toFloat player.x) ^ 2 + (bullet.y - toFloat player.y) ^ 2 < 10 ^ 2


spawnBullets : Int -> List Bullet -> List Bullet
spawnBullets num bullets =
    if List.length bullets < 15 then
        bullets |> List.append [ createNewBullet (toFloat num) 10 ]

    else
        bullets


createNewBullet : Float -> Float -> Bullet
createNewBullet x y =
    { x = x
    , y = y
    , vx = 0.0
    , vy = 1.0
    }


view : Model -> Html Msg
view { player, bullets, score } =
    svg
        [ width (String.fromInt screenWidth)
        , height (String.fromInt screenHeight)
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
