module GameModel exposing (Bullet, Model, Player, screenHeight, screenWidth, updateGameState)

import KeyState exposing (KeyState)
import Random


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


type alias Model =
    { player : Player
    , bullets : List Bullet
    , keyState : KeyState
    , timerCount : Int
    , score : Int
    , seed : Random.Seed
    }


updateGameState : Model -> Model
updateGameState model =
    let
        newModel =
            model
                |> updatePlayerPosition
                |> killBullets model.player
                |> spawnBullets
                |> updateBulletsPosition
                |> addScore
    in
    { newModel
        | timerCount = model.timerCount + 1
    }


addScore : Model -> Model
addScore model =
    let
        addingScore =
            model.bullets
                |> List.filter
                    (\bullet -> nearEnough model.player bullet)
                |> List.length
    in
    { model
        | score = model.score + addingScore
    }


updatePlayerPosition : Model -> Model
updatePlayerPosition model =
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
    in
    { model
        | player =
            { x = model.player.x + dx
            , y = model.player.y + dy
            }
    }


updateBulletsPosition : Model -> Model
updateBulletsPosition model =
    { model
        | bullets =
            model.bullets
                |> List.map
                    (\bullet ->
                        { bullet
                            | y = bullet.y + bullet.vy
                            , vy = bullet.vy + 0.1
                        }
                    )
    }


killBullets : Player -> Model -> Model
killBullets player model =
    { model
        | bullets =
            model.bullets
                |> List.filter (\b -> b.y < screenHeight)
                |> List.filter (\bullet -> not (nearEnough player bullet))
    }


spawnBullets : Model -> Model
spawnBullets model =
    let
        ( ( x, y ), newSeed ) =
            Random.step
                (Random.map2 Tuple.pair (Random.int 0 screenWidth) (Random.int 0 screenHeight))
                model.seed
    in
    { model
        | bullets =
            if List.length model.bullets < 15 then
                model.bullets |> List.append [ createNewBullet (toFloat x) 10 ]

            else
                model.bullets
        , seed = newSeed
    }


nearEnough : Player -> Bullet -> Bool
nearEnough player bullet =
    (bullet.x - toFloat player.x) ^ 2 + (bullet.y - toFloat player.y) ^ 2 < 10 ^ 2


createNewBullet : Float -> Float -> Bullet
createNewBullet x y =
    { x = x
    , y = y
    , vx = 0.0
    , vy = 1.0
    }
