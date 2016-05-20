module Spaceship exposing (..)

import Element exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Window
import Time
import Html exposing (Html)
import Html.App as Html
import Task
import Mouse
import AnimationFrame exposing (diffs)


-- MODEL


type alias Vector2 =
    { x : Float, y : Float }


zero2 : Vector2
zero2 =
    { x = 0, y = 0 }


type alias GameObject a =
    { a | position : Vector2, speed : Vector2 }


type alias Shot =
    GameObject { lifetime : Float }


type alias Ship =
    GameObject { health : Int }


type alias Model =
    { player : Ship
    , shots : List Shot
    , windowSize : ( Int, Int )
    }


initialShip : Model
initialShip =
    { player = { position = zero2, speed = zero2, health = 100 }
    , shots = []
    , windowSize = ( 400, 400 )
    }



-- VIEW


view : Model -> Html msg
view model =
    let
        ( w, h ) =
            model.windowSize

        windowSize' =
            { x = toFloat w, y = toFloat h }

        drawShot' =
            drawShot windowSize'
    in
        collage w
            h
            (List.concat
                [ [ drawGame windowSize' ]
                , (List.map drawShot' model.shots)
                , [ drawShip windowSize' model.player
                  , toForm (show model)
                  ]
                ]
            )
            |> toHtml


drawGame : Vector2 -> Form
drawGame v =
    rect v.x v.y
        |> filled gray


drawShot : Vector2 -> Shot -> Form
drawShot size shot =
    circle 10
        |> filled blue
        |> move ( shot.position.x - (size.x / 2), (size.y / 2) - shot.position.y )


drawShip : Vector2 -> Ship -> Form
drawShip size ship =
    let
        shipColor =
            red
    in
        ngon 3 30
            |> filled shipColor
            |> rotate (degrees 90)
            |> move ( ship.position.x - (size.x / 2), (size.y / 2) - ship.position.y )



-- UPDATE


type Msg
    = Tick
    | WindowSizeChanged Window.Size
    | MousePosition { x : Int, y : Int }
    | Fire
    | Error ()


addVector2 : Vector2 -> Vector2 -> Vector2
addVector2 v1 v2 =
    { x = v1.x + v2.x, y = v1.y + v2.y }


animate : GameObject a -> GameObject a
animate o =
    { o | position = addVector2 o.position o.speed }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fire ->
            let
                shots =
                    model.shots

                newShot =
                    { position = model.player.position, speed = { x = 0, y = -1 }, lifetime = 100 }
            in
                ( { model | shots = newShot :: shots }, Cmd.none )

        MousePosition pos ->
            let
                player =
                    model.player

                position =
                    { x = toFloat pos.x, y = player.position.y }
            in
                ( { model | player = { player | position = position } }, Cmd.none )

        Tick ->
            ( { model | shots = List.map animate model.shots }, Cmd.none )

        WindowSizeChanged size ->
            let
                { width, height } =
                    size

                player =
                    model.player

                position =
                    { x = model.player.position.x, y = toFloat height - 30 }
            in
                ( { model
                    | windowSize = ( width, height )
                    , player = { player | position = position }
                  }
                , Cmd.none
                )

        Error _ ->
            ( model, Cmd.none )


handleSubs : Model -> Sub Msg
handleSubs model =
    Sub.batch
        [ diffs (\time -> Tick)
          --Time.every Time.second (always Tick)
        , Window.resizes WindowSizeChanged
        , Mouse.moves MousePosition
        , Mouse.clicks (\_ -> Fire)
        ]


main : Program Never
main =
    Html.program
        { init = ( initialShip, Task.perform Error WindowSizeChanged Window.size )
        , view = view
        , update = update
        , subscriptions = handleSubs
        }
