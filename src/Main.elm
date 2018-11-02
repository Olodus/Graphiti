import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)

-------------
-- Base types
-------------

type alias Point = {x : Float, y : Float}

type Line 
    = Linear {p1 : Point, p2 : Point}
    | Cubic  {p1 : Point, p2 : Point, p1t : Point, p2t : Point} 

--type alias AreaShape = {lines : List Line}

type alias PointSource = {p : Point, precision : Int, energy : Float}
type alias LineSink = {line : Line, energy : Float}

type Area 
    = Simple (List Line)
    | Meta PointSource (List LineSink)
--    | Meta (List PointSource) (List LineSource)

-----------------
-- Property types
-----------------

type alias PointProperties = {id : Int, staticMeshId : Int, rotation : Float}

type alias LineProperties = {id : Int, staticMeshId : Int, distanceBetween : Float}

type alias AreaProperties = {id : Int, materialId : Int, height : Float, foliage : Bool}

----------------
-- Element types
----------------

type alias Thing = {id : Int, p : Point, prop : PointProperties}

type alias Lineup = {id : Int, l : Line, prop : LineProperties}

type alias Surface = {id : Int, area : Area, prop : AreaProperties}

--------------
-- UI types
--------------

type Msg = First
         | Second
         | Third

type alias Model = {
    things   : (List Thing),
    lineups  : (List Lineup),
    surfaces : (List Surface)
    }

main = 
    Browser.sandbox { init = {things=[], lineups=[], surfaces=[]}, update = update, view = view }


-- Update

update : Msg -> Model -> Model
update msg model = 
    case msg of 
        First -> 
            { model | 
                things = List.singleton 
                    (Thing (List.length model.things) (Point 240.0 240.0) (PointProperties 0 1 2.0))
            }
        Second -> 
            { model | 
                lineups = List.singleton 
                    (Lineup 
                        (List.length model.lineups) 
                        (Linear {p1=(Point 250.0 250.0), p2=(Point 300.0 300.0)}) 
                        (LineProperties 9 1 2.0)
                    )
            }
        Third -> 
            { model | 
                surfaces = List.singleton 
                    (Surface 
                        (List.length model.surfaces) 
                        (Simple 
                            [
                                (Linear {p1=(Point 100.0 300.0), p2=(Point 300.0 100.0)}), 
                                (Linear {p1=(Point 300.0 100.0), p2=(Point 300.0 300.0)}), 
                                (Linear {p1=(Point 300.0 300.0), p2=(Point 100.0 300.0)})
                            ]
                        )
                        (AreaProperties 1 1 2.0 True)
                    )
            }


-- View

view : Model -> Html Msg
view model = 
    div []
        [ div [] [ Html.text "Proof of Concept" ]
        , svg
            [ width "500", height "500", viewBox "0 0 500 500" ]
            (renderAll model) 
        , button [ onClick First ] [ Html.text "First" ]
        , button [ onClick Second] [ Html.text "Second"]
        , button [ onClick Third ] [ Html.text "Third" ]
        ]


-- Render functions

renderAll : Model -> (List (Svg Msg))
renderAll model = List.concat 
    [
        (renderSurfaces model.surfaces),
        (renderLineUps model.lineups), 
        (renderThings model.things) 
    ]

renderThings : (List Thing) -> (List (Svg Msg))
renderThings things = List.map renderThing things 

renderThing : Thing -> (Svg Msg)
renderThing thing = 
    circle 
    [ 
        (cx (String.fromFloat thing.p.x)), 
        (cy (String.fromFloat thing.p.y)), 
        (r "5"), 
        (fill (getPointPropertyColor thing.prop.id))
    ] []

renderLineUps : (List Lineup) -> (List (Svg Msg))
renderLineUps lineups = List.map renderLineUp lineups

renderLineUp : Lineup -> (Svg Msg)
renderLineUp lineup =
    Svg.path [ (d (renderLine True lineup.l)), (stroke (getLinePropertyColor lineup.prop.id)) ] []

renderLine : Bool -> Line -> String
renderLine shouldWriteStart line = case line of
    (Linear points) -> 
        if shouldWriteStart 
        then 
            "M " ++ (String.fromFloat points.p1.x) ++ "," ++ 
            (String.fromFloat points.p1.y) ++ " " ++ 
            (String.fromFloat points.p2.x) ++ "," ++ 
            (String.fromFloat points.p2.y) ++ " "
        else 
            (String.fromFloat points.p1.x) ++ "," ++ 
            (String.fromFloat points.p1.y) ++ " " ++ 
            (String.fromFloat points.p2.x) ++ "," ++ 
            (String.fromFloat points.p2.y) ++ " "
    (Cubic _) -> "M 100,100 h 30 "

renderSurfaces : (List Surface) -> (List (Svg Msg))
renderSurfaces surfaces = List.map renderSurface surfaces

renderSurface : Surface -> (Svg Msg)
renderSurface surface =
    Svg.path [ 
        (d  (renderArea surface.area)), 
        (fill (getAreaPropertyColor surface.prop.id)) 
        ] []

renderArea : Area -> String
renderArea area = case area of
    (Simple lines) -> 
        ( "M " ++ (String.concat (List.map (renderLine False) lines)) ++ "z")
    (Meta source walls) ->
        ( "M " ++ (String.concat (List.map (renderLine False) 
            (createMetaBallLines source.p 
                (createMetaBallFunction source walls)
            ))) ++ "z")
    _ -> "M 200,200 300,200 300,300 200,300 z"

createMetaBallLines : PointSource -> (Float -> Float -> Float) -> (List Line)
createMetaBallLines source potentialFunc = 
    
createMetaBallFunction : PointSource -> (List LineSink) -> (Point -> Float)
createMetaBallFunction source sinks =
    (\p -> 
        (sqrt ((source.x-p.x)^2-(source.y-p.y)^2)) - (foldl (+) (map (distFromLine source) sinks)) )

distFromLine : Point -> LineSink -> Float
distFromLine p l = let dist = (abs (
    in
        if dist < 0.01 
        then 100.0
        else 

parametrizied : Point -> Point -> (Float -> Point)
parametrizied p1 p2 = 

t : Int -> (Point -> Float) -> Point -> Point -> Point
t count func p1 p2 = case count of
    0 -> (parametrizied p1 p2) 0.5
    _ -> 
        let x = (parametrizied p1 p2) 0.5
        in 
            if (func x) > 0 
            then t (count-1) func x p2
            else t (count-1) func p1 x

-- Propoerty map functions

getPointPropertyColor : Int -> String
getPointPropertyColor id = 
    case id of
        0 -> "blue"
        _ -> "black"

getLinePropertyColor : Int -> String
getLinePropertyColor id = 
    case id of
        0 -> "blue"
        1 -> "red"
        _ -> "black"

getAreaPropertyColor : Int -> String
getAreaPropertyColor id = 
    case id of
        0 -> "blue"
        1 -> "red"
        _ -> "black"
