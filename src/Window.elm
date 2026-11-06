module Window exposing (Edge(..), Rect, Win, cursor, decoder, dragRect, edgeName, edges, encode, new)

import Json.Decode as D
import Json.Encode as Enc


type alias Win =
    { id : String
    , title : String
    , url : String
    , x : Int
    , y : Int
    , w : Int
    , h : Int
    , z : Int
    , zoom : Float
    , maximized : Bool
    , reload : Int
    }


type alias Rect =
    { x : Int, y : Int, w : Int, h : Int }


type Edge
    = North
    | South
    | East
    | West
    | NorthEast
    | NorthWest
    | SouthEast
    | SouthWest


edges : List Edge
edges =
    [ North, South, East, West, NorthEast, NorthWest, SouthEast, SouthWest ]


edgeName : Edge -> String
edgeName edge =
    case edge of
        North ->
            "n"

        South ->
            "s"

        East ->
            "e"

        West ->
            "w"

        NorthEast ->
            "ne"

        NorthWest ->
            "nw"

        SouthEast ->
            "se"

        SouthWest ->
            "sw"


cursor : Maybe Edge -> String
cursor edge =
    case edge of
        Nothing ->
            "grabbing"

        Just North ->
            "ns-resize"

        Just South ->
            "ns-resize"

        Just East ->
            "ew-resize"

        Just West ->
            "ew-resize"

        Just NorthEast ->
            "nesw-resize"

        Just SouthWest ->
            "nesw-resize"

        Just NorthWest ->
            "nwse-resize"

        Just SouthEast ->
            "nwse-resize"


new : String -> Int -> Int -> Win
new id offset z =
    { id = id
    , title = "New window"
    , url = ""
    , x = offset
    , y = offset
    , w = 640
    , h = 400
    , z = z
    , zoom = 1
    , maximized = False
    , reload = 0
    }



-- GEOMETRY


minWidth : Int
minWidth =
    160


minHeight : Int
minHeight =
    100


snap : Int -> Int
snap v =
    round (toFloat v / 8) * 8


{-| Compute the new rectangle for a move (`Nothing`) or a resize from an edge,
given the rectangle at drag start and the pointer delta.
-}
dragRect : Maybe Edge -> Rect -> Int -> Int -> Rect
dragRect edge base dx dy =
    case edge of
        Nothing ->
            { base | x = snap (base.x + dx), y = max 0 (snap (base.y + dy)) }

        Just e ->
            let
                name =
                    edgeName e

                has c =
                    String.contains c name

                right =
                    base.x + base.w

                bottom =
                    base.y + base.h

                -- Snap only the edge being dragged; the opposite edge stays put
                w =
                    if has "e" then
                        max minWidth (snap (right + dx) - base.x)

                    else if has "w" then
                        max minWidth (right - snap (base.x + dx))

                    else
                        base.w

                h =
                    if has "s" then
                        max minHeight (snap (bottom + dy) - base.y)

                    else if has "n" then
                        max minHeight (bottom - max 0 (snap (base.y + dy)))

                    else
                        base.h
            in
            { x =
                if has "w" then
                    right - w

                else
                    base.x
            , y =
                if has "n" then
                    bottom - h

                else
                    base.y
            , w = w
            , h = h
            }



-- JSON


encode : Win -> Enc.Value
encode win =
    Enc.object
        [ ( "id", Enc.string win.id )
        , ( "title", Enc.string win.title )
        , ( "url", Enc.string win.url )
        , ( "x", Enc.int win.x )
        , ( "y", Enc.int win.y )
        , ( "w", Enc.int win.w )
        , ( "h", Enc.int win.h )
        , ( "z", Enc.int win.z )
        , ( "zoom", Enc.float win.zoom )
        , ( "maximized", Enc.bool win.maximized )
        ]


decoder : D.Decoder Win
decoder =
    D.succeed Win
        |> andMap (D.field "id" D.string)
        |> andMap (D.field "title" D.string)
        |> andMap (D.field "url" D.string)
        |> andMap (D.field "x" number)
        |> andMap (D.field "y" number)
        |> andMap (D.field "w" number)
        |> andMap (D.field "h" number)
        |> andMap (D.oneOf [ D.field "z" number, D.succeed 1 ])
        |> andMap (D.oneOf [ D.field "zoom" D.float, D.succeed 1 ])
        |> andMap (D.oneOf [ D.field "maximized" D.bool, D.succeed False ])
        |> andMap (D.succeed 0)


andMap : D.Decoder a -> D.Decoder (a -> b) -> D.Decoder b
andMap =
    D.map2 (|>)


number : D.Decoder Int
number =
    D.map round D.float
