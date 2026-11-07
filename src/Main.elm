port module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Attribute, Html, button, div, iframe, input, span, text)
import Html.Attributes as A
import Html.Events as E
import Html.Keyed as Keyed
import Json.Decode as D
import Json.Encode as Enc
import Task
import Url exposing (Url)



-- PORTS


port save : Enc.Value -> Cmd msg


port toggleFullscreen : () -> Cmd msg


port iframeFocused : (String -> msg) -> Sub msg



-- MODEL


{-| Windows are kept in insertion order, which is also their DOM order. Never
reorder this list: moving an iframe in the DOM reloads it. Stacking is
expressed only through `z`.
-}
type alias Model =
    { windows : List Win
    , locked : Bool
    , drag : Maybe Drag
    , editing : Maybe { id : WinId, draft : String }
    , reloads : Dict String Int -- keyed by `idString`, since Dict keys must be comparable
    , viewport : { width : Int, height : Int }
    , https : Bool
    }


{-| Identifies a window. Also used as its DOM key, so it never changes.
-}
type WinId
    = WinId String


idString : WinId -> String
idString (WinId id) =
    id


type alias Win =
    { id : WinId
    , title : String
    , url : String
    , x : Int
    , y : Int
    , w : Int
    , h : Int
    , z : Int
    , zoom : Float
    , maximized : Bool
    , noticeDismissed : Bool
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


type alias Drag =
    { id : WinId
    , edge : Maybe Edge
    , startX : Int
    , startY : Int
    , base : Rect
    }


type alias Saved =
    { windows : List Win, locked : Bool }


type alias Flags =
    { saved : Saved
    , width : Int
    , height : Int
    , https : Bool
    }


toolbarHeight : Int
toolbarHeight =
    40


init : D.Value -> ( Model, Cmd Msg )
init value =
    let
        flags =
            D.decodeValue flagsDecoder value
                |> Result.withDefault
                    { saved = { windows = [], locked = False }, width = 1280, height = 800, https = False }
    in
    ( { windows = flags.saved.windows
      , locked = flags.saved.locked
      , drag = Nothing
      , editing = Nothing
      , reloads = Dict.empty
      , viewport = { width = flags.width, height = flags.height }
      , https = flags.https
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = AddWindow
    | Close WinId
    | Raise WinId
    | StartDrag WinId (Maybe Edge) Int Int
    | DragMove Int Int
    | DragEnd
    | ToggleMaximize WinId
    | SetZoom WinId Float
    | Reload WinId
    | DismissNotice WinId
    | EditUrl WinId
    | Draft String
    | CommitUrl
    | CancelEdit
    | Tile
    | Fullscreen
    | SetLocked Bool
    | Resized Int Int
    | NoOp


{-| Run `update` and persist the layout whenever it changes outside of a drag.
-}
updateAndSave : Msg -> Model -> ( Model, Cmd Msg )
updateAndSave msg model =
    let
        ( next, cmd ) =
            update msg model

        changed =
            next.windows /= model.windows || next.locked /= model.locked || model.drag /= Nothing
    in
    if next.drag == Nothing && changed then
        ( next, Cmd.batch [ cmd, save (encodeSaved next) ] )

    else
        ( next, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddWindow ->
            let
                id =
                    freshId model.windows

                offset =
                    24 + modBy 10 (List.length model.windows) * 32

                win =
                    { id = id
                    , title = "New window"
                    , url = ""
                    , x = offset
                    , y = offset
                    , w = 640
                    , h = 400
                    , z = topZ model.windows + 1
                    , zoom = 1
                    , maximized = False
                    , noticeDismissed = False
                    }
            in
            ( { model | windows = model.windows ++ [ win ], editing = Just { id = id, draft = "" } }
            , focusUrlInput id
            )

        Close id ->
            ( { model
                | windows = List.filter (\w -> w.id /= id) model.windows
                , reloads = Dict.remove (idString id) model.reloads
              }
            , Cmd.none
            )

        Raise id ->
            ( { model | windows = raise id model.windows }, Cmd.none )

        StartDrag id edge x y ->
            case findWin id model.windows of
                Nothing ->
                    ( model, Cmd.none )

                Just win ->
                    if model.locked || (win.maximized && edge /= Nothing) then
                        ( model, Cmd.none )

                    else
                        let
                            -- Dragging a maximized window restores its size and keeps it under the cursor
                            base =
                                if win.maximized then
                                    { x = x - win.w // 2, y = max 0 (y - toolbarHeight - 14), w = win.w, h = win.h }

                                else
                                    { x = win.x, y = win.y, w = win.w, h = win.h }
                        in
                        ( { model
                            | windows =
                                model.windows
                                    |> raise id
                                    |> updateWin id (\w -> setRect base { w | maximized = False })
                            , drag = Just { id = id, edge = edge, startX = x, startY = y, base = base }
                          }
                        , Cmd.none
                        )

        DragMove x y ->
            case model.drag of
                Nothing ->
                    ( model, Cmd.none )

                Just d ->
                    let
                        r =
                            dragRect d.edge d.base (x - d.startX) (y - d.startY)
                    in
                    ( { model | windows = updateWin d.id (setRect r) model.windows }, Cmd.none )

        DragEnd ->
            ( { model | drag = Nothing }, Cmd.none )

        ToggleMaximize id ->
            ( { model | windows = updateWin id (\w -> { w | maximized = not w.maximized }) model.windows }, Cmd.none )

        SetZoom id zoom ->
            ( { model | windows = updateWin id (\w -> { w | zoom = clamp 0.25 3 (toFloat (round (zoom * 100)) / 100) }) model.windows }
            , Cmd.none
            )

        Reload id ->
            ( { model | reloads = Dict.update (idString id) (\n -> Just (Maybe.withDefault 0 n + 1)) model.reloads }, Cmd.none )

        DismissNotice id ->
            ( { model | windows = updateWin id (\w -> { w | noticeDismissed = True }) model.windows }, Cmd.none )

        EditUrl id ->
            case findWin id model.windows of
                Just win ->
                    ( { model | editing = Just { id = id, draft = win.url } }, focusUrlInput id )

                Nothing ->
                    ( model, Cmd.none )

        Draft s ->
            ( { model | editing = Maybe.map (\e -> { e | draft = s }) model.editing }, Cmd.none )

        CommitUrl ->
            case model.editing of
                Nothing ->
                    ( model, Cmd.none )

                Just e ->
                    let
                        url =
                            normalizeUrl e.draft

                        apply w =
                            if url /= "" && url /= w.url then
                                { w | url = url, title = titleFromUrl url, noticeDismissed = False }

                            else
                                w
                    in
                    ( { model | editing = Nothing, windows = updateWin e.id apply model.windows }, Cmd.none )

        CancelEdit ->
            ( { model | editing = Nothing }, Cmd.none )

        Tile ->
            ( { model | windows = tile (deskArea model) model.windows }, Cmd.none )

        Fullscreen ->
            ( model, toggleFullscreen () )

        SetLocked locked ->
            ( { model | locked = locked, editing = Nothing }, Cmd.none )

        Resized w h ->
            ( { model | viewport = { width = w, height = h } }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


focusUrlInput : WinId -> Cmd Msg
focusUrlInput id =
    Task.attempt (\_ -> NoOp) (Browser.Dom.focus (urlInputId id))


urlInputId : WinId -> String
urlInputId id =
    "url-" ++ idString id


deskArea : Model -> { width : Int, height : Int }
deskArea model =
    { width = model.viewport.width
    , height =
        model.viewport.height
            - (if model.locked then
                0

               else
                toolbarHeight
              )
    }



-- WINDOW LIST


findWin : WinId -> List Win -> Maybe Win
findWin id windows =
    List.filter (\w -> w.id == id) windows |> List.head


updateWin : WinId -> (Win -> Win) -> List Win -> List Win
updateWin id f =
    List.map
        (\w ->
            if w.id == id then
                f w

            else
                w
        )


topZ : List Win -> Int
topZ windows =
    List.map .z windows |> List.maximum |> Maybe.withDefault 0


{-| Bring a window to the front by raising its `z` (the list order stays the same).
-}
raise : WinId -> List Win -> List Win
raise id windows =
    let
        top =
            topZ windows
    in
    case findWin id windows of
        Just win ->
            if win.z == top then
                windows

            else
                updateWin id (\w -> { w | z = top + 1 }) windows

        Nothing ->
            windows


{-| Arrange windows in a grid whose cells are as close to square as possible,
keeping their current reading order (top to bottom, left to right).
-}
tile : { width : Int, height : Int } -> List Win -> List Win
tile area windows =
    let
        n =
            List.length windows
    in
    if n == 0 || area.width <= 0 || area.height <= 0 then
        windows

    else
        let
            cols =
                ceiling (sqrt (toFloat (n * area.width) / toFloat area.height))

            rows =
                ceiling (toFloat n / toFloat cols)

            cw =
                area.width // cols

            ch =
                area.height // rows

            order =
                windows
                    |> List.sortBy (\w -> ( w.y, w.x ))
                    |> List.indexedMap (\i w -> ( idString w.id, i ))
                    |> Dict.fromList

            place w =
                let
                    i =
                        Dict.get (idString w.id) order |> Maybe.withDefault 0
                in
                { w | maximized = False, x = modBy cols i * cw, y = (i // cols) * ch, w = cw, h = ch }
        in
        List.map place windows


freshId : List Win -> WinId
freshId windows =
    let
        go n =
            let
                candidate =
                    WinId ("w" ++ String.fromInt n)
            in
            if List.any (\w -> w.id == candidate) windows then
                go (n + 1)

            else
                candidate
    in
    go (List.length windows + 1)



-- GEOMETRY


setRect : Rect -> Win -> Win
setRect r win =
    { win | x = r.x, y = r.y, w = r.w, h = r.h }


edges : List Edge
edges =
    [ North, South, East, West, NorthEast, NorthWest, SouthEast, SouthWest ]


sides : Edge -> { n : Bool, s : Bool, e : Bool, w : Bool }
sides edge =
    case edge of
        North ->
            { n = True, s = False, e = False, w = False }

        South ->
            { n = False, s = True, e = False, w = False }

        East ->
            { n = False, s = False, e = True, w = False }

        West ->
            { n = False, s = False, e = False, w = True }

        NorthEast ->
            { n = True, s = False, e = True, w = False }

        NorthWest ->
            { n = True, s = False, e = False, w = True }

        SouthEast ->
            { n = False, s = True, e = True, w = False }

        SouthWest ->
            { n = False, s = True, e = False, w = True }


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
                side =
                    sides e

                right =
                    base.x + base.w

                bottom =
                    base.y + base.h

                -- Snap only the edge being dragged; the opposite edge stays put
                w =
                    if side.e then
                        max minWidth (snap (right + dx) - base.x)

                    else if side.w then
                        max minWidth (right - snap (base.x + dx))

                    else
                        base.w

                h =
                    if side.s then
                        max minHeight (snap (bottom + dy) - base.y)

                    else if side.n then
                        max minHeight (bottom - max 0 (snap (base.y + dy)))

                    else
                        base.h
            in
            { x =
                if side.w then
                    right - w

                else
                    base.x
            , y =
                if side.n then
                    bottom - h

                else
                    base.y
            , w = w
            , h = h
            }



-- URL


{-| Normalize a user-entered URL so it can be shown in an iframe.
-}
normalizeUrl : String -> String
normalizeUrl input =
    let
        trimmed =
            String.trim input

        withScheme =
            if trimmed == "" || String.contains "://" trimmed then
                trimmed

            else
                "https://" ++ trimmed
    in
    case Url.fromString withScheme |> Maybe.andThen youtubeVideoId of
        -- Regular YouTube pages refuse to be framed, so convert them to the embed form.
        -- Browsers only allow autoplay when muted, hence mute=1.
        Just id ->
            "https://www.youtube.com/embed/" ++ id ++ "?autoplay=1&mute=1"

        Nothing ->
            withScheme


titleFromUrl : String -> String
titleFromUrl raw =
    case Url.fromString raw of
        Just url ->
            if String.contains "youtube" url.host then
                "YouTube"

            else
                stripPrefix "www." url.host

        Nothing ->
            if raw == "" then
                "New window"

            else
                raw


youtubeVideoId : Url -> Maybe String
youtubeVideoId url =
    let
        host =
            url.host |> stripPrefix "www." |> stripPrefix "m."

        segments =
            String.split "/" url.path |> List.filter ((/=) "")
    in
    if host == "youtu.be" then
        List.head segments |> Maybe.andThen validVideoId

    else if host == "youtube.com" || host == "youtube-nocookie.com" then
        case segments of
            "embed" :: _ ->
                Nothing

            "live" :: id :: _ ->
                validVideoId id

            "shorts" :: id :: _ ->
                validVideoId id

            _ ->
                url.query
                    |> Maybe.andThen (queryParam "v")
                    |> Maybe.andThen validVideoId

    else
        Nothing


queryParam : String -> String -> Maybe String
queryParam key query =
    query
        |> String.split "&"
        |> List.filterMap
            (\pair ->
                case String.split "=" pair of
                    [ k, v ] ->
                        if k == key then
                            Just v

                        else
                            Nothing

                    _ ->
                        Nothing
            )
        |> List.head


validVideoId : String -> Maybe String
validVideoId id =
    if String.length id == 11 && String.all (\c -> Char.isAlphaNum c || c == '-' || c == '_') id then
        Just id

    else
        Nothing


stripPrefix : String -> String -> String
stripPrefix prefix s =
    if String.startsWith prefix s then
        String.dropLeft (String.length prefix) s

    else
        s



-- JSON


flagsDecoder : D.Decoder Flags
flagsDecoder =
    D.map4 Flags
        (D.field "saved" (D.oneOf [ savedDecoder, D.succeed { windows = [], locked = False } ]))
        (D.field "width" D.int)
        (D.field "height" D.int)
        (D.field "https" D.bool)


savedDecoder : D.Decoder Saved
savedDecoder =
    D.map2 Saved
        (D.field "windows" (D.list winDecoder))
        (D.oneOf [ D.field "locked" D.bool, D.succeed False ])


encodeSaved : Model -> Enc.Value
encodeSaved model =
    Enc.object
        [ ( "windows", Enc.list encodeWin model.windows )
        , ( "locked", Enc.bool model.locked )
        ]


encodeWin : Win -> Enc.Value
encodeWin win =
    Enc.object
        [ ( "id", Enc.string (idString win.id) )
        , ( "title", Enc.string win.title )
        , ( "url", Enc.string win.url )
        , ( "x", Enc.int win.x )
        , ( "y", Enc.int win.y )
        , ( "w", Enc.int win.w )
        , ( "h", Enc.int win.h )
        , ( "z", Enc.int win.z )
        , ( "zoom", Enc.float win.zoom )
        , ( "maximized", Enc.bool win.maximized )
        , ( "noticeDismissed", Enc.bool win.noticeDismissed )
        ]


winDecoder : D.Decoder Win
winDecoder =
    D.succeed Win
        |> andMap (D.field "id" (D.map WinId D.string))
        |> andMap (D.field "title" D.string)
        |> andMap (D.field "url" D.string)
        |> andMap (D.field "x" number)
        |> andMap (D.field "y" number)
        |> andMap (D.field "w" number)
        |> andMap (D.field "h" number)
        |> andMap (D.oneOf [ D.field "z" number, D.succeed 1 ])
        |> andMap (D.oneOf [ D.field "zoom" D.float, D.succeed 1 ])
        |> andMap (D.oneOf [ D.field "maximized" D.bool, D.succeed False ])
        |> andMap (D.oneOf [ D.field "noticeDismissed" D.bool, D.succeed False ])


andMap : D.Decoder a -> D.Decoder (a -> b) -> D.Decoder b
andMap =
    D.map2 (|>)


{-| Accept fractional numbers too (mouse coordinates can be fractional).
-}
number : D.Decoder Int
number =
    D.map round D.float



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize Resized
        , iframeFocused (WinId >> Raise)
        , case model.drag of
            Just _ ->
                Sub.batch
                    [ Browser.Events.onMouseMove
                        (D.map3
                            (\buttons x y ->
                                -- The button was released outside the page
                                if buttons == 0 then
                                    DragEnd

                                else
                                    DragMove x y
                            )
                            (D.field "buttons" D.int)
                            (D.field "clientX" number)
                            (D.field "clientY" number)
                        )
                    , Browser.Events.onMouseUp (D.succeed DragEnd)
                    ]

            Nothing ->
                Sub.none
        ]



-- VIEW


view : Model -> Html Msg
view model =
    let
        top =
            topZ model.windows
    in
    div [ A.class "app" ]
        [ if model.locked then
            text ""

          else
            viewToolbar model
        , Keyed.node "main"
            [ A.class "desk" ]
            (List.map (\w -> ( idString w.id, viewWindow model (w.z == top) w )) model.windows
                ++ (if List.isEmpty model.windows then
                        [ ( "hint", viewHint ) ]

                    else
                        []
                   )
            )
        , if model.locked then
            button [ A.class "unlock", A.title "Unlock", E.onClick (SetLocked False) ] [ text "🔓" ]

          else
            text ""
        , case model.drag of
            -- Cover the iframes so they cannot swallow mouse events during a drag
            Just d ->
                div [ A.class "drag-overlay", A.style "cursor" (cursor d.edge) ] []

            Nothing ->
                text ""
        ]


viewToolbar : Model -> Html Msg
viewToolbar model =
    Html.header [ A.class "toolbar", A.style "height" (px toolbarHeight) ]
        [ span [ A.class "logo" ] [ text "madospace" ]
        , button [ E.onClick AddWindow ] [ text "+ New window" ]
        , span [ A.class "spacer" ] []
        , button [ E.onClick Tile, A.disabled (List.isEmpty model.windows) ] [ text "Tile" ]
        , button [ E.onClick Fullscreen ] [ text "Fullscreen" ]
        , button [ E.onClick (SetLocked True), A.title "Hide the toolbar and title bars and freeze the layout" ] [ text "🔒 Lock" ]
        ]


viewHint : Html Msg
viewHint =
    div [ A.class "hint" ]
        [ text "Click \"+ New window\" and enter a URL to start arranging pages."
        , Html.br [] []
        , text "The layout is saved automatically."
        ]


viewWindow : Model -> Bool -> Win -> Html Msg
viewWindow model active win =
    let
        geometry =
            if win.maximized then
                [ A.style "left" "0", A.style "top" "0", A.style "width" "100%", A.style "height" "100%" ]

            else
                [ A.style "left" (px win.x), A.style "top" (px win.y), A.style "width" (px win.w), A.style "height" (px win.h) ]
    in
    div
        ([ A.classList [ ( "win", True ), ( "active", active ), ( "locked", model.locked ) ]
         , A.style "z-index" (String.fromInt win.z)
         , E.onMouseDown (Raise win.id)
         ]
            ++ geometry
        )
        -- Keep the body at a fixed child index so toggling the title bar never recreates the iframe
        [ if model.locked then
            text ""

          else
            viewTitlebar model win
        , viewBody model win
        , if model.locked || win.maximized then
            text ""

          else
            div [] (List.map (viewHandle win.id) edges)
        ]


viewTitlebar : Model -> Win -> Html Msg
viewTitlebar model win =
    let
        editingDraft =
            case model.editing of
                Just e ->
                    if e.id == win.id then
                        Just e.draft

                    else
                        Nothing

                Nothing ->
                    Nothing
    in
    div
        [ A.class "titlebar"
        , onLeftMouseDown (StartDrag win.id Nothing)
        , E.onDoubleClick (ToggleMaximize win.id)
        ]
        [ case editingDraft of
            Just draft ->
                input
                    [ A.id (urlInputId win.id)
                    , A.class "url"
                    , A.value draft
                    , A.placeholder "Enter a URL and press Enter"
                    , E.onInput Draft
                    , E.onBlur CommitUrl
                    , onKey
                    , stopMouseDown
                    ]
                    []

            Nothing ->
                span [ A.class "title", A.title win.url ] [ text win.title ]
        , div [ A.class "buttons", stopMouseDown ]
            [ iconButton "Edit URL" "✎" (EditUrl win.id)
            , iconButton "Zoom out" "−" (SetZoom win.id (win.zoom - 0.1))
            , button [ A.class "zoom", A.title "Reset zoom", E.onClick (SetZoom win.id 1) ]
                [ text (String.fromInt (round (win.zoom * 100)) ++ "%") ]
            , iconButton "Zoom in" "+" (SetZoom win.id (win.zoom + 0.1))
            , iconButton "Reload" "↻" (Reload win.id)
            , iconButton "Maximize"
                (if win.maximized then
                    "❐"

                 else
                    "□"
                )
                (ToggleMaximize win.id)
            , button [ A.class "close", A.title "Close", E.onClick (Close win.id) ] [ text "×" ]
            ]
        ]


viewBody : Model -> Win -> Html Msg
viewBody model win =
    let
        reloads =
            Dict.get (idString win.id) model.reloads |> Maybe.withDefault 0

        content =
            if win.url == "" then
                ( "empty", div [ A.class "empty" ] [ text "Enter a URL" ] )

            else
                -- A new key makes Elm create a fresh iframe, which reloads the page
                ( "frame-" ++ String.fromInt reloads
                , iframe
                    [ A.src win.url
                    , A.title win.title
                    , A.attribute "data-win" (idString win.id)
                    , A.attribute "allow" "autoplay; fullscreen; encrypted-media; picture-in-picture"
                    , A.attribute "allowfullscreen" ""
                    , A.attribute "referrerpolicy" "strict-origin-when-cross-origin"
                    , A.style "width" (String.fromFloat (100 / win.zoom) ++ "%")
                    , A.style "height" (String.fromFloat (100 / win.zoom) ++ "%")
                    , A.style "transform" ("scale(" ++ String.fromFloat win.zoom ++ ")")
                    ]
                    []
                )

        -- HTTP pages framed inside an HTTPS page are blocked as mixed content
        notice =
            if model.https && String.startsWith "http:" win.url && not win.noticeDismissed then
                [ ( "notice"
                  , div [ A.class "notice" ]
                        [ span [] [ text "HTTP pages are blocked inside an HTTPS page. Allow \"Insecure content\" for this site in your browser settings to show it." ]
                        , button [ A.title "Dismiss", E.onClick (DismissNotice win.id) ] [ text "×" ]
                        ]
                  )
                ]

            else
                []
    in
    Keyed.node "div" [ A.class "body" ] (content :: notice)


viewHandle : WinId -> Edge -> Html Msg
viewHandle id edge =
    div [ A.class ("handle " ++ edgeClass edge), onLeftMouseDown (StartDrag id (Just edge)) ] []


edgeClass : Edge -> String
edgeClass edge =
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


iconButton : String -> String -> Msg -> Html Msg
iconButton title label msg =
    button [ A.title title, E.onClick msg ] [ text label ]


px : Int -> String
px n =
    String.fromInt n ++ "px"


onLeftMouseDown : (Int -> Int -> Msg) -> Attribute Msg
onLeftMouseDown toMsg =
    E.custom "mousedown"
        (D.map3 (\b x y -> ( b, x, y ))
            (D.field "button" D.int)
            (D.field "clientX" number)
            (D.field "clientY" number)
            |> D.andThen
                (\( b, x, y ) ->
                    if b == 0 then
                        D.succeed { message = toMsg x y, stopPropagation = True, preventDefault = True }

                    else
                        D.fail "not the left button"
                )
        )


stopMouseDown : Attribute Msg
stopMouseDown =
    E.stopPropagationOn "mousedown" (D.succeed ( NoOp, True ))


onKey : Attribute Msg
onKey =
    E.on "keydown"
        (D.field "key" D.string
            |> D.andThen
                (\key ->
                    case key of
                        "Enter" ->
                            D.succeed CommitUrl

                        "Escape" ->
                            D.succeed CancelEdit

                        _ ->
                            D.fail "ignored"
                )
        )



-- MAIN


main : Program D.Value Model Msg
main =
    Browser.element
        { init = init
        , update = updateAndSave
        , subscriptions = subscriptions
        , view = view
        }
