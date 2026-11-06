port module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Html exposing (Attribute, Html, button, div, iframe, input, span, text)
import Html.Attributes as A
import Html.Events as E
import Html.Keyed as Keyed
import Json.Decode as D
import Json.Encode as Enc
import Task
import Window exposing (Edge(..), Rect, Win)
import Address



-- PORTS


port save : Enc.Value -> Cmd msg


port toggleFullscreen : () -> Cmd msg


port iframeFocused : (String -> msg) -> Sub msg



-- MODEL


type alias Model =
    { windows : List Win
    , locked : Bool
    , drag : Maybe Drag
    , editing : Maybe { id : String, draft : String }
    , viewport : { width : Int, height : Int }
    , https : Bool
    }


type alias Drag =
    { id : String
    , edge : Maybe Edge
    , startX : Int
    , startY : Int
    , base : Rect
    }


type alias Flags =
    { saved : { windows : List Win, locked : Bool }
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
      , viewport = { width = flags.width, height = flags.height }
      , https = flags.https
      }
    , Cmd.none
    )


flagsDecoder : D.Decoder Flags
flagsDecoder =
    D.map4 Flags
        (D.field "saved"
            (D.oneOf
                [ D.map2 (\ws l -> { windows = ws, locked = l })
                    (D.field "windows" (D.list Window.decoder))
                    (D.oneOf [ D.field "locked" D.bool, D.succeed False ])
                , D.succeed { windows = [], locked = False }
                ]
            )
        )
        (D.field "width" D.int)
        (D.field "height" D.int)
        (D.field "https" D.bool)


encodeState : Model -> Enc.Value
encodeState model =
    Enc.object
        [ ( "windows", Enc.list Window.encode model.windows )
        , ( "locked", Enc.bool model.locked )
        ]



-- UPDATE


type Msg
    = AddWindow
    | Close String
    | Raise String
    | StartDrag String (Maybe Edge) Int Int
    | DragMove Int Int
    | DragEnd
    | ToggleMaximize String
    | SetZoom String Float
    | Reload String
    | EditUrl String
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
        ( next, Cmd.batch [ cmd, save (encodeState next) ] )

    else
        ( next, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddWindow ->
            let
                n =
                    List.length model.windows

                offset =
                    24 + modBy 10 n * 32

                win =
                    Window.new (freshId model.windows) offset (topZ model.windows + 1)
            in
            ( { model | windows = model.windows ++ [ win ], editing = Just { id = win.id, draft = "" } }
            , focusUrlInput win.id
            )

        Close id ->
            ( { model | windows = List.filter (\w -> w.id /= id) model.windows }, Cmd.none )

        Raise id ->
            ( { model | windows = raise id model.windows }, Cmd.none )

        StartDrag id edge x y ->
            case find id model.windows of
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

                            restore w =
                                if w.maximized then
                                    { w | maximized = False, x = base.x, y = base.y }

                                else
                                    w
                        in
                        ( { model
                            | windows = raise id model.windows |> updateWin id restore
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
                        rect =
                            Window.dragRect d.edge d.base (x - d.startX) (y - d.startY)
                    in
                    ( { model | windows = updateWin d.id (\w -> { w | x = rect.x, y = rect.y, w = rect.w, h = rect.h }) model.windows }
                    , Cmd.none
                    )

        DragEnd ->
            ( { model | drag = Nothing }, Cmd.none )

        ToggleMaximize id ->
            ( { model | windows = updateWin id (\w -> { w | maximized = not w.maximized }) model.windows }, Cmd.none )

        SetZoom id zoom ->
            ( { model | windows = updateWin id (\w -> { w | zoom = clamp 0.25 3 (toFloat (round (zoom * 100)) / 100) }) model.windows }
            , Cmd.none
            )

        Reload id ->
            ( { model | windows = updateWin id (\w -> { w | reload = w.reload + 1 }) model.windows }, Cmd.none )

        EditUrl id ->
            case find id model.windows of
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
                            Address.normalizeUrl e.draft

                        apply w =
                            if url /= "" && url /= w.url then
                                { w | url = url, title = Address.titleFromUrl url }

                            else
                                w
                    in
                    ( { model | editing = Nothing, windows = updateWin e.id apply model.windows }, Cmd.none )

        CancelEdit ->
            ( { model | editing = Nothing }, Cmd.none )

        Tile ->
            ( { model | windows = tile (deskSize model) model.windows }, Cmd.none )

        Fullscreen ->
            ( model, toggleFullscreen () )

        SetLocked locked ->
            ( { model | locked = locked, editing = Nothing }, Cmd.none )

        Resized w h ->
            ( { model | viewport = { width = w, height = h } }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


focusUrlInput : String -> Cmd Msg
focusUrlInput id =
    Task.attempt (\_ -> NoOp) (Browser.Dom.focus (urlInputId id))


urlInputId : String -> String
urlInputId id =
    "url-" ++ id


freshId : List Win -> String
freshId windows =
    let
        go n =
            let
                candidate =
                    "w" ++ String.fromInt n
            in
            if List.any (\w -> w.id == candidate) windows then
                go (n + 1)

            else
                candidate
    in
    go (List.length windows + 1)


find : String -> List Win -> Maybe Win
find id =
    List.filter (\w -> w.id == id) >> List.head


updateWin : String -> (Win -> Win) -> List Win -> List Win
updateWin id f =
    List.map
        (\w ->
            if w.id == id then
                f w

            else
                w
        )


topZ : List Win -> Int
topZ =
    List.map .z >> List.maximum >> Maybe.withDefault 0


{-| Bring a window to the front. Only `z` changes, never the list order, so the
DOM order stays stable and iframes are not reloaded.
-}
raise : String -> List Win -> List Win
raise id windows =
    let
        top =
            topZ windows
    in
    case find id windows of
        Just w ->
            if w.z == top then
                windows

            else
                updateWin id (\x -> { x | z = top + 1 }) windows

        Nothing ->
            windows


deskSize : Model -> { width : Int, height : Int }
deskSize model =
    { width = model.viewport.width
    , height =
        model.viewport.height
            - (if model.locked then
                0

               else
                toolbarHeight
              )
    }


{-| Arrange windows in a grid whose cells are as close to square as possible.
-}
tile : { width : Int, height : Int } -> List Win -> List Win
tile desk windows =
    let
        n =
            List.length windows
    in
    if n == 0 then
        windows

    else
        let
            cols =
                ceiling (sqrt (toFloat (n * desk.width) / toFloat desk.height))

            rows =
                ceiling (toFloat n / toFloat cols)

            cw =
                desk.width // cols

            ch =
                desk.height // rows

            order =
                windows
                    |> List.sortBy (\w -> ( w.y, w.x ))
                    |> List.indexedMap (\i w -> ( w.id, i ))

            index w =
                order
                    |> List.filter (\( id, _ ) -> id == w.id)
                    |> List.head
                    |> Maybe.map Tuple.second
                    |> Maybe.withDefault 0

            place w =
                let
                    i =
                        index w
                in
                { w | maximized = False, x = modBy cols i * cw, y = (i // cols) * ch, w = cw, h = ch }
        in
        List.map place windows



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize Resized
        , iframeFocused Raise
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


number : D.Decoder Int
number =
    D.map round D.float



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
            (List.map (\w -> ( w.id, viewWindow model (w.z == top) w )) model.windows
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
                div [ A.class "drag-overlay", A.style "cursor" (Window.cursor d.edge) ] []

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
            div [] (List.map (viewHandle win.id) Window.edges)
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
        content =
            if win.url == "" then
                ( "empty", div [ A.class "empty" ] [ text "Enter a URL" ] )

            else
                ( "frame-" ++ String.fromInt win.reload
                , iframe
                    [ A.src win.url
                    , A.title win.title
                    , A.attribute "data-win" win.id
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
            if model.https && String.startsWith "http:" win.url then
                [ ( "notice"
                  , div [ A.class "notice" ]
                        [ text "HTTP pages are blocked inside an HTTPS page. Allow \"Insecure content\" for this site in your browser settings to show it." ]
                  )
                ]

            else
                []
    in
    Keyed.node "div" [ A.class "body" ] (content :: notice)


viewHandle : String -> Edge -> Html Msg
viewHandle id edge =
    div [ A.class ("handle " ++ Window.edgeName edge), onLeftMouseDown (StartDrag id (Just edge)) ] []


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
