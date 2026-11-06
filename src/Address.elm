module Address exposing (normalizeUrl, titleFromUrl)

{-| Normalize user-entered URLs so they can be shown in an iframe.
-}

import Url exposing (Url)


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
    case Url.fromString withScheme of
        Nothing ->
            withScheme

        Just url ->
            -- Regular YouTube pages refuse to be framed, so convert them to the embed form.
            -- Browsers only allow autoplay when muted, hence mute=1.
            case videoId url of
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


videoId : Url -> Maybe String
videoId url =
    let
        host =
            url.host |> stripPrefix "www." |> stripPrefix "m."

        segments =
            String.split "/" url.path |> List.filter ((/=) "")
    in
    if host == "youtu.be" then
        List.head segments |> Maybe.andThen valid

    else if host == "youtube.com" || host == "youtube-nocookie.com" then
        case segments of
            "embed" :: _ ->
                Nothing

            "live" :: id :: _ ->
                valid id

            "shorts" :: id :: _ ->
                valid id

            _ ->
                url.query
                    |> Maybe.andThen (queryParam "v")
                    |> Maybe.andThen valid

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


valid : String -> Maybe String
valid id =
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
