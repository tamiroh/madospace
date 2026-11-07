# madospace

A single web page with its own window system, for keeping several live pages (live streams, dashboards, monitors, and so on) open side by side.

## Features

- Add, move, resize (snaps to an 8px grid), maximize, and close windows
- Show any URL in each window (YouTube `watch` / `youtu.be` / `live` / `shorts` URLs are converted to the embed form and autoplay muted)
- Per-window zoom (25–300%) and reload
- Tile layout and fullscreen
- Lock mode: hides the toolbar and title bars and freezes the layout (unlock with 🔓 in the bottom-right corner)
- Window positions and sizes are relative to the screen, so the layout keeps filling it in fullscreen, lock mode, or after resizing the browser
- The layout is saved to `localStorage` automatically

## Development

Written in [Elm](https://elm-lang.org/) with no other build tools. `npm` is only used to pin the Elm compiler.

```sh
npm install
npm run dev    # compiles to static/elm.js and starts elm reactor
```

Open <http://localhost:8000/static/index.html>. Re-run `npx elm make src/Main.elm --output=static/elm.js` and reload after editing.

`npm run build` writes the optimized site to `dist/`.

## Deployment

`.github/workflows/deploy.yml` builds and deploys to GitHub Pages. Set Settings → Pages → Source to "GitHub Actions", and every push to `main` is deployed. All asset paths are relative, so it works under any repository name.

## Limitations

- Pages are shown in iframes, so sites that forbid framing with `X-Frame-Options` or CSP `frame-ancestors` cannot be displayed.
- GitHub Pages is served over HTTPS, so HTTP-only pages are blocked as mixed content. Allow "Insecure content" for the site in your browser's site settings to show them.
