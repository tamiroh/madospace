const KEY = "madospace:v1";

let saved = null;
try {
  saved = JSON.parse(localStorage.getItem(KEY));
} catch {
  // Start from an empty layout if it cannot be read
}

const app = Elm.Main.init({
  node: document.getElementById("root"),
  flags: {
    saved,
    width: window.innerWidth,
    height: window.innerHeight,
    https: location.protocol === "https:",
  },
});

app.ports.save.subscribe((state) => {
  try {
    localStorage.setItem(KEY, JSON.stringify(state));
  } catch {
    // Keep working even if it cannot be saved
  }
});

app.ports.toggleFullscreen.subscribe(() => {
  if (document.fullscreenElement) document.exitFullscreen();
  else document.documentElement.requestFullscreen();
});

// Clicks inside an iframe don't reach the page, so detect the focus move and raise the window
window.addEventListener("blur", () => {
  setTimeout(() => {
    const el = document.activeElement;
    if (el instanceof HTMLIFrameElement && el.dataset.win) app.ports.iframeFocused.send(el.dataset.win);
  });
});
