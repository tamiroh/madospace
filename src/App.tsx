import { useEffect, useRef, useState } from "react";
import { Window } from "./Window";
import { loadState, newId, saveState, type State, type Win } from "./store";
import { titleFromUrl } from "./url";

const TOOLBAR_H = 40;

export function App() {
  const [state, setState] = useState<State>(loadState);
  const deskRef = useRef<HTMLDivElement>(null);

  useEffect(() => saveState(state), [state]);

  const { windows, locked } = state;
  const topZ = windows.reduce((m, w) => Math.max(m, w.z), 0);
  const activeId = windows.find((w) => w.z === topZ)?.id;

  function update(id: string, patch: Partial<Win>) {
    setState((s) => ({ ...s, windows: s.windows.map((w) => (w.id === id ? { ...w, ...patch } : w)) }));
  }

  function focus(id: string) {
    setState((s) => {
      const max = s.windows.reduce((m, w) => Math.max(m, w.z), 0);
      const target = s.windows.find((w) => w.id === id);
      if (!target || target.z === max) return s;
      return { ...s, windows: s.windows.map((w) => (w.id === id ? { ...w, z: max + 1 } : w)) };
    });
  }

  function add(url = "", w = 640, h = 400) {
    const n = windows.length;
    const win: Win = {
      id: newId(),
      title: url ? titleFromUrl(url) : "New window",
      url,
      x: 24 + (n % 10) * 32,
      y: 24 + (n % 10) * 32,
      w,
      h,
      z: topZ + 1,
      zoom: 1,
      maximized: false,
    };
    setState((s) => ({ ...s, windows: [...s.windows, win] }));
  }

  function close(id: string) {
    setState((s) => ({ ...s, windows: s.windows.filter((w) => w.id !== id) }));
  }

  // Arrange windows in a grid whose cells are as close to square as possible
  function tile() {
    const desk = deskRef.current;
    if (!desk || windows.length === 0) return;
    const W = desk.clientWidth;
    const H = desk.clientHeight;
    const n = windows.length;
    const cols = Math.ceil(Math.sqrt((n * W) / H));
    const rows = Math.ceil(n / cols);
    const cw = Math.floor(W / cols);
    const ch = Math.floor(H / rows);
    const sorted = [...windows].sort((a, b) => a.y - b.y || a.x - b.x);
    const pos = new Map(sorted.map((w, i) => [w.id, i]));
    setState((s) => ({
      ...s,
      windows: s.windows.map((w) => {
        const i = pos.get(w.id)!;
        return { ...w, maximized: false, x: (i % cols) * cw, y: Math.floor(i / cols) * ch, w: cw, h: ch };
      }),
    }));
  }

  function toggleFullscreen() {
    if (document.fullscreenElement) document.exitFullscreen();
    else document.documentElement.requestFullscreen();
  }

  const setLocked = (v: boolean) => setState((s) => ({ ...s, locked: v }));

  return (
    <div className="app">
      {!locked && (
        <header className="toolbar" style={{ height: TOOLBAR_H }}>
          <span className="logo">madospace</span>
          <button onClick={() => add()}>+ New window</button>
          <span className="spacer" />
          <button onClick={tile} disabled={windows.length === 0}>
            Tile
          </button>
          <button onClick={toggleFullscreen}>Fullscreen</button>
          <button onClick={() => setLocked(true)} title="Hide the toolbar and title bars and freeze the layout">
            🔒 Lock
          </button>
        </header>
      )}
      <main className="desk" ref={deskRef}>
        {windows.map((w) => (
          <Window
            key={w.id}
            win={w}
            locked={locked}
            active={w.id === activeId}
            onChange={(p) => update(w.id, p)}
            onFocus={() => focus(w.id)}
            onClose={() => close(w.id)}
          />
        ))}
        {windows.length === 0 && (
          <div className="hint">
            Click "+ New window" and enter a URL to start arranging pages.
            <br />
            The layout is saved automatically.
          </div>
        )}
      </main>
      {locked && (
        <button className="unlock" onClick={() => setLocked(false)} title="Unlock">
          🔓
        </button>
      )}
    </div>
  );
}
