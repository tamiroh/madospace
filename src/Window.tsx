import { useEffect, useRef, useState } from "react";
import type { Win } from "./store";
import { isMixedContent, normalizeUrl, titleFromUrl } from "./url";

type Props = {
  win: Win;
  locked: boolean;
  active: boolean;
  onChange: (patch: Partial<Win>) => void;
  onFocus: () => void;
  onClose: () => void;
};

type Edge = "n" | "s" | "e" | "w" | "ne" | "nw" | "se" | "sw";
const EDGES: Edge[] = ["n", "s", "e", "w", "ne", "nw", "se", "sw"];

const MIN_W = 160;
const MIN_H = 100;
const SNAP = 8;
const snap = (v: number) => Math.round(v / SNAP) * SNAP;

// Keep iframes from swallowing pointer events while dragging or resizing
function setInteracting(on: boolean) {
  document.body.classList.toggle("interacting", on);
}

export function Window({ win, locked, active, onChange, onFocus, onClose }: Props) {
  const [editing, setEditing] = useState(!win.url);
  const [draft, setDraft] = useState(win.url);
  const [reloadKey, setReloadKey] = useState(0);
  const inputRef = useRef<HTMLInputElement>(null);
  const frameRef = useRef<HTMLIFrameElement>(null);

  // Clicks inside an iframe don't reach the parent, so detect the focus move and raise the window
  useEffect(() => {
    const onBlur = () => {
      setTimeout(() => {
        if (document.activeElement === frameRef.current) onFocus();
      });
    };
    window.addEventListener("blur", onBlur);
    return () => window.removeEventListener("blur", onBlur);
  }, [onFocus]);

  useEffect(() => {
    if (editing) inputRef.current?.select();
  }, [editing]);

  function startDrag(e: React.PointerEvent, edge: Edge | null) {
    if (locked || e.button !== 0) return;
    if (win.maximized && edge) return;
    e.preventDefault();
    e.stopPropagation();
    onFocus();

    const sx = e.clientX;
    const sy = e.clientY;
    // Dragging a maximized window restores its size and keeps it under the cursor
    const base = win.maximized
      ? { x: sx - win.w / 2, y: sy - 16, w: win.w, h: win.h }
      : { x: win.x, y: win.y, w: win.w, h: win.h };
    if (win.maximized) onChange({ maximized: false, x: base.x, y: base.y });

    const target = e.currentTarget as HTMLElement;
    target.setPointerCapture(e.pointerId);
    setInteracting(true);

    const move = (ev: PointerEvent) => {
      const dx = ev.clientX - sx;
      const dy = ev.clientY - sy;
      if (!edge) {
        onChange({ x: snap(base.x + dx), y: Math.max(0, snap(base.y + dy)) });
        return;
      }
      let { x, y, w, h } = base;
      if (edge.includes("e")) w = Math.max(MIN_W, base.w + dx);
      if (edge.includes("s")) h = Math.max(MIN_H, base.h + dy);
      if (edge.includes("w")) {
        w = Math.max(MIN_W, base.w - dx);
        x = base.x + base.w - w;
      }
      if (edge.includes("n")) {
        h = Math.max(MIN_H, base.h - dy);
        y = base.y + base.h - h;
      }
      onChange({ x: snap(x), y: Math.max(0, snap(y)), w: snap(w), h: snap(h) });
    };
    const up = () => {
      target.removeEventListener("pointermove", move);
      target.removeEventListener("pointerup", up);
      target.removeEventListener("pointercancel", up);
      setInteracting(false);
    };
    target.addEventListener("pointermove", move);
    target.addEventListener("pointerup", up);
    target.addEventListener("pointercancel", up);
  }

  function commitUrl() {
    const url = normalizeUrl(draft);
    setDraft(url);
    setEditing(false);
    if (url && url !== win.url) onChange({ url, title: titleFromUrl(url) });
  }

  const style: React.CSSProperties = win.maximized
    ? { left: 0, top: 0, width: "100%", height: "100%", zIndex: win.z }
    : { left: win.x, top: win.y, width: win.w, height: win.h, zIndex: win.z };

  const zoom = win.zoom || 1;
  const mixed = isMixedContent(win.url);

  return (
    <div
      className={`win${active ? " active" : ""}${locked ? " locked" : ""}`}
      style={style}
      onPointerDown={onFocus}
    >
      {!locked && (
        <div
          className="titlebar"
          onPointerDown={(e) => startDrag(e, null)}
          onDoubleClick={() => onChange({ maximized: !win.maximized })}
        >
          {editing ? (
            <input
              ref={inputRef}
              className="url"
              value={draft}
              placeholder="Enter a URL and press Enter"
              onChange={(e) => setDraft(e.target.value)}
              onPointerDown={(e) => e.stopPropagation()}
              onKeyDown={(e) => {
                if (e.key === "Enter") commitUrl();
                if (e.key === "Escape") {
                  setDraft(win.url);
                  setEditing(false);
                }
              }}
              onBlur={commitUrl}
            />
          ) : (
            <span className="title" title={win.url}>
              {win.title}
            </span>
          )}
          <div className="buttons" onPointerDown={(e) => e.stopPropagation()}>
            <button title="Edit URL" onClick={() => setEditing(true)}>
              ✎
            </button>
            <button title="Zoom out" onClick={() => onChange({ zoom: Math.max(0.25, +(zoom - 0.1).toFixed(2)) })}>
              −
            </button>
            <button className="zoom" title="Reset zoom" onClick={() => onChange({ zoom: 1 })}>
              {Math.round(zoom * 100)}%
            </button>
            <button title="Zoom in" onClick={() => onChange({ zoom: Math.min(3, +(zoom + 0.1).toFixed(2)) })}>
              +
            </button>
            <button title="Reload" onClick={() => setReloadKey((k) => k + 1)}>
              ↻
            </button>
            <button title="Maximize" onClick={() => onChange({ maximized: !win.maximized })}>
              {win.maximized ? "❐" : "□"}
            </button>
            <button className="close" title="Close" onClick={onClose}>
              ×
            </button>
          </div>
        </div>
      )}
      <div className="body">
        {win.url ? (
          <iframe
            key={reloadKey}
            ref={frameRef}
            src={win.url}
            title={win.title}
            allow="autoplay; fullscreen; encrypted-media; picture-in-picture"
            allowFullScreen
            referrerPolicy="strict-origin-when-cross-origin"
            style={{
              width: `${100 / zoom}%`,
              height: `${100 / zoom}%`,
              transform: `scale(${zoom})`,
            }}
          />
        ) : (
          <div className="empty">Enter a URL</div>
        )}
        {mixed && (
          <div className="notice">
            HTTP pages are blocked inside an HTTPS page. Allow "Insecure content" for this site in your browser settings to show it.
          </div>
        )}
      </div>
      {!locked &&
        !win.maximized &&
        EDGES.map((edge) => (
          <div key={edge} className={`handle ${edge}`} onPointerDown={(e) => startDrag(e, edge)} />
        ))}
    </div>
  );
}
