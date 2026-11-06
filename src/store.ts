export type Win = {
  id: string;
  title: string;
  url: string;
  x: number;
  y: number;
  w: number;
  h: number;
  z: number;
  zoom: number;
  maximized: boolean;
};

export type State = {
  windows: Win[];
  locked: boolean;
};

const KEY = "madospace:v1";

export function newId(): string {
  return Math.random().toString(36).slice(2, 10);
}

export function loadState(): State {
  try {
    const raw = localStorage.getItem(KEY);
    if (raw) {
      const s = JSON.parse(raw) as State;
      if (Array.isArray(s.windows)) return { windows: s.windows, locked: !!s.locked };
    }
  } catch {
    // Fall back to the initial state if it cannot be read
  }
  return { windows: [], locked: false };
}

export function saveState(s: State): void {
  try {
    localStorage.setItem(KEY, JSON.stringify(s));
  } catch {
    // Keep working even if it cannot be saved
  }
}
