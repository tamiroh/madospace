// Normalize user-entered URLs so they can be shown in an iframe

const YT_ID = /^[\w-]{11}$/;

function youtubeId(u: URL): string | null {
  const host = u.hostname.replace(/^(www|m)\./, "");
  if (host === "youtu.be") {
    const id = u.pathname.slice(1);
    return YT_ID.test(id) ? id : null;
  }
  if (host !== "youtube.com" && host !== "youtube-nocookie.com") return null;
  const v = u.searchParams.get("v");
  if (v && YT_ID.test(v)) return v;
  const m = u.pathname.match(/^\/(?:live|shorts|embed)\/([\w-]{11})/);
  return m ? m[1] : null;
}

export function normalizeUrl(input: string): string {
  let s = input.trim();
  if (!s) return "";
  if (!/^[a-z][a-z0-9+.-]*:/i.test(s)) s = `https://${s}`;

  let u: URL;
  try {
    u = new URL(s);
  } catch {
    return s;
  }

  // Regular YouTube pages refuse to be framed, so convert them to the embed form.
  // Browsers only allow autoplay when muted, hence mute=1.
  if (!u.pathname.startsWith("/embed/")) {
    const id = youtubeId(u);
    if (id) return `https://www.youtube.com/embed/${id}?autoplay=1&mute=1`;
  }
  return u.toString();
}

export function titleFromUrl(url: string): string {
  try {
    const u = new URL(url);
    if (u.hostname.includes("youtube")) return "YouTube";
    return u.hostname.replace(/^www\./, "");
  } catch {
    return url || "New window";
  }
}

// HTTP pages framed inside an HTTPS page are blocked as mixed content
export function isMixedContent(url: string): boolean {
  return location.protocol === "https:" && url.startsWith("http:");
}
