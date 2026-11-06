/**
 * Share links carry the whole document in the URL hash as `#value=<encoded text>`. Links made by
 * earlier versions (encoded with qs, spaces as %20) and form-encoded links (spaces as +) both
 * decode here, so a link stays valid forever.
 */
export function readSharedValue(hash: string): string | null {
  return new URLSearchParams(hash.replace(/^#/, "")).get("value");
}

export function shareUrl(location: Location, value: string): string {
  return `${location.origin}${location.pathname}#value=${encodeURIComponent(value)}`;
}
