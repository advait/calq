/**
 * localStorage that never throws: it can be blocked (privacy settings) or full, and calq should
 * then still work for the session, just without remembering anything.
 */
export function load(key: string): string | null {
  try {
    return localStorage.getItem(key);
  } catch {
    return null;
  }
}

export function save(key: string, value: string) {
  try {
    localStorage.setItem(key, value);
  } catch {
    // Not saved; the session goes on.
  }
}
