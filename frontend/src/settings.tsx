import { useEffect, useId, useRef, useState } from "react";
import * as EditorPS from "@purs-compiled/Editor";
import { Icon } from "./icons";

export type Theme = "system" | "light" | "dark";

export type Settings = {
  /** The most decimal places an answer shows, or "all" for every digit calq has. */
  places: number | "all";
  theme: Theme;
};

const STORAGE_KEY = "settings";
const DEFAULTS: Settings = { places: 2, theme: "dark" };
const PLACES = [0, 1, 2, 3, 4, 5, 6, 8, 10, "all"] as const;
const THEMES: { value: Theme; label: string }[] = [
  { value: "system", label: "System" },
  { value: "light", label: "Light" },
  { value: "dark", label: "Dark" },
];

/** decimal.js keeps 20 significant digits, so 20 places shows all of them. */
export const enginePlaces = (s: Settings) =>
  s.places === "all" ? 20 : s.places;

function load(): Settings {
  try {
    const saved = JSON.parse(localStorage.getItem(STORAGE_KEY) ?? "{}");
    return {
      places: PLACES.includes(saved.places) ? saved.places : DEFAULTS.places,
      theme: THEMES.some((t) => t.value === saved.theme)
        ? saved.theme
        : DEFAULTS.theme,
    };
  } catch {
    return DEFAULTS;
  }
}

const prefersLight = () =>
  window.matchMedia("(prefers-color-scheme: light)").matches;

/**
 * Sets the theme on <html>. index.html runs the same logic inline before first paint, so a
 * light theme never flashes dark.
 */
function applyTheme(theme: Theme) {
  const resolved =
    theme === "system" ? (prefersLight() ? "light" : "dark") : theme;
  document.documentElement.dataset.theme = resolved;
  const surface = getComputedStyle(document.documentElement)
    .getPropertyValue("--surface")
    .trim();
  document
    .querySelector('meta[name="theme-color"]')
    ?.setAttribute("content", surface);
}

/** The user's settings, saved in this browser. */
export function useSettings() {
  const [settings, setSettings] = useState(load);

  useEffect(() => {
    applyTheme(settings.theme);
    if (settings.theme !== "system") return;
    const query = window.matchMedia("(prefers-color-scheme: light)");
    const onChange = () => applyTheme("system");
    query.addEventListener("change", onChange);
    return () => query.removeEventListener("change", onChange);
  }, [settings.theme]);

  const update = (patch: Partial<Settings>) => {
    const next = { ...settings, ...patch };
    localStorage.setItem(STORAGE_KEY, JSON.stringify(next));
    setSettings(next);
  };
  return [settings, update] as const;
}

/** A gear button that opens the settings in a popover anchored below it. */
export function SettingsButton(props: {
  settings: Settings;
  onChange: (patch: Partial<Settings>) => void;
}) {
  const id = useId();
  const button = useRef<HTMLButtonElement>(null);
  const panel = useRef<HTMLDivElement>(null);

  // Native popovers handle Escape, outside clicks and focus order; we only place it.
  useEffect(() => {
    const el = panel.current;
    if (!el) return;
    // Below the app bar, flush with the right edge of its content.
    const place = () => {
      const bar = button.current!.closest(".app-bar-inner")!;
      const r = bar.getBoundingClientRect();
      const inset = parseFloat(getComputedStyle(bar).paddingRight);
      el.style.top = `${r.bottom + 8}px`;
      el.style.right = `${window.innerWidth - r.right + inset}px`;
    };
    const onToggle = (e: Event) => {
      if ((e as ToggleEvent).newState === "open") place();
    };
    el.addEventListener("beforetoggle", onToggle);
    window.addEventListener("resize", place);
    return () => {
      el.removeEventListener("beforetoggle", onToggle);
      window.removeEventListener("resize", place);
    };
  }, []);

  const example = (() => {
    const [answer] = EditorPS.run(enginePlaces(props.settings))([
      "1/7",
    ]).results;
    return "success" in answer ? answer.success : "";
  })();

  return (
    <>
      <button
        ref={button}
        type="button"
        className="icon-button"
        popoverTarget={id}
        aria-label="Settings"
      >
        <Icon name="settings" />
      </button>
      <div
        ref={panel}
        id={id}
        popover="auto"
        className="settings"
        role="dialog"
        aria-label="Settings"
      >
        <h2>Settings</h2>
        <label className="setting">
          <span className="setting-label">Decimal places</span>
          <select
            className="select"
            value={String(props.settings.places)}
            onChange={(e) =>
              props.onChange({
                places:
                  e.target.value === "all" ? "all" : Number(e.target.value),
              })
            }
          >
            {PLACES.map((p) => (
              <option key={p} value={String(p)}>
                {p === "all" ? "All digits" : p}
              </option>
            ))}
          </select>
          <span className="setting-example" aria-live="polite">
            1/7 = {example}
          </span>
        </label>
        <fieldset className="setting">
          <legend>Theme</legend>
          <div className="segmented">
            {THEMES.map((t) => (
              <label key={t.value}>
                <input
                  type="radio"
                  name={`${id}-theme`}
                  value={t.value}
                  checked={props.settings.theme === t.value}
                  onChange={() => props.onChange({ theme: t.value })}
                />
                {t.label}
              </label>
            ))}
          </div>
        </fieldset>
      </div>
    </>
  );
}
