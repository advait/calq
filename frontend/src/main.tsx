import { StrictMode, useEffect, useRef, useState } from "react";
import { createRoot } from "react-dom/client";
import Editor from "./editor";
import { Icon } from "./icons";
import { SettingsButton, enginePlaces, useSettings } from "./settings";
import { readSharedValue, shareUrl } from "./share";
import { useToast } from "./toast";

const STORAGE_KEY = "value";
const DEFAULT_VALUE = "1 m/s * 3 years\n2 * 2";

/**
 * Reads a shared document from the URL hash, then drops the hash so a reload or a re-share
 * doesn't resurrect it. A shared document replaces the saved one only once it is edited.
 */
function takeSharedValue(): string | null {
  const shared = readSharedValue(window.location.hash);
  if (window.location.hash) {
    history.replaceState(null, "", window.location.pathname);
  }
  return shared;
}

/**
 * The editor treats only "\n" as a line break, but a browser textarea also breaks lines at "\r"
 * and other vertical whitespace, which would push its text out of line with the overlay.
 */
const normalize = (value: string) =>
  value.replace(/\r\n?|[\v\f\u0085\u2028\u2029]/g, "\n");

const initialShared = takeSharedValue();
const initialValue = normalize(
  initialShared ?? localStorage.getItem(STORAGE_KEY) ?? DEFAULT_VALUE,
);

function App() {
  const [value, setValue] = useState(initialValue);
  const [caret, setCaret] = useState<{ to: "start" | "end" }>({
    to: initialShared === null ? "end" : "start",
  });
  const [settings, updateSettings] = useSettings();
  const input = useRef<HTMLTextAreaElement>(null);
  const toast = useToast();
  const { show } = toast;

  useEffect(() => {
    if (initialShared !== null) show("Loaded from link");
    // A link pasted into an open tab only changes the hash.
    const onHashChange = () => {
      const shared = takeSharedValue();
      if (shared === null) return;
      setValue(normalize(shared));
      setCaret({ to: "start" });
      show("Loaded from link");
    };
    window.addEventListener("hashchange", onHashChange);
    return () => window.removeEventListener("hashchange", onHashChange);
  }, [show]);

  const onChange = (raw: string) => {
    const v = normalize(raw);
    localStorage.setItem(STORAGE_KEY, v);
    setValue(v);
  };

  const onClear = () => {
    const el = input.current;
    if (!el) return;
    el.focus();
    el.select();
    // Deleting through the browser keeps Clear on the undo stack (Ctrl+Z restores it).
    if (!document.execCommand("delete")) onChange("");
  };

  const copy = async (text: string, message: string) => {
    try {
      await navigator.clipboard.writeText(text);
      toast.show(message);
    } catch {
      toast.show("Couldn't access the clipboard");
    }
  };

  return (
    <>
      <header className="app-bar">
        <div className="app-bar-inner">
          <button type="button" className="button outlined" onClick={onClear}>
            <Icon name="clear" />
            Clear
          </button>
          <button
            type="button"
            className="button"
            onClick={() =>
              copy(shareUrl(window.location, value), "Copied link to clipboard")
            }
          >
            <Icon name="share" />
            Share
          </button>
          <span className="spacer" />
          <h1 className="title">calq</h1>
          <SettingsButton settings={settings} onChange={updateSettings} />
          <a
            className="icon-button"
            href="https://github.com/advait/calq"
            target="_blank"
            rel="noreferrer"
            aria-label="calq on GitHub"
          >
            <Icon name="github" />
          </a>
        </div>
      </header>
      <main className="main">
        <Editor
          inputRef={input}
          value={value}
          caret={caret}
          places={enginePlaces(settings)}
          onChange={onChange}
          onCopyResult={(r) => copy(r, `Copied ${r}`)}
          onShowError={show}
        />
      </main>
      {toast.element}
    </>
  );
}

createRoot(document.getElementById("root")!).render(
  <StrictMode>
    <App />
  </StrictMode>,
);
