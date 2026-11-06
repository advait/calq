import { Fragment, useLayoutEffect, useMemo, useRef } from "react";
import type { MouseEvent, RefObject } from "react";
import * as EditorPS from "@purs-compiled/Editor";
import type { Answer, Highlight } from "@purs-compiled/Editor";
import { Icon } from "./icons";

type EditorProps = {
  inputRef: RefObject<HTMLTextAreaElement | null>;
  value: string;
  initialCaret: "start" | "end";
  onChange: (value: string) => void;
  onCopyResult: (result: string) => void;
  onShowError: (error: string) => void;
};

/**
 * One native textarea holds the whole document, so selection, undo, paste, IME and mobile
 * keyboards behave like any text field. Its text is transparent; a grid behind it renders each
 * line highlighted, with that line's result in a second column. The textarea spans every row of
 * the first column, so both wrap at the same width and stay aligned line for line.
 */
export default function Editor(props: EditorProps) {
  const textArea = props.inputRef;
  const lines = props.value.split("\n");
  const { highlights, results } = useMemo(
    () => EditorPS.run(props.value.split("\n")),
    [props.value],
  );

  // Resume a saved document at its end; read a shared one from the top.
  const grid = useRef<HTMLDivElement>(null);
  const initialCaret = useRef(props.initialCaret);
  useLayoutEffect(() => {
    const el = textArea.current;
    if (!el) return;
    el.focus({ preventScroll: true });
    if (initialCaret.current === "start") {
      el.setSelectionRange(0, 0);
      return;
    }
    el.setSelectionRange(el.value.length, el.value.length);
    grid.current?.lastElementChild?.scrollIntoView({ block: "nearest" });
  }, [textArea]);

  // Clicking the empty space below the document puts the caret at its end.
  const onMouseDown = (e: MouseEvent) => {
    const el = textArea.current;
    if (!el || e.target !== e.currentTarget) return;
    e.preventDefault();
    el.focus();
    el.setSelectionRange(el.value.length, el.value.length);
  };

  return (
    <div className="editor" onMouseDown={onMouseDown}>
      <div className="editor-grid" ref={grid}>
        <textarea
          ref={textArea}
          className="editor-input"
          style={{ gridRow: `1 / span ${lines.length}` }}
          value={props.value}
          onChange={(e) => props.onChange(e.target.value)}
          placeholder="Type a calculation…"
          aria-label="calq document"
          spellCheck={false}
          autoCapitalize="off"
          autoComplete="off"
          autoCorrect="off"
          rows={1}
        />
        {lines.map((line, i) => (
          <Fragment key={i}>
            <div
              className="editor-line"
              style={{ gridRow: i + 1 }}
              aria-hidden="true"
            >
              <Highlighted line={line} highlights={highlights[i]} />
            </div>
            <Result
              row={i + 1}
              answer={results[i]}
              onCopy={props.onCopyResult}
              onShowError={props.onShowError}
            />
          </Fragment>
        ))}
      </div>
    </div>
  );
}

function Highlighted(props: { line: string; highlights: Highlight[] }) {
  // The overlay must contain exactly the textarea's text or the two drift apart. Fall back to
  // plain text if the tokenizer ever renders a token differently from its source.
  const rendered = props.highlights.map((h) => h.text).join("");
  if (rendered !== props.line) return <>{props.line}</>;
  return (
    <>
      {props.highlights.map((h, i) => (
        <span key={i} className={h.highlightType}>
          {h.text}
        </span>
      ))}
    </>
  );
}

function Result(props: {
  row: number;
  answer: Answer;
  onCopy: (result: string) => void;
  onShowError: (error: string) => void;
}) {
  const style = { gridRow: props.row };
  if ("success" in props.answer) {
    const value = props.answer.success;
    return (
      <button
        type="button"
        className="result success"
        style={style}
        title={`${value} (click to copy)`}
        onClick={() => props.onCopy(value)}
      >
        <Icon name="copy" />
        <span className="result-text">{value}</span>
      </button>
    );
  }
  if ("error" in props.answer) {
    const error = props.answer.error;
    return (
      <button
        type="button"
        className="result error"
        style={style}
        title={error}
        onClick={() => props.onShowError(error)}
      >
        <span className="result-text">{error}</span>
      </button>
    );
  }
  return <div className="result" style={style} />;
}
