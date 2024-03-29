import React, { useState, useRef, useEffect } from "react";
import ContentCopyIcon from "@mui/icons-material/ContentCopy";
import Snackbar from "@mui/material/Snackbar";
import * as EditorPS from "@purs-compiled/Editor";

type EditorProps = {
  value: string;
  setValue: (string) => void;
};

/**
 * Code editor that works by providing a transprent textarea and overlaying a <pre> which cointains
 * highlighted tokens.
 * Inspired by react-simple-code-editor and simplified for this use case.
 */
export default function Editor(props: EditorProps) {
  const lines = props.value.split("\n");
  const [showCopySnack, setShowCopySnack] = useState(false);
  const [focus, setFocus] = useState<{
    line: number | null;
    offset: number | null;
  }>({
    line: lines.length - 1,
    offset: lines[lines.length - 1].length,
  });

  const setLines = (l: string[]) => {
    const newValue = l.join("\n");
    localStorage.setItem("value", newValue);
    props.setValue(newValue);
  };

  const { highlights, results } = EditorPS.run(lines);

  const onValueChanged = (e, i) => {
    const newLines = [...lines];
    const inserted = e.target.value.split("\n");
    newLines.splice(i, 1, ...inserted);
    setLines(newLines);
    // When the editor value changes, each textarea will manage its own focus/selectionStart
    setFocus({ line: null, offset: null });
  };

  const onEnter = (i: number, selectionStart: number, selectionEnd: number) => {
    const first = lines[i].substring(0, selectionStart);
    const second = lines[i].substring(selectionEnd);
    const newLines = [...lines];
    newLines.splice(i, 1, first, second);
    setLines(newLines);
    setFocus({ line: i + 1, offset: 0 });
  };

  // When the backspace key is pressed at the cursor is at the beginning of the line
  const onBackspace0 = (i: number) => {
    if (i == 0) {
      return;
    }
    const newLines = [...lines];
    newLines[i - 1] = newLines[i - 1] + newLines[i];
    newLines.splice(i, 1);
    setLines(newLines);
    setFocus({ line: i - 1, offset: lines[i - 1].length });
  };

  // When either the up or down key is pressed
  const onUpDown = (i: number, up: boolean, selectionStart: number) => {
    let targetLine = up ? i - 1 : i + 1;
    targetLine = Math.max(Math.min(targetLine, lines.length - 1), 0);
    const offset = Math.min(lines[targetLine].length, selectionStart);
    setFocus({
      line: targetLine,
      offset,
    });
  };

  return (
    <div>
      {lines.map((line, i) => (
        <Line
          i={i}
          key={`key${i}`}
          onValueChanged={onValueChanged}
          line={line}
          result={results[i]}
          focusOffset={focus.line === i ? focus.offset : null}
          highlight={highlights[i]}
          showHint={lines.length === 1 && lines[0].length === 0}
          onEnter={onEnter}
          onBackspace0={onBackspace0}
          onUpDown={onUpDown}
          showCopySnack={() => setShowCopySnack(true)}
        />
      ))}
      <Snackbar
        open={showCopySnack}
        onClose={() => setShowCopySnack(false)}
        autoHideDuration={4000}
        message="Copied to clipboard"
      />
    </div>
  );
}

const KEY_ENTER = 13;
const KEY_BACKSPACE = 8;
const KEY_UP = 38;
const KEY_DOWN = 40;

function Line(props) {
  const textAreaRef = useRef<HTMLTextAreaElement>(null);

  useEffect(() => {
    const textArea = textAreaRef.current;
    if (!textArea || props.focusOffset === null) {
      return;
    }
    textArea.setSelectionRange(props.focusOffset, props.focusOffset);
    textArea.focus();
  }, [props.focusOffset]);

  const onKeyDown = (e) => {
    const selectionStart = textAreaRef.current?.selectionStart || 0;
    const selectionEnd = textAreaRef.current?.selectionEnd || 0;
    if (e.keyCode == KEY_ENTER) {
      props.onEnter(props.i, selectionStart, selectionEnd);
      e.preventDefault();
    } else if (
      e.keyCode == KEY_BACKSPACE &&
      selectionStart == 0 &&
      selectionEnd == 0
    ) {
      props.onBackspace0(props.i);
      e.preventDefault();
    } else if (e.keyCode == KEY_UP || e.keyCode == KEY_DOWN) {
      props.onUpDown(props.i, e.keyCode == KEY_UP, selectionStart);
      e.preventDefault();
    }
  };

  const result = (() => {
    if (props.result.empty) {
      return;
    } else if (props.result.success !== undefined) {
      return (
        <div
          className="result success"
          onClick={() => {
            navigator.clipboard.writeText(props.result.success);
            props.showCopySnack();
          }}
        >
          <span className="return-icon">⤷</span>
          {props.result.success}
          <ContentCopyIcon className="copy-icon" />
        </div>
      );
    } else if (props.result.error !== undefined) {
      return (
        <div className="result error">
          <span className="return-icon">⤷</span>
          {props.result.error}
        </div>
      );
    }
  })();

  return (
    <div className="editor-line">
      <div className="editable">
        <textarea
          onChange={(e) => props.onValueChanged(e, props.i)}
          value={props.line}
          ref={textAreaRef}
          onKeyDown={onKeyDown}
        />
        {!props.showHint ? null : (
          <span className="start-hint highlight">Click to begin editing</span>
        )}
        <pre ariea-hidden="true" className="highlight">
          {props.highlight.map((h, i) => (
            <span key={`key${i}`} className={h.highlightType}>
              {h.text}
            </span>
          ))}
        </pre>
      </div>
      {result}
    </div>
  );
}
