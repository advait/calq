import React, { useCallback, useRef, useState } from 'react';

type Editor2Props = {
  initialText: String
};

export default function Editor2(props: Editor2Props) {
  const initialLines: String[] = props.initialText.split("\n");
  const [lines, setLines] = React.useState(initialLines);
  const [focus, setFocus] = useState({ line: lines.length - 1, offset: lines[lines.length - 1].length })

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

  const onUpDown = (i: number, up: boolean, selectionStart: number) => {
    const targetLine = up ? i - 1 : i + 1;
    if (targetLine < 0 || targetLine >= lines.length) {
      return;
    }
    setFocus({ line: targetLine, offset: Math.min(lines[targetLine].length, selectionStart) });
  };

  return (
    <div>
      {lines.map((line, i) => (
        <Line
          i={i}
          onValueChanged={onValueChanged}
          line={line}
          focusOffset={focus.line === i ? focus.offset : null}
          onEnter={onEnter}
          onBackspace0={onBackspace0}
          onUpDown={onUpDown} />
      ))}
    </div>
  );
}

const KEY_ENTER = 13;
const KEY_BACKSPACE = 8;
const KEY_UP = 38;
const KEY_DOWN = 40;

function Line(props) {
  let textarea = null;

  const taRef = useCallback(node => {
    textarea = node;
    if (!textarea || props.focusOffset === null) {
      return;
    }
    textarea.setSelectionRange(props.focusOffset, props.focusOffset);
    textarea.focus();
  });

  const onKeyDown = (e) => {
    const selectionStart = (textarea && textarea.selectionStart) || 0;
    const selectionEnd = (textarea && textarea.selectionEnd) || 0;
    if (e.keyCode == KEY_ENTER) {
      props.onEnter(props.i, selectionStart, selectionEnd);
      e.preventDefault();
    } else if (e.keyCode == KEY_BACKSPACE && selectionStart == 0 && selectionEnd == 0) {
      props.onBackspace0(props.i);
      e.preventDefault();
    } else if (e.keyCode == KEY_UP || e.keyCode == KEY_DOWN) {
      props.onUpDown(props.i, e.keyCode == KEY_UP, selectionStart);
      e.preventDefault();
    }
  };

  return (
    <div className="editor2-container">
      <textarea
        onChange={e => props.onValueChanged(e, props.i)}
        value={props.line}
        ref={taRef}
        autoFocus={props.autoFocus}
        onKeyDown={onKeyDown} />
      <div ariea-hidden="true" className="highlight">{props.line}</div>
    </div>
  );
}
