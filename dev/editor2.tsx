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
    setFocus({ line: null, offset: null });
  };

  const onEnter = (i, selectionStart) => {
    console.log("Enter", i, selectionStart);
    const first = lines[i].substring(0, selectionStart);
    const second = lines[i].substring(selectionStart);
    const newLines = [...lines];
    newLines.splice(i, 1, first, second);
    setLines(newLines);
    setFocus({ line: i + 1, offset: 0 });
  };

  return (
    <div>
      {lines.map((line, i) => (
        <Line
          i={i}
          onValueChanged={onValueChanged}
          line={line}
          focusOffset={focus.line === i ? focus.offset : null}
          onEnter={onEnter} />
      ))}
    </div>
  );
}

const KEY_ENTER = 13;

function Line(props) {
  let textarea = null;

  const taRef = useCallback(node => {
    console.log("cb", props.i, node, node && node.selectionStart, node && node.selectionEnd);
    textarea = node;
    if (!textarea || props.focusOffset === null) {
      return;
    }
    textarea.setSelectionRange(props.focusOffset, props.focusOffset);
    textarea.focus();
  });

  const onKeyPress = (e) => {
    console.log(e.charCode);
    const selectionStart = (textarea && textarea.selectionStart) || 0;
    if (e.charCode == KEY_ENTER) {
      props.onEnter(props.i, selectionStart);
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
        onKeyPress={onKeyPress} />
      <div ariea-hidden="true" className="highlight">{props.line}</div>
    </div>
  );
}
