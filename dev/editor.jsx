import React, { useRef, useState } from 'react';
import Editor from 'react-simple-code-editor';

const Interpreter = require('../output/Interpreter');
const EditorPS = require('../output/Editor');
const Tokenizer = require('../output/Tokenizer');

export default function (props) {
  const editorRef = useRef(null);
  const [value, setValue] = useState(props.initialValue || "");

  const fontSizePx = props.fontSizePx || 16;
  const lineHeight = props.lineHeight || 4.5;
  const lineHeightPx = fontSizePx * lineHeight;

  const { highlight, results } = EditorPS.run(value);

  const setValueLS = (v) => {
    localStorage.setItem("value", v);
    setValue(v);
  };

  return (
    <div>
      <div style={{height: lineHeightPx + "px"}}></div>
      <div className="editor" style={{fontSize: fontSizePx + "px"}}>
        <div className="editor-left" ref={editorRef}>
          <Editor
            value={value}
            onValueChange={setValueLS}
            highlight={() => highlight}
            style={{lineHeight: lineHeight}}
          />
        </div>
        {results.map((res, i) => {
          const offset = i * lineHeightPx + lineHeightPx * .8;
          const style = {top: offset + "px", left: "2em"};
          return (<div className="result" style={style}>{res || '\u00A0'}</div>);
        })}
      </div>
    </div>
  )
};
