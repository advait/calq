import React, { useState } from 'react';
import Editor from 'react-simple-code-editor';

const Interpreter = require('../output/Interpreter');
const EditorPS = require('../output/Editor');
const Tokenizer = require('../output/Tokenizer');

export default function (props) {
  const [value, setValue] = useState(props.initialValue || "");

  const { highlight, results } = EditorPS.run(value);

  const setValueLS = (v) => {
    localStorage.setItem("value", v);
    setValue(v);
  };

  return (
    <div className="editor">
      <div className="editor-left">
        <Editor
          value={value}
          onValueChange={setValueLS}
          highlight={() => highlight}
        />
      </div>
      <div className="editor-right">
        {results.map(res => (<div>{res || '\u00A0'}</div>))}
      </div>
    </div>
  )
};
