import React, { useState } from 'react';
import Editor from 'react-simple-code-editor';

const Interpreter = require('../output/Interpreter');
const Tokenizer = require('../output/Tokenizer');

export default function (props) {
  const [value, setValue] = useState(props.initialValue || "");
  const results = Interpreter.evalProgramShow(value);

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
          highlight={code => testHighlight(code)}
        />
      </div>
      <div className="editor-right">
        {results.map(res => (<div>{res}</div>))}
      </div>
    </div>
  )
};

function testHighlight(s) {
  // TODO(advait): Better error checking if tokenization fails
  return Tokenizer.highlight(s).value0;
}
