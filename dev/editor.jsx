import React, { useState } from 'react';
import Editor from 'react-simple-code-editor';

const Interpreter = require('../output/Interpreter');

export default function (props) {
  const [value, setValue] = useState(props.initialValue || "");
  const results = Interpreter.evalProgramAllShow(value);

  const setValueLS = (v) => {
    localStorage.setItem("value", v);
    setValue(v);
  };

  return (
    <div className="editor columns">
      <div className="column editor-left is-two-thirds">
        <Editor
          value={value}
          onValueChange={setValueLS}
          highlight={code => code}
        />
      </div>
      <div className="column editor-right">
        {results.map(res => (<div>{res}</div>))}
      </div>
    </div>
  )
};
