import React from 'react';
import ReactDOM from 'react-dom';
import Editor from './editor';

function main() {
  const initialValue = localStorage.getItem("value") || "1 m/s * 3 years\n2 * 2";
  console.log(initialValue);

  const mainComponent = (
    <div className="primary">
      <div style={{ height: "2em" }} />
      <div className="container">
        <Editor initialText={initialValue} />
      </div>
    </div>
  );

  ReactDOM.render(mainComponent, document.getElementById('react-container'));
}

// HMR stuff
// For more info see: https://parceljs.org/hmr.html
if (module.hot) {
  module.hot.accept(function () {
    console.log('Reloaded, running main again');
    main();
  });
}

console.log('Starting app');
main();
