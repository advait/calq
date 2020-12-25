import React from 'react';
import ReactDOM from 'react-dom';
import Editor from './editor';

function main() {
  const initialValue = localStorage.getItem("value") || "1 m/s * 3 years\n2 * 2";

  const mainComponent = (
    <div className="primary">
      <div className="container">
        <Editor initialValue={initialValue} />
      </div>
      <div className="background">
        <div className="columns">
          <div className="column editor-left is-two-thirds" />
          <div className="column editor-right" />
        </div>
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
