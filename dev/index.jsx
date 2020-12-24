import React from 'react';
import ReactDOM from 'react-dom';

import createReactClass from 'create-react-class';
React.createClass = createReactClass;
var App = require('../output/Editor');

function main() {
  const mainComponent = (
    <div className="primary">
      <div className="container">
        <App.counter initialValue="1 m/s * 3 years" />
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