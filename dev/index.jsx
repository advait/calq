import React from 'react';
import ReactDOM from 'react-dom';

import createReactClass from 'create-react-class';
React.createClass = createReactClass;
var App = require('../output/Editor');

function main() {
  const myComponent = (
    <App.counter label="hello world" />
  );

  ReactDOM.render(myComponent, document.getElementById('react-container'));
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