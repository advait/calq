"use strict";

var React = require("react");
var ContentEditable = require("react-contenteditable").default;

exports.contentEditableInternal = function (props) {
  return React.createElement(ContentEditable, props);
};

exports.htmlToTextContent = function (html) {
  var div = document.createElement("div");
  div.innerHTML = html;
  console.log(html, div.textContent);
  return div.textContent;
};
