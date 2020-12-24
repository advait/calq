"use strict";

var React = require("react");
var ContentEditable = require("react-contenteditable").default;

exports.contentEditableInternal = function (props) {
  return React.createElement(ContentEditable, props);
};
