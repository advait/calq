"use strict";

var React = require("react");
var ContentEditable = require("react-contenteditable").default;

exports.contentEditableInternal = function (props) {
  // var onChangeOld = props.onChange;
  // props.onChange = function (e) {
  //   onChangeOld(e.target.value);
  // }
  return React.createElement(ContentEditable, props);
};
