"use strict";

exports.debugLog = function (a) {
  console.log(a);
  return a;
};

exports.debugLogAlt = function (a) {
  console.log(a);
  return function (b) {
    return b;
  };
};

exports.eventTargetTextContent = function (e) {
  console.log(e.target);
  return e.target.textContent;
};

exports.refireEvent = function (e) {
  setTimeout(function () {
    var newE = new InputEvent("delayedinput", e);
    e.target.dispatchEvent(newE);
  }, 0);
  return function (b) {
    return b;
  };
}