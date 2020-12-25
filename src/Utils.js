"use strict";

exports.debugLog = function (a) {
  console.log(a);
  return a;
}

exports.debugLogAlt = function (a) {
  console.log(a);
  return function (b) {
    return b;
  };
};

exports.bigNumberFormatFixed = function (n) {
  return function (b) {
    return b.toFormat(n);
  }
};

exports.bigNumberFixed = function (n) {
  return function (b) {
    var ret = b.toFixed(n);
    console.log(n, b.toString(), ret.toString());
    return b.toFixed(n);
  }
};
