"use strict";

var fs = require("fs");

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

exports.undefinedLog = function (message) {
  throw new Error(message);
}

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

exports.readFileSync = function (path) {
  return fs.readFileSync(path, "utf-8");
};
