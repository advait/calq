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

exports.undefined = undefined;

exports.undefinedLog = function (message) {
  throw new Error(message);
}

exports.definitionsFile = fs.readFileSync("src/Definitions.calq", "utf-8");

exports.interpreterTestFile = fs.readFileSync("test/InterpreterTest.calq", "utf-8");
