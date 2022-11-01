"use strict";

import * as fs from "fs";

export const debugLog = function (a) {
  console.log(a);
  return a;
}

export const undefined_ = undefined;

export const undefinedLog = message => { throw new Error(message) };

export const definitionsFile = fs.readFileSync("src/Definitions.calq", "utf-8");

export const interpreterTestFile = fs.readFileSync("test/InterpreterTest.calq", "utf-8");
