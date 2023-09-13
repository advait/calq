import definitions from "@src/Definitions.calq?raw";
import interpreterTest from "@test/InterpreterTest.calq?raw";

export const debugLog = function (a) {
  console.log(a);
  return a;
};

export const undefined_ = undefined;

export const undefinedLog = (message) => {
  throw new Error(message);
};

export const definitionsFile = definitions;

export const interpreterTestFile = interpreterTest;
