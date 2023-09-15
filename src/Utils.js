export { default as definitionsFile } from "@src/Definitions.calq?raw";
export { default as interpreterTestFile } from "@test/InterpreterTest.calq?raw";

export const debugLog = function (a) {
  console.log(a);
  return a;
};

export const undefined_ = undefined;

export const undefinedLog = (message) => {
  throw new Error(message);
};
