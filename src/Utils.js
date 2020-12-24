"use strict";

exports.debugLogAlt = function (a) {
  console.log(a);
  return function (b) {
    return b;
  };
};
