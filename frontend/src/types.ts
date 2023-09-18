/**
 * Typescript equivalents for Purescript types.
 */

export type EvalResult = {
  empty?: string;
  success?: string;
  error?: string;
};

export type HighlightToken = {
  text: string;
  highlightType: string;
};
