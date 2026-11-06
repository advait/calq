declare module "@purs-compiled/Editor" {
  export type Highlight = { highlightType: string; text: string };
  export type Answer =
    { empty: true } | { success: string } | { error: string };
  /** Highlights and answers for each line, rounded to at most `places` decimal places. */
  export function run(places: number): (lines: string[]) => {
    highlights: Highlight[][];
    results: Answer[];
  };
}
