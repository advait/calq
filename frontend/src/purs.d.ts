declare module "@purs-compiled/Editor" {
  export type Highlight = { highlightType: string; text: string };
  export type Answer =
    { empty: true } | { success: string } | { error: string };
  export function run(lines: string[]): {
    highlights: Highlight[][];
    results: Answer[];
  };
}
