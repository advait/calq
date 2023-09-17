import { extendTheme } from "@chakra-ui/react";

export const theme = extendTheme({
  fonts: {
    body: "'Roboto', sans-serif",
    mono: "'Roboto Mono', 'Menlo', 'Monaco', 'DeJaVu Sans Mono', monospace",
  },
  styles: {
    global: {
      body: {
        bgColor: "gray.100",
        color: "tokens.foreground",
      },
      ".mono": {
        fontFamily: "mono",
        fontWeight: "normal",
        fontSize: "lg",
        height: "1lh",
      },
    },
  },
  colors: {
    row: {
      bg: {
        normal: "tokens.background",
        hover: "var(--chakra-colors-gray-300)",
        selected: "var(--chakra-colors-gray-300)",
      },
    },
    tokens: {
      foreground: "#4d4d4c",
      background: "#ffffff",
      selection: "#d6d6d6",
      line: "#efefef",
      comment: "#8e908c",
      red: "#c82829",
      orange: "#f5871f",
      yellow: "#eab700",
      green: "#718c00",
      aqua: "#3e999f",
      blue: "#4271ae",
      purple: "#8959a8",
      window: "#efefef",
    },
  },
});
