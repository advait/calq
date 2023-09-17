import React, {
  useState,
  useRef,
  useEffect,
  ChangeEvent,
  KeyboardEventHandler,
} from "react";
import { useToast, Box, Code, Textarea, Text } from "@chakra-ui/react";
import { CopyIcon } from "@chakra-ui/icons";
import * as EditorPS from "@purs-compiled/Editor";
import { monoSx } from "./styles";
import { EvalResult, HighlightToken } from "./types";

/**
 * Code editor that works by providing a transprent textarea and overlaying a <pre> which cointains
 * highlighted tokens.
 * Inspired by react-simple-code-editor and simplified for this use case.
 */
export const Editor: React.FC<{
  value: string;
  setValue: (string) => void;
}> = (props) => {
  const lines = props.value.split("\n");
  const [focus, setFocus] = useState<{
    line: number | null;
    offset: number | null;
  }>({
    line: lines.length - 1,
    offset: lines[lines.length - 1].length,
  });

  const setLines = (l: string[]) => {
    const newValue = l.join("\n");
    localStorage.setItem("value", newValue);
    props.setValue(newValue);
  };

  const {
    highlights,
    results,
  }: {
    highlights: HighlightToken[][];
    results: EvalResult[];
  } = EditorPS.run(lines);

  const onValueChanged =
    (i: number) => (e: ChangeEvent<HTMLTextAreaElement>) => {
      const newLines = [...lines];
      const inserted = e.target.value.split("\n");
      newLines.splice(i, 1, ...inserted);
      setLines(newLines);
      // When the editor value changes, each textarea will manage its own focus/selectionStart
      setFocus({ line: null, offset: null });
    };

  const onEnter =
    (i: number) => (selectionStart: number, selectionEnd: number) => {
      const first = lines[i].substring(0, selectionStart);
      const second = lines[i].substring(selectionEnd);
      const newLines = [...lines];
      newLines.splice(i, 1, first, second);
      setLines(newLines);
      setFocus({ line: i + 1, offset: 0 });
    };

  // When the backspace key is pressed at the cursor is at the beginning of the line
  const onBackspace0 = (i: number) => () => {
    if (i == 0) {
      return;
    }
    const newLines = [...lines];
    newLines[i - 1] = newLines[i - 1] + newLines[i];
    newLines.splice(i, 1);
    setLines(newLines);
    setFocus({ line: i - 1, offset: lines[i - 1].length });
  };

  // When either the up or down key is pressed
  const onUpDown = (i: number) => (up: boolean, selectionStart: number) => {
    let targetLine = up ? i - 1 : i + 1;
    targetLine = Math.max(Math.min(targetLine, lines.length - 1), 0);
    const offset = Math.min(lines[targetLine].length, selectionStart);
    setFocus({
      line: targetLine,
      offset,
    });
  };

  return (
    <Box sx={monoSx}>
      {lines.map((line, i) => (
        <Line
          key={`key${i}`}
          line={line}
          result={results[i]}
          focusOffset={focus.line === i ? focus.offset : null}
          highlightTokens={highlights[i]}
          showHint={lines.length === 1 && lines[0].length === 0}
          onValueChanged={onValueChanged(i)}
          onEnter={onEnter(i)}
          onBackspace0={onBackspace0(i)}
          onUpDown={onUpDown(i)}
        />
      ))}
    </Box>
  );
};

const KEY_ENTER = 13;
const KEY_BACKSPACE = 8;
const KEY_UP = 38;
const KEY_DOWN = 40;

const Line: React.FC<{
  line: string;
  focusOffset: number | null;
  result: EvalResult;
  showHint: boolean;
  highlightTokens: HighlightToken[];
  onValueChanged: (e: ChangeEvent<HTMLTextAreaElement>) => void;
  onEnter: (selectionStart: number, selectionEnd: number) => void;
  onBackspace0: () => void;
  onUpDown(up: boolean, selectionStart: number): void;
}> = (props) => {
  const toast = useToast();

  const result = (() => {
    if (
      props.result.empty ||
      (props.result.success !== undefined && props.result.error !== undefined)
    ) {
      return null;
    }
    return (
      <Box
        cursor="pointer"
        color="tokens.comment"
        sx={{
          "& .copy-icon": {
            opacity: 0,
            visibility: "hidden",
            transition: "visibility 0s, opacity 0s 0.4s",
          },
          "&:hover .copy-icon": {
            opacity: 1,
            visibility: "visible",
            animation: "fadein 0.4s ease-in-out 0.3s forwards",
          },
        }}
        onClick={() => {
          navigator.clipboard.writeText(props.result.success || "");
          toast({
            title: "Copied to clipboard",
            duration: 4000,
            isClosable: true,
          });
        }}
      >
        <Text as="span" ml={2} mr={2}>
          ⤷
        </Text>
        {props.result.success || props.result.error}
        <CopyIcon ml={2} className="copy-icon" />
      </Box>
    );
  })();

  return (
    <Box
      lineHeight={1}
      p={2}
      bgColor="row.bg.normal"
      sx={{
        "&:hover": {
          bgColor: "row.bg.hover",
        },
        "&:focus-within": {
          bgColor: "row.bg.selected",
        },
      }}
    >
      <HighlightedTextarea
        line={props.line}
        focusOffset={props.focusOffset}
        showHint={props.showHint}
        highlightTokens={props.highlightTokens}
        onValueChanged={props.onValueChanged}
        onEnter={props.onEnter}
        onBackspace0={props.onBackspace0}
        onUpDown={props.onUpDown}
      />
      {result}
    </Box>
  );
};

const HighlightedTextarea: React.FC<{
  line: string;
  focusOffset: number | null;
  showHint: boolean;
  highlightTokens: HighlightToken[];
  onValueChanged: (e: ChangeEvent<HTMLTextAreaElement>) => void;
  onEnter: (selectionStart: number, selectionEnd: number) => void;
  onBackspace0: () => void;
  onUpDown(up: boolean, selectionStart: number): void;
}> = (props) => {
  const textAreaRef = useRef<HTMLTextAreaElement>(null);

  const onKeyDown: KeyboardEventHandler<HTMLTextAreaElement> = (e) => {
    const selectionStart = textAreaRef.current?.selectionStart || 0;
    const selectionEnd = textAreaRef.current?.selectionEnd || 0;
    if (e.keyCode == KEY_ENTER) {
      props.onEnter(selectionStart, selectionEnd);
      e.preventDefault();
    } else if (
      e.keyCode == KEY_BACKSPACE &&
      selectionStart == 0 &&
      selectionEnd == 0
    ) {
      props.onBackspace0();
      e.preventDefault();
    } else if (e.keyCode == KEY_UP || e.keyCode == KEY_DOWN) {
      props.onUpDown(e.keyCode == KEY_UP, selectionStart);
      e.preventDefault();
    }
  };

  useEffect(() => {
    const textArea = textAreaRef.current;
    if (!textArea || props.focusOffset === null) {
      return;
    }
    textArea.setSelectionRange(props.focusOffset, props.focusOffset);
    textArea.focus();
  }, [props.focusOffset]);

  return (
    <Box
      position="relative"
      lineHeight={1}
      overflow="hidden"
      height="1lh"
      mb={2}
    >
      <Textarea
        onChange={(e) => props.onValueChanged(e)}
        value={props.line}
        ref={textAreaRef}
        onKeyDown={onKeyDown}
        rows={1}
        position="absolute"
        top={0}
        left={0}
        padding={0}
        width="100%"
        color="transparent"
        border="none"
        overflow="hidden"
        resize="none"
        outline="none"
        boxShadow="none"
        fontSize="lg"
        sx={{
          "caret-color": "black",
          "&:focus, &:focus-visible": {
            border: "none",
            outline: "none",
            boxShadow: "none",
            "+ .hint": {
              visibility: "hidden",
            },
          },
        }}
      />
      {props.showHint && (
        <Text className="hint" color="tokens.comment">
          Click to begin editing
        </Text>
      )}
      <Code
        as="pre"
        ariea-hidden="true"
        sx={monoSx}
        p={0}
        m={0}
        bg="transparent"
      >
        {props.highlightTokens.map((h, i) => (
          <HighlightTokenView {...h} key={`key${i}`} />
        ))}
      </Code>
    </Box>
  );
};

const HighlightTokenView: React.FC<HighlightToken & { key: string }> = ({
  text,
  highlightType,
  key,
}) => {
  const colorMap = {
    comment: "tokens.comment",
    number: "tokens.aqua",
    name: "tokens.blue",
    punctuation: "tokens.purple",
    reserved: "tokens.orange",
    infix: "tokens.foreground",
    unknown: "tokens.red",
  };

  return (
    <Text
      color={colorMap[highlightType] || "red.100"}
      as="span"
      lineHeight={1}
      margin={0}
      padding={0}
      top={0}
      left={0}
      key={key}
    >
      {text}
    </Text>
  );
};
