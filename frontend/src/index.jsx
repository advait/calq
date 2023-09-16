import React, { useState } from "react";
import { createRoot } from "react-dom/client";
import { Box, Button, ChakraProvider, Container, extendTheme, HStack, IconButton, Link, useToast, Spacer, Text } from '@chakra-ui/react'
import { EditIcon, ExternalLinkIcon } from '@chakra-ui/icons';
import * as qs from "qs";

import Editor from "./editor";

const theme = extendTheme({
  fonts: {
    body: "'Roboto', sans-serif",
    mono: "'Roboto Mono', 'Menlo', 'Monaco', 'DeJaVu Sans Mono', monospace",
  },
  styles: {
    global: {
      body: {
        bgColor: "gray.100"
      },
      ".mono": {
        fontFamily: "mono",
        fontWeight: "normal",
        fontSize: "lg",
        height: "1lh",
      },
    }
  },
  colors: {
    row: {
      bg: {
        normal: "var(--chakra-colors-gray-200)",
        selected: "var(--chakra-colors-gray-300)",
      }
    }
  }
});

const MainAppBar = (props) => (
  <Container bg="gray.100" pt={2} pb={2}>
    <HStack >
      <Button
        aria-label="clear"
        leftIcon={<EditIcon />}
        onClick={props.onClear}
        mr={1}
      >
        Clear
      </Button>
      <Button
        leftIcon={<ExternalLinkIcon />}
        onClick={props.onShare}>
        Share
      </Button>
      <Spacer />
      <Text variant="h6" mr={1}>
        calq
      </Text>
      <Link href="https://github.com/advait/calq" target="_blank">
        <IconButton aria-label="github">
          {/* <GithubIcon color="primary" /> */}
          GH
        </IconButton>
      </Link>
    </HStack>
  </Container>
);

const initialValue = (() => {
  const hash = (window.location.hash || "#").replace(/^#/, "");
  const urlValue = qs.parse(hash).value;
  history.pushState(null, null, "#"); // Clear hash so users don't re-share stale URL
  if (urlValue) {
    return { value: urlValue, fromHash: true };
  } else {
    return {
      value: localStorage.getItem("value") || "1 m/s * 3 years\n2 * 2",
      fromHash: false,
    };
  }
})();

const MainApp = () => {
  const toast = useToast();
  const [value, setValue_] = useState(initialValue.value);
  const onClear = () => setValue_("");
  const setValue = (v) => {
    localStorage.setItem("value", v);
    setValue_(v);
  };

  const onShare = () => {
    const loc = window.location;
    const hash = qs.stringify({ value: value });
    const url = loc.origin + loc.pathname + "#" + hash;
    navigator.clipboard.writeText(url);
    toast({
      title: "Copied link to clipboard",
      status: "success",
      duration: 4000,
      isClosable: true,
    })
  };
  if (initialValue.fromHash) {
    toast({
      title: "Loaded from URL",
      status: "success",
      duration: 4000,
      isClosable: true,
    })
  }

  return (
    <div className="primary">
      <ChakraProvider theme={theme}>
        <MainAppBar onClear={onClear} onShare={onShare} />
        <Container>
          <Editor value={value} setValue={setValue} />

        </Container>
      </ChakraProvider>
    </div>
  );
};

const container = document.getElementById("react-container");
const root = createRoot(container);
root.render(<MainApp />);
