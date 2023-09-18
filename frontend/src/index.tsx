import React, { useState } from "react";
import { createRoot } from "react-dom/client";
import {
  Button,
  Box,
  ChakraProvider,
  Container,
  Heading,
  HStack,
  IconButton,
  Link,
  useToast,
  Spacer,
} from "@chakra-ui/react";
import { LiaGithub, LiaShareSquare, LiaTrashAltSolid } from "react-icons/lia";
import { theme } from "./styles";
import qs from "qs";

import { EDITOR_WIDTH, Editor } from "./editor";

const MainAppBar = (props) => (
  <Box bg="navbar.bg" mb={2}>
    <Container
      bg="row.normal"
      pt={2}
      pb={2}
      pl={0}
      pr={0}
      minWidth={EDITOR_WIDTH}
    >
      <HStack>
        <Button leftIcon={<LiaTrashAltSolid />} onClick={props.onClear} mr={1}>
          Clear
        </Button>
        <Button leftIcon={<LiaShareSquare />} onClick={props.onShare}>
          Share
        </Button>
        <Spacer />
        <Heading as="h1" size="l" mr={1}>
          calq
        </Heading>
        <Link href="https://github.com/advait/calq" target="_blank">
          <IconButton aria-label="github">
            <LiaGithub size="2em" />
          </IconButton>
        </Link>
      </HStack>
    </Container>
  </Box>
);

const initialValue = (() => {
  const hash = (window.location.hash || "#").replace(/^#/, "");
  const urlValue = qs.parse(hash).value as string;
  history.pushState(null, "", "#"); // Clear hash so users don't re-share stale URL
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
    });
  };
  if (initialValue.fromHash) {
    toast({
      title: "Loaded from URL",
      status: "success",
      duration: 4000,
      isClosable: true,
    });
  }

  return (
    <div className="primary">
      <ChakraProvider theme={theme}>
        <MainAppBar onClear={onClear} onShare={onShare} />
        <Editor value={value} setValue={setValue} />
      </ChakraProvider>
    </div>
  );
};

const container = document.getElementById("react-container");
if (!container) {
  throw new Error("Unable to find root element");
}
const root = createRoot(container);
root.render(<MainApp />);
