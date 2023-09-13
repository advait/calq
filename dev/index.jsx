import React, { useState } from 'react';
import ReactDOM from 'react-dom';
import * as qs from "qs";
import AppBar from '@mui/material/AppBar';
import Container from '@mui/material/Container';
import Toolbar from '@mui/material/Toolbar';
import Typography from '@mui/material/Typography';
import Button from '@mui/material/Button';
import CreateIcon from "@mui/icons-material/Create"
import ShareIcon from "@mui/icons-material/Share"
import Link from "@mui/material/Link";
import IconButton from "@mui/material/IconButton";
import GithubIcon from '@mui/icons-material/GitHub';
import { ThemeProvider, createTheme } from '@mui/material/styles'
import Snackbar from '@mui/material/Snackbar';

import Editor from './editor';

const theme = createTheme({
  palette: {
    mode: 'dark',
    primary: {
      main: "#c5c8c6",
      light: "#fff",
      dark: "#fff",
      contrastText: "#1D1F21",
    },
    text: {
      primary: "#c5c8c6",
    },
  },
});

const MainAppBar = (props) => (
  <AppBar position="static">
    <Container maxWidth="md">
      <Toolbar disableGutters={true}>
        <Button
          variant="outlined"
          startIcon={<CreateIcon />}
          onClick={props.onClear}
          sx={{ marginRight: "1em" }}>Clear</Button>
        <Button
          startIcon={<ShareIcon />}
          onClick={props.onShare} >Share</Button>
        <div style={{ flexGrow: 1 }} />
        <Typography variant="h6" sx={{ marginRight: "1em" }}>calq</Typography>
        <Link href="https://github.com/advait/calq" target="_blank">
          <IconButton aria-label="github"><GithubIcon color="primary" /></IconButton>
        </Link>
      </Toolbar>
    </Container>
  </AppBar>
)

const initialValue = (() => {
  const hash = (window.location.hash || "#").replace(/^#/, "");
  const urlValue = qs.parse(hash).value;
  history.pushState(null, null, "#"); // Clear hash so users don't re-share stale URL
  if (urlValue) {
    return { value: urlValue, fromHash: true };
  } else {
    return { value: localStorage.getItem("value") || "1 m/s * 3 years\n2 * 2", fromHash: false };
  }
})();

const MainApp = () => {
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
    setShowShowSnack(true);
  };
  const [showShareSnack, setShowShowSnack] = useState(false);
  const [showHashSnack, setShowHashSnack] = useState(initialValue.fromHash);

  return (
    <div className="primary">
      <ThemeProvider theme={theme}>
        <MainAppBar onClear={onClear} onShare={onShare} />
        <Snackbar open={showShareSnack} onClose={() => setShowShowSnack(false)} autoHideDuration={4000} message="Copied link to clipboard" />
        <Snackbar open={showHashSnack} onClose={() => setShowHashSnack(false)} autoHideDuration={4000} message="Loaded from URL" />
        <Container maxWidth="md" className="main-container">
          <Editor value={value} setValue={setValue} />
        </Container>
      </ThemeProvider>
    </div>
  );
}

function main() {
  ReactDOM.render((<MainApp />), document.getElementById('react-container'));
}

console.log('Starting app');
main();
