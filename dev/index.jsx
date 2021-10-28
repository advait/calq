import React, { useState } from 'react';
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

import ReactDOM from 'react-dom';
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
        {/* <Button startIcon={<ShareIcon />} >Share</Button> */}
        <div style={{ flexGrow: 1 }} />
        <Typography variant="h6" sx={{ marginRight: "1em" }}>calq</Typography>
        <Link href="https://github.com/advait/calq" target="_blank">
          <IconButton aria-label="github"><GithubIcon color="primary" /></IconButton>
        </Link>
      </Toolbar>
    </Container>
  </AppBar>
)

const MainApp = () => {
  const initialValue = localStorage.getItem("value") || "1 m/s * 3 years\n2 * 2";
  const [value, setValue_] = useState(initialValue);
  const onClear = () => setValue_("");
  const setValue = (v) => {
    localStorage.setItem("value", v);
    setValue_(v);
  };

  return (
    <div className="primary">
      <ThemeProvider theme={theme}>
        <MainAppBar onClear={onClear} />
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

// HMR stuff
// For more info see: https://parceljs.org/hmr.html
if (module.hot) {
  module.hot.accept(function () {
    console.log('Reloaded, running main again');
    main();
  });
}

console.log('Starting app');
main();
