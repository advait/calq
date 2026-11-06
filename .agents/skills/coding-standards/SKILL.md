---
name: coding-standards
description: calq's language rules and the check every diff passes before commit. Read before writing or reviewing code here.
---

# Coding standards

Status: implemented

The check before every commit is `pnpm run ci` (build, test, then `lint:ci`), the same command CI
runs (`.github/workflows/node.js.yml`). Run `pnpm lint` first to apply formatting.

## PureScript (`engine/`)

- Format with purs-tidy (`.tidyrc.json`); `pnpm lint:purs` fixes, `lint:purs:ci` checks.
- Dependencies come from the registry package set pinned in `spago.yaml`; add packages to
  `engine/spago.yaml` (`dependencies`, or `test.dependencies` for test-only).
- JS FFI files sit next to their module (`Decimal.js`, `Utils.js`) and are ES modules.
  `Utils.js` imports `.calq` files as raw strings (`?raw`), which Vite resolves in the app and
  `engine/test/custom-loader.mjs` resolves in tests; a new import style must work in both.
- Numbers are `decimal.js` decimals (`engine/src/Decimal.purs`), never JS floats.
- New tests go under `engine/test/` as `Test.*` modules; `Test.Main` discovers them.
  Interpreter cases that are just input/output pairs go in `engine/test/InterpreterTest.calq`.

## Frontend (`frontend/`)

- ESLint flat config (`eslint.config.mjs`: `@eslint/js` + `typescript-eslint` recommended) and
  Prettier defaults; `pnpm lint:ts` fixes, `lint:ts:ci` checks.
- There is no `tsc` step; TypeScript is used only through the ESLint parser.
  `typescript-eslint` caps `typescript` below 6.1, so `typescript` stays on `~6.0` until it
  supports newer.
- MUI components and `@mui/icons-material` for UI; styles in `frontend/src/styles.scss`.
