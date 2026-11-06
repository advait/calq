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

- React with no UI library: the app bar, buttons, toast and icons are ours (`main.tsx`,
  `toast.tsx`, `icons.tsx`), styled in plain CSS (`styles.css`, native nesting and custom
  properties). Add a dependency only when writing it ourselves is clearly worse.
- The editor is one transparent `<textarea>` over a highlighted overlay (`editor.tsx`). Both
  must lay text out identically: any font, spacing or wrapping property set on one goes on the
  shared `.editor-input, .editor-line` rule in `styles.css`.
- `pnpm typecheck` runs `tsc` in strict mode over `frontend/src` and `e2e/`. The engine's JS
  shape is declared in `frontend/src/purs.d.ts`; keep it in step with `engine/src/Editor.purs`.
  `typescript-eslint` caps `typescript` below 6.1, so `typescript` stays on `~6.0` until it
  supports newer.
- ESLint flat config (`eslint.config.mjs`: `@eslint/js` + `typescript-eslint` recommended) and
  Prettier defaults; `pnpm lint:ts` fixes, `lint:ts:ci` checks.
