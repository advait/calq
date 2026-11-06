---
name: project-context
description: What calq is, who uses it, what is one-way, and how its tasks run. Read before the first decision in this repository.
---

# Project context

Status: implemented

calq is a public, single-maintainer side project: a notepad calculator with units, live at
<https://calq.app> (GitHub Pages, custom domain). Users are anonymous visitors; there is no backend,
account or server-side data. The worst failure is a broken calq.app or a change that breaks
documents people already have.

## Layout

- `engine/`: PureScript interpreter (tokenizer, parser, evaluator). Package manifest
  `engine/spago.yaml`; workspace and registry package set in `spago.yaml`. Compiles to `output/`.
- `engine/src/Definitions.calq`: every unit, alias and constant, written in calq itself.
- `frontend/`: React + MUI app, bundled by Vite (`vite.config.js`), which imports the compiled
  engine through the `@purs-compiled` alias.
- `design.md`: the engine's concepts (axes, units, aliases).

## One-way

- **Share links.** `#value=<qs-encoded document>` (`frontend/src/index.jsx`, `initialValue` and
  `onShare`). Links are already shared in the wild; old links must keep loading.
- **Saved documents.** `localStorage` key `"value"` (`index.jsx`, `frontend/src/editor.tsx`).
  Changing the key or format loses every returning user's document.
- **Meaning of existing documents.** Renaming or removing a unit or alias in `Definitions.calq`, or
  changing syntax, breaks documents and links that use it. Additions are safe.
- **calq.app.** Every push to `master` that passes CI deploys (see the **release** skill).

## Tasks

Yarn 1 via Corepack (`packageManager` in `package.json`); Node from `.node-version`, which mise
picks up. Canonical scripts in `package.json`:

- `yarn install`, then `yarn build:spago` once per checkout (creates `output/`; the frontend
  imports from it).
- `yarn dev` (Vite on :8080; pair with `yarn dev:spago` for engine rebuilds), `yarn test`,
  `yarn lint` (fixes), `yarn ci` (the check CI runs), `yarn build` (to `dist/`).

## Follow-ups

GitHub issues on `advait/calq`; list them in the reply and file only when asked.
