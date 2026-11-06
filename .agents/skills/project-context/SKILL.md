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
- `frontend/`: React app with no UI library, bundled by Vite (`vite.config.js`), which imports
  the compiled engine through the `@purs-compiled` alias.
- `e2e/`: Playwright tests of the built app (`playwright.config.ts`).
- `design.md`: the engine's concepts (axes, units, aliases).

## One-way

- **Share links.** `#value=<URL-encoded document>` (`frontend/src/share.ts`). Links made by older
  versions (qs-encoded) are already shared in the wild and must keep loading; `e2e/` tests both
  formats.
- **Saved documents.** `localStorage` key `"value"` (`frontend/src/main.tsx`).
  Changing the key or format loses every returning user's document. Settings live under
  `"settings"` as JSON `{ places, theme }` (`frontend/src/settings.tsx`); unknown or missing
  fields fall back to the defaults, so new fields are safe but renames are not.
- **Meaning of existing documents.** Renaming or removing a unit or alias in `Definitions.calq`, or
  changing syntax, breaks documents and links that use it. Additions are safe.
- **calq.app.** Every push to `master` that passes CI deploys (see the **release** skill).

## Tasks

pnpm, version pinned by `packageManager` in `package.json` (Node 26 ships no corepack; use
`mise x pnpm@<version> -- pnpm` or a global pnpm). Node from `.node-version`, which mise picks up.
`pnpm-workspace.yaml` allows only `purescript`'s install script (it downloads `purs`). Canonical
scripts in `package.json`; use `pnpm run ci`, since `pnpm ci` is pnpm's own clean install:

- `pnpm install`, then `pnpm build:spago` once per checkout (creates `output/`; the frontend
  imports from it).
- `pnpm dev` (Vite on :8080; pair with `pnpm dev:spago` for engine rebuilds), `pnpm test`,
  `pnpm lint` (fixes), `pnpm run ci` (the check CI runs), `pnpm build` (to `dist/`).

## Follow-ups

GitHub issues on `advait/calq`; list them in the reply and file only when asked.
