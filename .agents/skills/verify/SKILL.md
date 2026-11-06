---
name: verify
description: How to prove a calq change works, from engine tests to driving the app in a browser. Read before claiming a change works.
---

# Verify

Status: implemented

1. `pnpm run ci` passes: build, type check, the engine's spec and QuickCheck suites, lint checks,
   then the Playwright suite (`e2e/editor.spec.ts`) against `dist/`. `pnpm test` alone prints
   `N/N tests passed`.
2. The Playwright suite is the editor's acceptance test; extend it with every user-visible
   change. It runs desktop Chromium and mobile Chrome (Pixel 7); the maintainer chose Chromium
   only, so Firefox and Safari are not tested. Its pixel test screenshots the textarea's text
   and the highlight overlay separately and requires them to match exactly, so any layout change
   that desyncs the two fails it.
3. Also look at what a test can't judge: `pnpm build && pnpm serve` serves `dist/` on
   <http://localhost:8080>. Load a document by setting `localStorage.value` and reloading, or by
   a share link (`/#value=<urlencoded text>`), and screenshot desktop and a 390px viewport.
4. For changes to share links, saved documents or `Definitions.calq`, also load a document
   written for the old behavior and show it still evaluates the same (see the **project-context**
   skill's One-way section).

After a deploy, load <https://calq.app> and evaluate a line. It is a static page with no data, so
reading it is safe.
