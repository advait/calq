---
name: verify
description: How to prove a calq change works, from engine tests to driving the app in a browser. Read before claiming a change works.
---

# Verify

Status: implemented

1. `pnpm run ci` passes (build, the engine's spec and QuickCheck suites, lint checks). `pnpm test`
   alone prints `N/N tests passed`.
2. For any change a user can see (frontend, engine output, units), drive the app:
   - `pnpm dev` serves on <http://localhost:8080> (`vite.config.js`); for the production bundle,
     `pnpm build && pnpm serve` serves `dist/` on the same port.
   - Load a document through a share link, `http://localhost:8080/#value=<urlencoded text>`, or
     by setting `localStorage.value` and reloading. Read each `⤷` result line in
     `.main-container`, type into a line's `textarea`, and press Share (copies the link) and Clear.
   - Headless Chromium through Playwright works; record the console errors (none expected).
3. For changes to share links, saved documents or `Definitions.calq`, also load a document
   written for the old behavior and show it still evaluates the same (see the **project-context**
   skill's One-way section).

After a deploy, load <https://calq.app> and evaluate a line. It is a static page with no data, so
reading it is safe.
