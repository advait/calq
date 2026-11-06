<h1 align="center">calq</h1>

<p align="center">
  <strong>A calculator for humans.</strong><br />
  Write math the way you think about it, units included.
</p>

<p align="center">
  <a href="https://calq.app"><strong>calq.app</strong></a> ·
  <a href="https://github.com/advait/calq/actions/workflows/node.js.yml"><img alt="CI" src="https://github.com/advait/calq/actions/workflows/node.js.yml/badge.svg" /></a>
</p>

<p align="center">
  <img src="docs/hero.png" alt="calq evaluating a road-trip budget with miles, mph, gallons and USD" width="760" />
</p>

## What it does

calq is a notepad that evaluates every line as you type.

- **Units everywhere.** `1 m/s * 3 years in km` → `94608 km`. Length, time, mass, area, volume,
  speed, current, temperature (Kelvin), currency and more, with SI prefixes.
- **Variables.** `distance = 420 miles`, then reuse `distance` on later lines.
- **Conversions.** End any expression with `in <unit>`.
- **Comments.** Lines starting with `#` are notes.
- **Share.** The Share button copies a link with your whole document in the URL. Your work is also
  saved in the browser between visits.

Every unit calq knows is defined in calq's own language, in
[`engine/src/Definitions.calq`](engine/src/Definitions.calq).

## Development

calq is a PureScript engine (`engine/`) behind a React frontend (`frontend/`), built with Vite.

Requirements: Node (version in [`.node-version`](.node-version)) and Yarn via Corepack
(`corepack enable`).

```sh
yarn install
yarn build:spago   # compile the engine to output/
yarn dev           # http://localhost:8080
```

| Command     | What it does                                                  |
| ----------- | ------------------------------------------------------------- |
| `yarn dev`  | Vite dev server on port 8080 (run `yarn dev:spago` alongside) |
| `yarn test` | Build the engine and run its spec and QuickCheck suites       |
| `yarn lint` | Format PureScript (purs-tidy) and TS/JS (ESLint, Prettier)    |
| `yarn ci`   | Build, test and lint check, the same command CI runs          |

Pushes to `master` that pass CI deploy to [calq.app](https://calq.app) via GitHub Pages.

The engine's concepts (axes, units, aliases) are described in [design.md](design.md).

## License

[MIT](LICENSE)
