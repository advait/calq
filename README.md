<h1 align="center">calq</h1>

<p align="center">
  <strong>A calculator for humans.</strong><br />
  Write math the way you think about it, units included.<br />
  Written in <a href="https://www.purescript.org">PureScript</a>.
</p>

<p align="center">
  <a href="https://calq.app"><strong>calq.app</strong></a> ·
  <a href="https://github.com/advait/calq/actions/workflows/node.js.yml"><img alt="CI" src="https://github.com/advait/calq/actions/workflows/node.js.yml/badge.svg" /></a>
</p>

<p align="center">
  <img src="docs/hero.png" alt="calq evaluating unit conversions, a road-trip budget, a salary, and physics and data calculations" width="760" />
</p>

## What it does

calq is a notepad that evaluates every line as you type, with each answer beside its line.

- **Units everywhere.** `1 m/s * 3 years in km` → `94608 km`. Length, time, mass, area, volume,
  speed, data (`1 GB / (100 Mbit/s)` → `80 s`), current, temperature (Kelvin), currency and
  more, with SI and binary prefixes.
- **Variables.** `distance = 420 miles`, then reuse `distance` on later lines.
- **Conversions.** End any expression with `in <unit>`.
- **Comments.** Lines starting with `#` are notes.
- **Copy.** Click an answer to copy it.
- **Settings.** The gear sets how many decimal places answers show (0 to 10, or every digit)
  and the theme (light, dark, or follow the system).
- **Share.** The Share button copies a link with your whole document in the URL. Your work is also
  saved in the browser between visits.

Every unit calq knows is defined in calq's own language, in
[`engine/src/Definitions.calq`](engine/src/Definitions.calq).

## Written in PureScript

calq's engine (tokenizer, parser, interpreter and unit system) is about 1,300 lines of pure,
strongly typed functional code in [PureScript](https://www.purescript.org), compiled to
JavaScript and run in your browser. React only draws the page.

- **Parser combinators.** The tokenizer and the expression grammar are small parsers built from
  smaller ones with [`parsing`](https://pursuit.purescript.org/packages/purescript-parsing)
  ([`Tokenizer.purs`](engine/src/Tokenizer.purs), [`TokenParser.purs`](engine/src/TokenParser.purs)).
- **Units form a group.** A unit like `m/s²` is a map from base units to integer powers, with a
  `Group` instance: multiplying values multiplies their units, and every unit has an inverse
  ([`Exponentials.purs`](engine/src/Exponentials.purs)).
- **Errors are values.** Evaluation runs in `StateT InterpreterState (Either String)`, so a bad
  line becomes a message beside it, never a crash
  ([`Interpreter.purs`](engine/src/Interpreter.purs)).
- **Decimal arithmetic.** Numbers are [decimal.js](https://mikemcl.github.io/decimal.js/)
  decimals behind a small FFI, so `0.1 + 0.2` is `0.3`
  ([`Decimal.purs`](engine/src/Decimal.purs)).
- **Property-tested.** QuickCheck checks that no calculator-like input makes the engine throw,
  and that syntax highlighting reproduces every line exactly.

## Development

The PureScript engine lives in `engine/` (built with spago); the React frontend in `frontend/`
has no UI library and is built with Vite.

Requirements: Node (version in [`.node-version`](.node-version)) and pnpm (version in
`packageManager` in [`package.json`](package.json)).

```sh
pnpm install
pnpm build:spago   # compile the engine to output/
pnpm dev           # http://localhost:8080
```

| Command          | What it does                                                  |
| ---------------- | ------------------------------------------------------------- |
| `pnpm dev`       | Vite dev server on port 8080 (run `pnpm dev:spago` alongside) |
| `pnpm test`      | Build the engine and run its spec and QuickCheck suites       |
| `pnpm test:e2e`  | Browser tests against `dist/` in desktop and mobile Chromium  |
| `pnpm typecheck` | Type-check the frontend and the browser tests                 |
| `pnpm lint`      | Format PureScript (purs-tidy) and TS (ESLint, Prettier)       |
| `pnpm run ci`    | Everything above, the same command CI runs                    |

Browser for the e2e tests: `pnpm exec playwright install chromium`.

Pushes to `master` that pass CI deploy to [calq.app](https://calq.app) via GitHub Pages.

The engine's concepts (axes, units, aliases) are described in [design.md](design.md).

## License

[MIT](LICENSE)
