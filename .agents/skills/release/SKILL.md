---
name: release
description: How a calq change lands on master and reaches calq.app, and who may push. Read before committing, pushing or deploying.
---

# Release

Status: implemented

- **Trunk.** Changes land on `master` (no branch protection). Agents work on a branch, then push
  to `master` directly, but only after the maintainer explicitly says to push or land it in the
  current conversation. Without that go-ahead, commit on a branch, push nothing, and hand back the
  SHA with the evidence. A go-ahead covers the change it was given for, not later ones.
- **Review.** The contract default holds: a diff touching share links, saved documents or the
  meaning of existing units (see the **project-context** skill) is reviewed by a model that did
  not write it before landing.
- **CI and deploy.** `.github/workflows/node.js.yml` runs `pnpm run ci` on every push and pull
  request. On a push to `master` that passes, the `deploy` job publishes `dist/` to the
  `gh-pages` branch (with `CNAME` `calq.app`), which GitHub Pages serves. Landing on `master` is
  deploying.
- **After landing.** Watch the run (`gh run list --branch master --limit 1`, then
  `gh run watch <id>`), confirm `deploy` succeeded, then check calq.app per the **verify** skill.
- **Manual deploy.** `pnpm gh-pages` builds locally and pushes `dist/` to `gh-pages`. Use it only
  when the maintainer asks.
