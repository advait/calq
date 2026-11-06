---
name: release
description: How a calq change lands on master and reaches calq.app, and who may push. Read before committing, pushing or deploying.
---

# Release

Status: implemented

- **Trunk.** Changes land on `master` (no branch protection). Asking for a change is asking for
  it to ship: don't ask again before merging. Work on a branch, open a pull request so CI runs,
  and once CI is green and any required review below is done, fast-forward `master` to the PR
  head (`git push origin <sha>:master`), then do the after-landing checks. Stop and ask only when
  the maintainer said to hold, CI fails in a way you can't fix, or review finds a `fix` you can't
  resolve.
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
