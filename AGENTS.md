# AGENTS.md

Instructions for AI agents working in this repository.

## Never run `gatsby clean` (or `yarn clean` / `npm run clean`)

This site is usually developed with `yarn develop` running at
<http://localhost:8000>. `gatsby clean` deletes both `.cache/` and `public/`
while that server is live, which breaks it in ways that cannot be recovered
without a restart:

- `.cache/dev-404-page.js` disappears, but the dev server's page store still
  points at it, so the next rebuild fails with
  `#85913 GRAPHQL.EXTRACTION: There was a problem reading the file`.
- `public/` loses webpack's emitted assets, which the dev server will not
  re-emit, so every page returns an empty `HTTP 500` even though the build
  logs report success.

Restoring individual files does not help. The only recovery is killing the
dev server and starting it again.

If you think you need a clean build:

1. Ask the user to stop `yarn develop` first, or
2. Use a throwaway copy or a different working directory, or
3. Just build without cleaning — `gatsby build` is fine on its own and does
   not wipe `.cache`.

## Other things to know

- **Node >= 22.12** is required (`sanitize-html` enforces it, not Gatsby). A
  Nix flake provides it: `nix develop`. `.nvmrc` pins 22 for Netlify.
- **Gatsby pins `lmdb: 2.5.3` exactly.** Never override it in
  `package.json` `resolutions`; a newer lmdb silently corrupts the datastore
  so incremental rebuilds drop pages. Only `msgpackr` is overridden, to
  work around a Node 24 crash.
- **Animated GIFs** are not processed by `gatsby-remark-images`; they are kept
  as relative `<img>` tags and copied into `public/` by
  `gatsby-remark-copy-linked-files`. Do not remove that plugin.
- Verify changes with `yarn lint`, `yarn format:check` and `yarn build` before
  committing.

## CI

`.github/workflows/ci.yml` runs on pushes to `main` and on every pull
request. It sets up Node from `.nvmrc`, restores the yarn download cache
(via `setup-node`'s `cache: yarn`) and the Gatsby build cache, installs with
`--frozen-lockfile`, then runs, in order:

1. `yarn lint` (ESLint)
2. `yarn format:check` (Prettier, check-only)
3. `yarn build` (`gatsby build`)

The Gatsby cache (`actions/cache`) stores `.cache` and `public` and is keyed
on `yarn.lock` + `gatsby-config.js` + `gatsby-node.js` so lock/plugin/config
changes start from cold. Two consequences to keep in mind:

- CI builds are incremental, so a "successful" build may have reused pages
  from the cached `public/`. When debugging something that builds in CI but
  fails elsewhere, check whether it also fails with an empty `restore-keys`
  hit before trusting the green check. Never cache in this way for a deploy
  that must be reproducible.
- The cache is written from the workspace the build left behind. Anything
  generated into `public/` that should not ship will be carried forward, so
  deleted posts can linger in a cached CI build.

So a PR must be lint-clean, Prettier-formatted and buildable to merge. Run
`yarn format` to fix formatting locally — it rewrites Markdown too, which is
why blog posts and README are covered by `.prettierrc` like any other file.
Keep `.prettierignore` (`.cache`, `public`, `package.json`) in mind when
debugging a `format:check` failure: `package.json` is deliberately not
formatted.
