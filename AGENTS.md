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
- Verify changes with `yarn lint` and `yarn build` before committing.
