# juliu.is

A blog by [Ju Liu](https://juliu.is), built with [Gatsby](https://www.gatsbyjs.com) and Markdown.

## Develop

Requires Node.js >= 20 (see `engines` in `package.json`). A Nix flake is
provided for a ready-made shell with Node 24 and yarn:

```shell
nix develop   # or: direnv allow
```

Then:

```shell
yarn install
yarn develop
```

The site runs at `http://localhost:8000`, with a GraphQL explorer at
`http://localhost:8000/___graphql`.

## Build

```shell
yarn build
yarn serve   # preview the production build at http://localhost:9000
```

## Writing

Blog posts live in `content/blog/<slug>/index.md`, with assets alongside them:

```markdown
---
title: My post
date: "2026-01-01"
description: A short summary.
---

Content goes here.
```

## Structure

```
content/blog/     # posts
src/
  components/     # bio, layout, seo
  pages/          # index, 404
  templates/      # blog-post
gatsby-config.js  # plugins and site metadata
gatsby-node.js    # page creation and schema
```
