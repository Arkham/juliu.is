---
title: How I write software for fun in 2026
date: "2026-11-07T12:00:00.000Z"
description: Using Nix, Ghostty, pi, and cheap models.
---

_This post has been all hand-written, like in the golden days of the past._

Big news, software development has changed a lot in the last year.

So I thought it was time to update how I actually write software nowadays.

First of all, there is really no excuse for not using Nix anymore. Your coding
harness can insta-shot Nix configurations and fix all sorts of Nix errors, so do
yourself a favour and set up Nix on your computer. You can read this [blog
post](https://juliu.is/tidying-your-home-with-nix) from 2023, point your agent
at [my setup](https://github.com/Arkham/dotfiles.nix), just do it.

Once you have Nix and home-manager all set up, it's time to write some code!

So open your terminal and start typing `rails new`... Just kidding :)

Actually, talking about terminals, you should install [Ghostty](https://ghostty.org), it's made by Mitchell Hashimoto, and it's super great.

Then you want to install `pi`, it's the simplest and greatest agentic harness
ever written by man. I like that the creator is a normal human being who still
has doubts, and can express them without the pressure of trillions of dollars
of backing on his shoulders. Watch [this talk](https://www.youtube.com/watch?v=RjfbvDXpFls)
which explains the philosophical principles behind pi, then skim through [its website](https://pi.dev). You won't regret it.

Now comes the fun part: I don't want to be paying a $20 monthly subscription
for software that I write for fun. And also I don't want to use my work laptop
to write personal projects, which only exist to make me enjoy building
software. So what gives?

Luckily China exists, and in the last few years they have released open-weights
models that are basically (for my fun intents and purposes) equivalent to SOTA
models by American labs. But I still feel a bit funny actually connecting my
laptop to DeepSeek's servers in China.

_Open-weights._ A-ha!

There are many providers based in Europe or North America that host these
open-weights models, and using `pi` you can just use them and tell them to do
things. My personal favourite provider at the moment is
[Fireworks](https://fireworks.ai).

The great thing about these Chinese models is that some of them are _extremely_ cheap.

`deepseek-4.1-flash-1017` (a 552B-parameter model) costs this much:

| Type           | Cost     |
| -------------- | -------- |
| Uncached Input | $0.30/M  |
| Cached Input   | $0.006/M |
| Output         | $1.20/M  |

On average a short prompt costs me $0.002, that's incredibly cheap. And even
though this is the "standard" version, it's really fast, averaging 250 tokens
per second.

Compare that with an equivalent model from Anthropic, `claude-sonnet-5-5`,
which is much slower, averaging 140 tokens per second:

| Type             | Cost    |
| ---------------- | ------- |
| Uncached Input   | $2/M    |
| Cached Input     | $0.10/M |
| Cache Write (5m) | $2.50/M |
| Output           | $10/M   |

Moreover, you can easily switch to another model whenever you want, just check
the list of models Fireworks supports: https://fireworks.ai/models

There you have it, hope this was useful! 👋
