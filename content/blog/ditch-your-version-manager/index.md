---
title: Ditch your version manager
date: "2021-09-17T12:00:00.000Z"
description: Try Nix and don't look back.
---

You probably use a variety of tools to manage your Ruby, Node, Python,
Elixir versions, such as [rvm](http://rvm.io/),
[rbenv](https://github.com/rbenv/rbenv),
[nvm](https://github.com/nvm-sh/nvm), or [asdf](https://asdf-vm.com/).
They all work reasonably well, right?

Well, not quite.

The fundamental issue is that many programs depend on _other_ programs being
present in the same system. For many years, if you wanted to install
[nokogiri](https://nokogiri.org/), a popular XML parser written in Ruby,
you'd have to remember to install `libxml` beforehand. Furthermore, some of
these programs depend on OS-specific configurations, such as libraries
shipped with Mac OS or Linux.

It's not uncommon to check out a project and having to install a plethora
of libraries and tools, and having to hope that the versions that you
installed are compatible with the ones used by the original authors. Then
you'd have to figure out a way to install an older version of a specific
package.

I can hear what you're wondering now: what about
[Docker](https://www.docker.com/)? The idea of having a container image
that contains all your dependencies sounds very promising indeed. The issue
with docker images is that all those dependencies are still intertwined,
only the whole dependency tree has become *invisible*: for example, if the
version of Ubuntu you are using has reached its End-Of-Life, you will have
to manually untangle all the dependencies and update everything in one go.
If you're working on a large project with many dependencies, please do accept
my condolences.

My ideal dependency manager would allow me to specify **each and every**
dependency that is required to work on my projects. It should be easily
reproducible, declarative and easy to upgrade.

Does it sound too good to be true?

## Welcome to the jungle

Let me introduce you to [Nix](https://nixos.org/).

> Nix is a tool that takes a unique approach to package management and system configuration. Learn how to make reproducible, declarative and reliable systems.

Let's try it out.

```bash
$ sh <(curl -L https://nixos.org/nix/install) --daemon
```

```bash
$ nix-env --version
nix-env (Nix) 2.3.10
```

Before we can start on our first project we will need to install
[direnv](https://direnv.net/), a tool which will let us build
a custom environment in a specific folder. You'll need to:

- `brew install direnv`
- [hook direnv into your shell](https://direnv.net/docs/hook.html)
- restart your shell

Now we're ready and raring to go! 🏄‍♀️

Let's create a new folder:

```bash
$ mkdir nix_hello_world && cd nix_hello_world

$ hello
-bash: hello: command not found
```

As you can see, no `hello` command is available yet.

Great, what I haven't told you is that Nix is also a pure, lazy, functional
[programming language](https://nixos.wiki/wiki/Nix_Expression_Language). Exciting!

We can create a `default.nix` like this one:

```nix
let
  pkgs = import <nixpkgs> { };
in
pkgs.mkShell {
  buildInputs = [ pkgs.hello ];
}
```

And we can tell direnv to load this file and update our environment.

```bash
$ echo "use nix" > .envrc
direnv: error /Users/arkham/Desktop/nix_hello_world/.envrc
is blocked. Run `direnv allow` to approve its content

$ direnv allow
<loads of nix output>

$ hello
Hello, world!
```

🎉

What did we do?

- we imported the whole set of packages provided by Nix and called it `pkgs`
- we used the `mkShell` built-in function to create a new environment where
  we can use Nix programs
- we specify we want the GNU `hello` program

What if we wanted ruby? Couldn't be simpler:

```nix
let
  pkgs = import <nixpkgs> { };
in
pkgs.mkShell {
  buildInputs = [
    pkgs.hello
    pkgs.ruby
  ];
}
```

By saving this file and returning to your shell (you might need to press
`Enter` to let direnv reload), Nix will pick up the change and install the
new dependency.

```bash
$ ruby --version
ruby 2.7.4p191 (2021-07-07) [x86_64-darwin17]

$ which ruby
/nix/store/r6siyyqmnidkn2y8y5hl7payykb51kb0-ruby-2.7.4/bin/ruby
```

What's with that super long path? Don't worry, it's just how Nix stores its
data (*waves hands.. iMmUTaBIlitY*).

What if we needed node?

```nix
let
  pkgs = import <nixpkgs> { };
in
pkgs.mkShell {
  buildInputs = [
    pkgs.hello
    pkgs.ruby
    pkgs.nodejs
  ];
}
```

```bash
$ node --version
v14.17.2

$ which node
/nix/store/zj1sqykb3s5a0fakm94qwz7fg6g2zxpm-nodejs-14.17.2/bin/node
```

You see the pattern here.

How about a different version of Ruby or Node? Let's say that our project
depends on Ruby 2.6 and Node 10. We can [go and
search](https://search.nixos.org/packages) for those specific versions,
then change our `default.nix` accordingly:

```nix
let
  pkgs = import <nixpkgs> { };
in
pkgs.mkShell {
  buildInputs = [
    pkgs.hello
    pkgs.ruby_2_6
    pkgs.nodejs-10_x
  ];
}
```

```
$ ruby --version
ruby 2.6.8p205 (2021-07-07) [x86_64-darwin17]

$ node --version
v10.24.1
```

🎉

## Some bad news

Unfortunately, if you tried to follow this article step by step, you'll
have noticed that the versions of `ruby` and `node` you installed are
probably slightly different from the ones above.

Wasn't Nix supposed to give us _reproducible_ builds?

Yes, but...

All the software that we installed depends on the specific version of the
**nixpkgs channel** that we installed on our system. We can check which
version that is by running:

```bash
$ nix repl
Welcome to Nix version 2.3.10. Type :? for help.

nix-repl> (import <nixpkgs> {}).lib.version
"21.05pre292875.60563458051"
```

We can then grab the commit part (the third segment) and view it on
[GitHub](https://github.com/NixOS/nixpkgs/commit/60563458051).

If only we were pointing to the same commit, then we would have exactly the
same dependencies. But it would be foolish to require every project to
depend on the same version of nixpkgs. We need a way to specify this on a
per-project basis, right?

## niv

In order to pin our configuration to a specific version, we can use
[niv](https://github.com/nmattia/niv).

Let's install it:

```bash
$ nix-env -iA nixpkgs.niv
```

Now we can initialize a niv project:

```bash
$ niv init --no-nixpkgs
Initializing
  Creating nix/sources.nix
  Creating nix/sources.json
  Not importing 'nixpkgs'.
Done: Initializing
```

This command generates a couple files:

```bash
$ tree
.
└── nix
    ├── sources.json
    └── sources.nix

1 directory, 3 files
```

The `sources.json` will contain all the dependencies that we're going to
use.  Don't worry about the `sources.nix` file, it's mostly glue to allow
us to load a JSON file in Nix.

For the sake of reproducibility, change your `sources.json` to look like
this:

```json
{
    "nixpkgs": {
        "branch": "release-21.05",
        "description": "Nix Packages collection",
        "homepage": "",
        "owner": "NixOS",
        "repo": "nixpkgs",
        "rev": "5f244caea76105b63d826911b2a1563d33ff1cdc",
        "sha256": "1xlgynfw9svy7nvh9nkxsxdzncv9hg99gbvbwv3gmrhmzc3sar75",
        "type": "tarball",
        "url": "https://github.com/NixOS/nixpkgs/archive/5f244caea76105b63d826911b2a1563d33ff1cdc.tar.gz",
        "url_template": "https://github.com/<owner>/<repo>/archive/<rev>.tar.gz"
    }
}
```

Now we can create our top-level Nix configuration:

```nix
let
  sources = import ./nix/sources.nix { };
  pkgs = import sources.nixpkgs { };
in
pkgs.mkShell {
  buildInputs = [
    pkgs.hello
    pkgs.ruby_2_6
    pkgs.nodejs-10_x
  ];
}
```

Going back to the shell, you will see that the version of ruby has changed:

```bash
$ ruby --version
ruby 2.6.7p197 (2021-04-05) [x86_64-darwin17]
```

Same for node:

```bash
$ which node
/nix/store/d08y5bbjq5pric7rwifim6bf4zvwmasc-nodejs-10.24.1/bin/node
```

This time you're going to see exactly the same versions!

Another good news is that configuration will work **forever**. That's
right, the moment you commit this configuration to your project you will
ensure that it will never stop working.

Need to install a new tool? Pop it in the Nix config.
Need to patch a program and make sure every one is using it? Create an
[overlay](https://nixos.wiki/wiki/Overlays), pop it in the Nix file and
grab some pop corn. Need to update your packages? Run `niv update` and all
the new dependencies will be pulled in.

Nix is a version manager for everything. You'll never have to tell the new
colleague "oh right, you need to install this version of LLVM to get this
thing to work". They can pull the repo, `direnv allow` and BAM,
fully-working, reproducible project setup.

What are you waiting for?!
