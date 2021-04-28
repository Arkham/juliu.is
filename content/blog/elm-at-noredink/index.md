---
title: "Elm at NoRedInk"
date: "2021-04-28T12:00:00.000Z"
description: How to organize 500K lines of Elm.
---

At [NoRedInk](https://www.noredink.com) we have one of the largest Elm apps
in the world. It serves millions of teachers and students, and our frontend
code is almost exclusively written in Elm. In this post, we will explore
the structure of our codebase and the patterns that we use to stay sane.
One of the most common questions I get about Elm is:

> "Does it scale? And if so, how?"

I don't think that there can ever be a comprehensive answer to that
question, but I hope this post can be a useful reference to resources and
techniques we've used to solve real-world problems, so you can dig deeper
at your own leisure.

## Table of contents

```toc

```

## A wild monorepo appears

We have a glorious monorepo that contains all the services that power our
infrastructure. Our biggest service (which we affectionately call the
`monolith`) is written in Rails and contains most of our Elm code:

```bash
$ cloc --include-ext=elm monolith/ui/src
-----------------------------------------------
Language    files    blank    comment      code
-----------------------------------------------
Elm          1506    49759      16535    211835
-----------------------------------------------
```

Nothing much to add here. Moving on :)

```bash
$ cloc --include-ext=elm monolith/ui/tests
-----------------------------------------------
Language    files    blank    comment      code
-----------------------------------------------
Elm           569    10275       1309    200586
-----------------------------------------------
```

As you can see, we write loads of tests. As much as the Elm compiler gives
you that wonderful confidence while refactoring, we still want to make sure
that our code is working as intended. We will talk more in-depth about
testing later.

```bash
$ cloc --include-ext=elm monolith/ui/generated
-----------------------------------------------
Language    files    blank    comment      code
-----------------------------------------------
Elm           129     2428       1728     26399
-----------------------------------------------
```

We end up auto-generating a lot of Elm code: we do this for multiple
purposes, such as automatically generating types from graphql, or ensuring
that JSON payloads sent from Rails controllers match the definitions inside
our Elm decoders.

```bash
$ cloc --include-ext=elm content-creation
-----------------------------------------------
Language    files    blank    comment      code
-----------------------------------------------
Elm            80     3245       1182     13941
-----------------------------------------------
```

At last, here's a snapshot of Elm code in one of our Haskell services. I
won't spend much time describing how that works in this post, let me know
if you are interested and I'll write a follow-up. For now, let's take a
deeper look at how we integrate with Rails.

## Rails conventions

Following the Rails architecture, each REST resource is managed by its
dedicated controller. Here is what happens when a teacher goes to manage
their classes:

- they visit the `/teach/classes` URL in their browser
- that route is managed by the `Teach::ClassesController` controller
- that controller will fetch stuff from the database, clean it up and load a corresponding view, `app/views/teach/classes/index.html.haml`

That view looks like this:

```haml
- content_for :title, "Manage Classes | NoRedInk"

- content_for(:javascript) do
  = javascript_include_tag "shake/Page/Teach/Classes/index.js"

= elm_mount(@elm_flags, 'teach-classes-elm')
```

What is that `elm_mount` helper? We define it to be like this:

```ruby
def elm_mount(flags, prefix = 'elm')
  tag.div(id: prefix + '-flags',
          class: "elm-flags",
          data: {flags: flags.to_json}) +
    tag.div(id: prefix + '-host')
end
```

In short, we need some conventions for the names of two DOM nodes:

- one that contains the flags that we pass from Rails to Elm: we use this
  to pass data to the Elm application that needs to be available on boot
- one that will contain the actual Elm app when it's mounted

The javascript file that we include is the result of the compilation of the
Elm app. We use a build system called [Shake](https://shakebuild.com/) to
generate all our assets. The actual entry point looks like this:

```javascript
import { Elm } from "./Main.elm"

import * as NriProgram from "Nri/Program.js"
import setupReadAloud from "ReadAloud/setup.js"

NriProgram.domready(function () {
  const { subscribe, send } = NriProgram.mountPorts(
    Elm.Page.Teach.Classes.Main,
    "ui/src/Page/Teach/Classes/index.js",
    "teach-classes-elm"
  )

  setupReadAloud({ subscribe, send })
})
```

The `NriProgram` that you see here is a small wrapper around common
operations that we need to perform, such as:

- passing an environment object to our apps so that we can have different
  settings in test, development and production
- setting up some basic analytics and reporting
- grabbing the flags and the div where the Elm app will be mounted
- giving us the ability to easily set up ports using `subscribe` and `send`

Another interesting bit is the convention that we use to name our Elm
modules. Since the URL is going to be `/teach/classes`, the associated Elm
module is going to be `Page.Teach.Classes.Main`. Here are some other
examples:

- `Page.Admin.RelevantTerms.Main` corresponds to `/admin/relevant_terms`
- `Page.Learn.ChooseSourceMaterials.Main` corresponds to `/learn/choose_source_materials`
- `Page.Preferences.Main` corresponds to `/preferences`
- `Page.Teacher.Courses.Assignments.Main` corresponds to `/teach/courses/:id/assignments`

In total, we have over a hundred Elm apps that serve different Rails
controllers. They consist of a mixture of normal Elm applications and
single-page apps. By adapting the Rails motto of "convention over
configuration" we're pretty confident we can scale this approach indefinitely.

## Our Elm programs

At this point, you probably won't be surprised to learn that we use a
custom wrapper as the entrypoint of our Elm programs. Here is how it looks
like:

```elm
main : Nri.Program.Program Model Msg
main =
    Nri.Program.program
        { moduleName = "Page.Interests.Main"
        , flagsDecoder = \_ -> decode
        , perform = perform
        , init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
```

Let's compare it to `Browser.element` from `elm/browser`:

```elm
main : Browser.Program flags model msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
```

As you can see, the types are slightly different and we include a couple of
different fields in our version.

What are the advantages of using a custom `Nri.Program`?

- **reducing boilerplate**: for example, we can automatically handle decoding failures so that
  we don't have to worry about them on each individual page.
- **handling generic behaviours**: for example, we can automatically detect the user's
  input method to make our product more accessible.

The differences don't stop there. Let's look at the type of the `view` function:

```elm
view : Env -> Model -> Html Msg
```

Here we take an additional `Env` argument. This record contains information
about the current release, the logged-in user and the environment where the
code is running. For example, if we detect that we are running in test
mode, there is no need to run fancy UI animations, thus we can disable
them. This in turn makes our tests faster and less flaky. Win-win!

If you look at the record that we pass to `Nri.Program.program` you will notice
an additional `perform` field. Let's look at the type signature:

```elm
perform : Env -> Effect -> Cmd Msg
```

This is easier to understand if we look at the type signature of `update`:

```elm
update : Env -> Msg -> Model -> ( Model, Effect )
```

The curious change is that a standard update function in Elm returns a
`(Model, Cmd Msg)` tuple, while ours returns `(Model, Effect)`. Why is
that? `Cmd Msg` is an opaque type that is meant to be passed as-is to the
Elm runtime. But in order to test that the correct side effect has been
emitted we need to be able to inspect it: so we define our own type and we
call it `Effect`.

Then we can use the fantastic `elm-program-test` library (you can find it
[here](https://package.elm-lang.org/packages/avh4/elm-program-test/latest))
to test our Elm programs. Meanwhile, in our normal application, we
can use the `perform` function to convert the `Effect` representation of a
side effect to its `Cmd Msg` counterpart.

Similarly, we have another custom wrapper for single-page applications called
`Nri.Spa.Program` which mostly wraps `Browser.application`.

The general lesson here is that just because Elm provides an `update`
function with the shape `msg -> model -> (model, Cmd msg)` your version
**doesn't need to**. You might need more arguments, or different types, or
more return values. As long as you convert the result of your function to
match the API of `Browser.element` you can customize your API as much as
you want.

## How we write Elm files

We don't believe in the mantra:

> Prefer small files

Here is a little snapshot of some large files in our codebase:

```bash
2251 src/Page/Learn/GuidedDrafts/Main.elm
1773 src/Page/Curriculum/BrowseAndAssignLayout.elm
1725 src/Page/Teach/Classes/Page/Classes/Explore.elm
1691 src/Page/Teach/Assignment/Form/GuidedDraft/Customize.elm
1524 src/Page/Teach/Courses/PeerReviews/Progress.elm
1280 src/Page/Admin/Alignments/Main.elm
1188 src/Page/Learn/Home/Main.elm
1184 src/Page/Teach/Assignment/Form/Model.elm
1136 src/Page/Preview/PeerReview/Main.elm
```

As Evan Czaplicki explains in [this
talk](https://www.youtube.com/watch/XpDsk374LDE), larger files in Elm are
not a problem. We tend to delineate the different sections related to the
model, update and view functions with comments. Then we extract functions
around their data structures, rather than their perceived role.

Another point I want to emphasize is that in Elm you don't have to get
everything right from the start. Choose the simplest architecture that
works. If you need to change it later, the compiler will assist you every
step of the way. Specifically for this sort of mechanical changes, the
chance of introducing bugs is very close to zero. If it compiles, it works.

We regularly merge refactors in the thousands of lines that don't introduce
a single regression to the product. There is really nothing to be afraid
of! 🍧

## Nesting the Elm Architecture

When we need to combine multiple Elm applications, we nest them under one
another. So for example if we have a modal in a page with its own state and
messages, we will do this:

```elm
type alias Model =
    { modal : Modal.Model
    }

type Msg = ModalMsg Modal.Msg

update : Env -> Msg -> Model -> ( Model, Effect )
update env msg model =
    case msg of
        ModalMsg modalMsg ->
            let
                ( newModal, modalEffect ) =
                    Modal.update env modalMsg model.modal
            in
            ( { model | modal = newModal}
            , ModalEffect modalEffect
            )

view : Env -> Model -> Html Msg
view env model =
    div []
        [ Modal.view env model.modal |> Html.map ModalMsg
        , span [] [ text "Hello there" ]
        ]
```

Let's break this down.

We have a top-level `Msg` type that describes the behaviour of the
top-level application. Its only variant is a `ModalMsg Modal.Msg` which
wraps the message type of the modal.

When we receive a `ModalMsg` we pattern match to extract the inner message
and we run it against the modal state stored in the model through
`Modal.update`. Then we replace the modal with the updated version and do
something with the side effect.

Something else to note is that we need to use `Html.map` to wrap the
`Modal.view` function. In this way, we ensure that all the messages that are
emitted are wrapped with the `ModalMsg` message constructor.

This is how we build big apps in Elm. That's the secret sauce. By applying
this concept repeatedly you can scale your Elm apps as much as you want.
Here is the actual `Msg` type of our `Teach.Classes.Main` module:

```elm
type Msg
    = ChangePage Route
    -- Nested TEA
    | TimedAlerts Alert.Msg
    | ClassesMsg Classes.Msg
    | ModalMsg Modal.Msg
    | EditClassMsg EditClass.Msg
    -- HTTP responses
    | ReceiveClasses
        Class.RedirectToClass
        (WebData
            { activeClasses : List Class.Class
            , archivedClassCount : Int
            }
        )
    | ReceiveArchivedClasses (WebData (List Archived.Class))
    -- DOM focus
    | Focus String
    | Focused (Result Dom.Error ())
    -- User actions
    | OpenModal Modal.Kind String
    | CloseModal
    | SetModalKind Modal.Kind
    | ModalError Http.Error
    | AddStudent Modal.CreateStudentAccountsManuallyModel
    | Archived Int
    | Unarchived Int
    | ToggleTooltip String Bool
```

## When to nest TEA modules

If you looked at the section above and you thought "welp, that looks like a
lot of work", I hear you. As a matter of fact, you don't need to do the
whole nesting dance every time we want to reuse a module. In fact, most of
the UI components that we use don't have their own state, so they don't
need even need an `update` function. Let's look at how we use a button:

```elm
import Nri.Ui.Button.V10 as Button

view : Env -> Model -> Html Msg
view env model =
    div []
        [ Button.link "Edit"
            [ Button.css [ Css.marginBottom (Css.px 10) ]
            , Button.onClick EditAssignment
            , Button.icon UiIcon.calendar
            , Button.small
            ]
        ]
```

It seems obvious, but if there is no state, then there is no need to do
extra work.

If you are curious about how we structure our UI components, you can take a
gander at [noredink-ui](https://github.com/NoRedInk/noredink-ui) (check out
the preview [here](https://noredink-ui.netlify.app/)). We're always
including simple examples for each UI component so that they act as the
best documentation possible
([component](https://noredink-ui.netlify.app/#/doodad/Switch) /
[example](https://github.com/NoRedInk/noredink-ui/blob/master/styleguide-app/Examples/Switch.elm)).

If you feel that it is too cumbersome to nest TEA applications, it may be
because _it just is_. If you choose it as your default approach to scale
Elm code, you will find yourself constantly wrapping and unwrapping
messages for little or no benefit. I would recommend reaching for it as a
last resort, like using a rocket launcher to plant daffodils in your
garden: it works, but it's a bit heavy-handed.

Instead you might find some abstractions which don't require another model
and another set of update messages. You can do lots with a view function
that takes a config record and an extensible record:

```elm
viewSidebar :
    { onClick : msg, onClose : msg }
    -> { a | sidebarItems : List SidebarItem }
    -> Html msg
```

As mentioned above, choose the simplest solution that works. Later you can
refactor and make it feel good. You can watch [this
talk](https://www.youtube.com/watch?v=DoA4Txr4GUs) by Richard Feldman if you want
to learn more.

A good point to reiterate is that, when writing Elm, you don't have to get
everything right from the start. The compiler gives you an unprecedented
level of confidence when refactoring code. So go out there and make
mistakes, then fix them, then keep going.

## Interacting with JavaScript

We mainly use [ports](https://guide.elm-lang.org/interop/ports.html) to
interact with JavaScript code. We have two simple rules for ports:

- All ports **must** return values as `Json.Value`. If a `Int -> Cmd msg`
  port receives a float instead, it will cause a crash. This is one of the
  simplest ways to introduce runtime errors in your application. By
  treating all values as JSON blobs, we must decode them and deal with the
  eventual decoding failure. Repeat with me, ports need JSON values.
- All port functions **must** be documented in the Elm module. We don't want to
  go hunting in JavaScriptLand how a port is being used. A couple of lines
  explaining what the port triggers can go a long way.

In cases where ports are not enough, we use [custom
elements](https://guide.elm-lang.org/interop/custom_elements.html). You
might want to do this for many reasons, such as reusing [existing React
components](https://github.com/cultureamp/react-elm-components) inside your
Elm application. In our case, we have integrated rich text editing using a
custom element that wraps [quilljs](https://quilljs.com/). If you're
interested in learning more about integrating custom elements in Elm, I
recommend watching [this great talk](https://www.youtube.com/watch?v=tyFe9Pw6TVE)
by Luke Westby.

## Testing

We write tests for our Elm code at four different layers:

- **Unit tests**: these are pure tests around data structures. Create a piece
  of data, run a function on it and assert some results.
- **View tests**: here we construct a model, pass it to the view
  function and write assertions against the result using `elm-explorations/test`.
- **Integration tests**: here we load up an auto-generated JSON file
    (using `rails_edge_test`, find it [here](https://github.com/NoRedInk/rails_edge_test)), pass it to
  the `elm-program-test` program, interact with the elements on the page,
  and assert side effects.
- **Acceptance tests**: we write these in Capybara as happy-path tests.
  They are extremely useful to test JavaScript interop.

I can recommend [this excellent
talk](https://www.youtube.com/watch?v=rIxCwPPA-D8) by Tessa Kelly if you
want to learn how to write testable Elm.

## Tooling

Here's a selection of tools that we use:

- [elm-format](https://github.com/avh4/elm-format), the Holy Grail of code
  formatters. I love repeating "garbage in, code out". In this
  case, it happens to be beautifully formatted, while respecting your preference
  in terms of whitespace, and applying tiny cleanups of your code on the
  fly. What's not to like?
- [elm-css](https://github.com/rtfeldman/elm-css), so that our CSS is
  typed. We use it so much that the `view` function in our `Nri.Program`
  returns a `Html.Styled.Html msg` by default.
- [accessible-html](https://github.com/tesk9/accessible-html), or how to
  ensure that accessibility is a first-class citizen in your app. I think
  that this library is also a great case study on how to create a light
  wrapper around another library.
- [elm-review](https://github.com/jfmengels/elm-review), a must-have
  addition to any Elm project. It will detect unused variables, unused
  variants, unused modules, and much much more. Use it!
- [elm-json-decode-pipeline](https://github.com/NoRedInk/elm-json-decode-pipeline),
  another approach at writing JSON decoders. I love how easy it is to read
  and modify decoders written in this style.

That's all for today, and if this sort of work interests you do check out [this
page](https://www.noredink.com/jobs). Thanks for reading 👋

_My eternal gratitude to the wonderful folks that have read through the drafts of
this post: [@juanedi](https://twitter.com/juanedi), [@brianhicks](https://twitter.com/brianhicks),
[@rtfeldman](https://twitter.com/rtfeldman), and
[@michaelglass](https://twitter.com/michaelglass)._
