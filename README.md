# Mote for Emacs

This is an Emacs minor mode for interacting with [mote][mote], a tool for interactive Haskell development by [Izaak Meckler][im].

## Configuration

Put the mode in your load path and then add this to your `.emacs`:

    (require 'mote)

## Commands

<!-- Command documentation loosely based on the readme from the main mote repository. -->

Here are the core commands. You may want to give them keybindings for convenience.

  * `mote/init` - initializes the `mote` process and has to be called at least once before using the mode. It probably helps to add it to `haskell-mode-hook`:

    (add-hook 'haskell-mode-hook 'mote/init)

Before you use any other commands, you have to be in a hole. One of the following commands will enter a hole for you:

  * `mote/next-hole` - jumps to the first hole in front of the cursor

  * `mote/prev-hole` - jumps to the first hole behind the cursor

Once you're in a hole, you can do a few different things:

  * `mote/hole-info` - displays the expected type ("goal") and relevant bindings in scope

  * `mote/refine` - prompts for a function that produces the goal type and applies it to the hole:

        foo :: [a] -> Int
        foo xs = _

  and after `M-x mote-refine` and entering `length`

        foo :: [a] -> Int
        foo xs = length _

  Will error out if the function you give is not in scope or produces an incompatible type.

  * `mote/case-at-point` - expands the argument at the point into cases.

        foo :: [a] -> Int
        foo xs = _

    If you call `mote/case-at-point` with your cursor over `xs`, you'd get:

        foo :: [a] -> Int
        foo [] = _
        foo (x : xs') = _

  * `mote/case-on` - case expand the given expression in place of the hole

        foo :: [a] -> Int
        foo xs = _

    turns into

        foo :: [a] -> Int
        foo xs = case xs of
          [] -> _
          x : xs -> _

[mote]: https://github.com/imeckler/mote
[im]: http://parametricity.com/
