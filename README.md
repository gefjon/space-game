# some out-of-this-world hacking i've been doing

  i'm running these in 64-bit ccl on macos mojave. with quicklisp,
  it's as simple as cloning this repo into
  `~/quicklisp/local-projects/` and then doing `(ql:quickload
  :space-game)`. because of my (lazy af) debugging strategy, that will
  create an instance of `space-game:space-game`, which is a subclass
  of `sketch:sketch`, and thus `kit.sdl2:window`, so it'll create a
  window when you load it. if you don't want that... i guess, comment
  out the initial value of `*the-game*` (but leave the declaration
  intact) in [space-game.lisp].

  when hacking in slime, i've found that it helps to run ccl in a
  terminal/as a standalone process, evaluate `(ql:quickload :swank)`
  and then `(swank:create-server)`, and then do `M-x slime-connect`
  from within emacs and hack from there - this helps the operating
  system keep track of which windows it has to kill when a hack
  crashes and to not kill your text editor along with it.
