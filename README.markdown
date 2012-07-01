Flip
====

*Flip* is a very simple computer game by John S. James which first appeared
in the March/April 1977 edition of *Creative Computing*.

In the game, the computer flips a virtual coin 50 times, and the object is
for you to guess whether the coin will come up heads or tails each time.

What makes it interesting is that the coin is not fair.  The computer tries
to find patterns in your guesses, and exploit them by biasing the coin toss
away from what it thinks you are likely to guess next.

This is interesting because humans are actually not all that good at moving
randomly; we tend to and fall into patterns.  So a pigeon, not thinking
about the game and pecking haphazardly at "heads" and "tails" buttons, might
actually be a better Flip player than you or me.

You can think of the program as creating a Markov chain to model your
guesses, and updating it each time you make a guess, based on your last few
guesses.  Although, it's not quite that straightforward -- there is some
randomness thrown in, too.

This version of the game is written in Erlang, based largely on the version
written in BASIC by Steve North appearing in *More BASIC Computer Games*,
Ed. David H. Ahl (ISBN 0-89480-137-6).  Note that in this implementation,
heads and tails are called `Y` and `N`.

Running
-------

Start an Erlang shell, load the `flip` module, and run `flip:start()`.
