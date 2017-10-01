# lark

[Not yet released]

Open-source components of the [Lark Editor](https://www.lark.cloud).

> A lighthearted, fun episode is a lark. You could describe the wonderful day you spent with friends exploring little fishing towns along the coast of Maine as a lark. Often unplanned, a lark can happen when you are feeling adventurous. You might decide on a lark to audition for a reality show — and be picked! The act of trying something new like this can also be called larking.
>
> — [Vocabulary.com](https://www.vocabulary.com/dictionary/lark)

[Lark](https://lark.cloud) is the new editor I am building to continue my last couple of years of work on the design of interactive programming environments. Previous waypoints on this journey include my submission to the Programming Experience workshop ([PX/16](http://programming-experience.org/px16/)) in Rome entitled [_When I Sit Down At My Editor, I Feel Relaxed_](http://px16.matt.is), and a beginner's coding environment called [Maria](https://www.maria.cloud), which features a fantastic [programming curriculum](https://www.maria.cloud/curriculum) for Clojure written by [Dave Liepmann](http://www.daveliepmann.com/). Maria is a work-in-progress and is currently being informally tested as a tool for [programming workshops](http://clojurebridge-berlin.org/).

The purpose of Lark is to make new kinds of experimentation _and real-world making-of-stuff_ possible, for more diverse sets of people: folks who do not see themselves as programmers, or as technical per se, but are curious and want to gain power and confidence in this computational world we have inherited. We will use whatever means are most graspable. The learning curve should be on the order of Microsoft Excel, but with more interesting motivational nuggets of delight to be discovered along the way. I believe there are powerful ideas in programming which are no more difficult than grade-school algebra, but have not been made sufficiently accessible or compelling to the average human. 

I aim as soon as possible to release a prototype fit to be tested by anyone who is interested, and would like to be as experimental with modes of feedback as with the environment itself. 

Clojure is the substrate on which Lark is constructed, because I find it to be a wonderfully flexible tool for thinking, but Lark is not about Clojure per se.

I give the [Recurse Center](https://www.recurse.com/) credit for introducing me to Clojure in 2014 (thanks [Zach](https://twitter.com/riotonthebay), [Bert](https://twitter.com/stijlist), and [Giorgio](https://github.com/ppold)), and [Jack Rusher](http://www.jackrusher.com) for a never-ending supply of probing questions, deep insight, historical context, and _en pointe_ feedback ever since we met in Berlin in 2015. In addition, _many_ others in the [ClojureScript](https://clojurescript.org/) world have been generous with their time and input, both in-person and in the wonderful Clojurians Slack community.

Lark is a project of BrainTripping, Inc., a company I founded in 2012 to explore interesting ideas in programming, beginning with [BrainTripping](https://matt.is/writing/introducing-braintripping) itself.

### What's here?

This repository will contain pieces of the editor which are fit to be reused in other projects, or necessary to extend functionality of the editing environment (when such facility becomes possible). Please see each subdirectory for licensing details.

So far this repository contains:


1. **Cells**: a fun library for interactive, exploratory asynchronous programming. A great way to introduce beginners to things like fetching docs from the internet, or views on a timer. See [Welcome to Cells](https://www.maria.cloud/cells) on Maria.
2. **Commands**, a library for defining context-aware commands with keybindings.
3. A couple other components which are more in flux, relating to the defining of content-type-specific editor blocks and structural editing.
