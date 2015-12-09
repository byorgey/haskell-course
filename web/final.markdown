---
title: Final project
---

Overview/important dates
------------------------

For CIS 194 you will complete a final project which will tie together
some of the things you have learned and give you some practical
Haskell development experience.  The expectation is for you to spend
around **10-15 hours** working on the project.  Here are some
important dates:

  * **Monday, March 18** -- Project proposals due
  * **April 8--12** -- Checkpoint meetings
  * **Tuesday, April 23** -- Final project submission deadline

Get started early!

Resources
---------

Here is a document explaining a few things relevant to
[Haskell programming in the large](/docs/inthelarge.pdf).

Format
------

You may work by yourself, or in groups of up to *three* students.
Note, however, that projects for groups of three will be held to
somewhat higher standards than those for individuals or pairs.  Groups
of five are *right out*.

There are two types of projects you may complete:

1. Application/library

    For your project you may write some sort of Haskell application or
    library which does something fun/useful/interesting.  Your
    imagination is the limit.  Some possibilities/suggestions include:

    + A program to play a game (like tic-tac-toe, Connect 4, othello,
      gomoku, poker, mancala, ...) against the user.

    + A program to solve puzzles like sudoku or kenken.

    + A program to generate random mazes and let the user
      interactively solve them, or to solve mazes input by the user.

    + An implementation of some interesting data structure like
      red-black trees, 2-3-4 trees, binomial heaps, or Fibonacci heaps.

    + An parser and interpreter for a small programming language, such
      as a
      [while language](http://www.program-analysis.com/while.html).

    + A raytracer.

    + Create a web site using [hakyll](http://jaspervdj.be/hakyll/).

    + Take an interesting program you have written in some other
      language, and figure out how to port/re-implement it in
      idiomatic Haskell.

    + Whatever else your creativity suggests!

2. Open-source contribution

    For your project you may choose an open-source library or
    application on [Hackage](http://haskell.org/hackage/) to
    contribute to.  Contributions may include bug fixes, new features,
    and/or documentation.  Here are a few suggestions---these are
    projects whose authors/maintainers have indicated that there would
    be good ways for beginning Haskell students to contribute.  (But
    you are free to work on any project you like, as long as you can
    find a reasonable way to contribute.)  If you want to try
    contributing to one of these projects, you should contact the
    relevant person(s) and discuss it with them prior to submitting
    your project proposal.

    **Note**: if you are thinking of making an open-source
      contribution you may turn in your proposal by **Monday, March
      25**.

    Open-source projects students have contributed to in prior years
    include a package to efficiently compute prime numbers using a
    mutable-array-based sieve and Haskell bindings to the Kinect.

    * [snowdrift.coop](http://snowdrift.coop) (Contact: [David Thomas](mailto:davidleothomas@gmail.com))

        I'm working with some people putting together
        <http://snowdrift.coop>, which is built on
        [Yesod](http://www.yesodweb.com/).  We've got a lot of tasks
        on our plate, of various sizes and scopes and time-lines, and
        if students are interested I'd be happy to work with you to
        come up with some appropriate projects, and do as much hand
        holding as is necessary thereafter.

    * [taffybar](https://github.com/travitch/taffybar) (Contact: [Tristan Ravitch](mailto:travitch@cs.wisc.edu))

        I have an [Xmonad](http://xmonad.org) status bar called
        [taffybar](https://github.com/travitch/taffybar).  It is meant
        for xmonad, but does not truly require it.  It uses
        [gtk2hs](http://projects.haskell.org/gtk2hs/) and is basically
        just a horizontal list of widgets.  Each widget is entirely
        separate from the others, and is just a gtk widget that can do
        whatever it wants.  The codebase is actually pretty simple as
        long as they are familiar with doing IO in Haskell.

        A few possible ideas of varying complexity:

        * Add a new backend to the weather widget (e.g., Weather Underground).
          This would involve parsing whatever response Weather Underground
          returns for its queries.

            Possibly on the high end for what they are prepared for

         * Add weather icons for current weather conditions (rainy, cloudy, etc)

            Probably on the easier end - it would just require conditionally adding
            the appropriate icon whenever the weather changes.

         * Add a launcher widget.  It would launch programs typed into a text box
          (possibly triggered by a keystroke or always visible).  That step is
          very easy.  More interesting work could be done in adding tab
          completion for commands, or possibly modal commands.

            A modal command system might be worth its own project separately from
            a widget if it was plugin based.  For example, a 'note' plugin would
            add a command like

                note NAME CONTENT...

            where the command would save CONTENT to a file named NAME somewhere
            sensible.  Launching programs could just be another plugin.

            This can be as complex as desired and is still useful in its simpler
            stages.

    * EPUB reader for [pandoc](http://johnmacfarlane.net/pandoc/)
      (Contact: [John MacFarlane](mailto:jgm@berkeley.edu)) 

        I'll just resuggest what I suggested last year:

        ...implement an EPUB reader for pandoc.

        This would have to unzip the input file, look in the metadata
        to see what's there, extract the chapters and parse them with
        pandoc's HTML reader, and do something with images and the
        like (maybe make them into data: URIs -- see
        [Text.Pandoc.SelfContained](https://github.com/jgm/pandoc/blob/master/src/Text/Pandoc/SelfContained.hs)).

		It would be handy to be able to convert an epub directly into
		to markdown, HTML, LaTeX, docx, or PDF. And the pieces are all
		pretty much there in pandoc already.

		One fun application I see for this would be a command line
		tool that converts the epub to texinfo, then converts this
		into info and uses info to browse the epub.  Essentially:
		a cheap command-line epub browser.

    * [hnn](https://github.com/alpmestan/hnn) examples (Contact: [Alp Mestanogullari](mailto:alpmestan@gmail.com))

		My suggestion may sound a bit odd, but if they're looking for
		a challenging but still simple enough project, I'd love for
		people to test out the new version of hnn (not yet released,
		but [on github](https://github.com/alpmestan/hnn) and make
		something fun with it. I'd love to mentor this and add things
		to the library altogether as they progress and give some
		feedback. The biggest issue with that proposal is that they
		either have to know a bit about neural networks before or must
		be able to learn very quickly. This can however be compensated
		by that warm feeling you have when your neural net finally
		does what you want it to.

    * [MoCap](https://github.com/netogallo/MoCap) (Contact: [Ernesto Rodriguez](mailto:neto@netowork.me))

		Not something very big, but if someone wants to get hands on working
		with Parsec I started developing a library to work with motion capture
		(MoCap) data. I need to parse MoCap data for my
		[bachelor's thesis](https://github.com/netogallo/LambdaNN) so I
		decided to do it in a way that might benefit others. On the other
		hand, I don't need all the info in the file for my work and this
		project is not my priority, so one of the students could extend the
		parser to parse the sections of the file it currently ignores. Also
		more file types support would be nice since I am only developing a
		parser for ASF/AMC files. The project is very new, small and is
		located here: <https://github.com/netogallo/MoCap> .  I can give you
		more details of the tasks that could be done (essentially creating a
		parser fore more sections of the file, define ADTs to represent those
		sections, semantic validation of files, better error messages, etc.)
		and I would be willing to exchange e-mails and answer questions to
		students if necessary.

    * [diagrams](http://projects.haskell.org/diagrams) (Contact: Brent Yorgey)

        diagrams is a framework for creating vector graphics by
        writing Haskell code which is a *description* of the graphics
        you would like.  There are many small tasks which could be
        done: creating more examples for the
        [gallery](http://projects.haskell.org/diagrams/gallery.html),
        adding some specialized module(s) to the
        [`diagrams-contrib` package](https://github.com/diagrams/diagrams-contrib)
        (*e.g.* a module for drawing representations of Turing
        machines, or for visualizing sorting algorithms, or ...),
        adding support for
        [rounded polygons and paths](https://github.com/diagrams/diagrams-lib/issues/36),
        [grid layout](https://github.com/diagrams/diagrams-lib/issues/33),
        or [gradients](https://github.com/diagrams/diagrams-lib/issues/9)...

		**Note**: if you would like to contribute to diagrams, I will
        happily provide guidance and so on, but in order to make
        things fair only the TAs will grade your final project.  Hence
        this is not a way to get an easy A.

Project proposal
----------------

You must submit a project proposal by **Monday, March 18** (unless you
are thinking about working on an open-source contribution, in which
case you have until **Monday, March 25** in order to have time to
communicate with the maintainer(s).  This gives us a chance
to discuss your proposal and ensure it will make a suitable project.
You are encouraged to submit your proposal earlier than March 18 if
you already have an idea.  You should also feel free to submit several
project proposals if you would like help deciding which is most
suitable.

To submit your proposal, send an email of a few paragraphs to me
(byorgey at cis) with the subject "CIS 194 final project proposal".
Try to answer the questions: What do you propose to do?  What do you
hope to learn from the project?  What are some concrete goals,
i.e. how will we judge the success of your project?

Checkpoint
----------

Sometime during the week of April 8-12 or thereabouts, you should
[schedule a meeting](https://piazza.com/class#spring2013/cis194/96)
with one of Brent, Zach, or Adi to show off the progress you have made
on your project, get any guidance or help you might need, and discuss
your plans for completing the project.  Note that *the checkpoint
meeting will constitute part of your final project grade*, though
there's no need to stress out about it.  Concretely, by the checkpoint
you should:

  * Have some code to show off and explain to us
  * Have a few questions for us: things you are wondering about,
      confused about, struggling with, *etc.*

The purpose of the meetings is twofold: to make sure you get started
on your project before the last minute, and to provide a convenient
space for you to ask questions and get some direction and feedback.

Final submission
----------------

Final submissions are due by **Tuesday, April 23**.  Extensions to the
final deadline will be cheerfully granted, but you *must ask for one
by **Tuesday, April 16*** (one week in advance).  Simply choose your
own deadline---up to and including the last day of finals, **May
7**---and that will be your new deadline.

Your final submission should consist of any and all code you have
written, along with a document describing your project (a simple text
file is fine).  The document should contain

  * a description of your project and what you accomplished;

  * instructions on how to compile/run/try out/play with your project;

  * a description of work you did and things you learned along the way.

You may submit your project in one of two ways:

  * By emailing me a `.tar`, `.tgz` or `.zip` file

  * By emailing me a link to a publically accessible source
    repository, *e.g.* on github, bitbucket, or hub.darcs.net. Be sure
    to submit a link to one or more specific commits, tags, *etc.*
    representing what you would like to be graded, rather than just to
    the repository in general.

Grading will be as follows:

  * Checkpoint (25%).  Did you make some progress on your
    project by the time of the checkpoint meeting?

  * Style (25%).  Your project should use good Haskell style and be
    well-documented.

  * Correctness (25%).  Your project should be free of compilation
    errors and should correctly accomplish whatever it is supposed to
    accomplish.  This means that if the deadline is looming, your time
    would be better spent fixing bugs in what you already have than
    adding one last feature.

  * Effort/accomplishment (25%).  We will be looking for evidence that
    you put energy and effort (~10-15 hours) into your project and
    that you have learned something.  This is where the document you
    submit along with your project comes in: be sure to use it to
    highlight work you did and things you learned, especially if it is
    not obvious from looking at the final product.  For example, if
    you spent two hours trying an approach that ultimately did not
    work, you should write about that and what you learned from the
    experience.  However, we will not necessarily look with sympathy
    on *unnecessary* work: for example, if you spent five hours trying
    to track down a bug without asking for help, that's just plain
    silly stubbornness.  If you are stuck on something, please ask for
    help.  We want you to spend your time making progress on your
    project, not banging your head against a wall (although a small
    amount of head-banging can be healthy).
