This repository contains all the code examples from the book "The Seasoned
Schemer." "The Seasoned Schemer" is the continuation of "The Little Schemer"
(http://bit.ly/4GjWdP) book. It's written in the same style as "The Little
Schemer", i.e., as a dialogue between you and the authors of the book, but the
goal is different - it teaches you to think about the nature of computation.

If you're interested, get the book from Amazon: http://bit.ly/8cyjgw

The code examples were copied (and completed where necessary) from
"The Seasoned Schemer" by Peteris Krumins (peter@catonmat.net).

His blog is at http://www.catonmat.net  --  good coders code, great reuse.

------------------------------------------------------------------------------

Table of contents:
    [11] Chapter 11: Welcome Back to the Show
         11-welcome-back-to-the-show.ss
    [12] Chapter 12: Take Cover
         12-take-cover.ss
    [13] Chapter 13: Hop, Skip, and Jump
         13-hop-skip-and-jump.ss
    [14] Chapter 14: Let There Be Names
         14-let-there-be-names.ss
    [15] Chapter 15: The Difference Between Men and Boys...
         15-men-and-boys.ss
    [16] Chapter 16: Ready, Set, Bang!
         16-ready-set-bang.ss
    [17] Chapter 17: We Change, Therefore We Are!
         17-we-change-therefore-we-are.ss
    [18] Chapter 18: We Change, Therefore We Are The Same!
         18-we-change-therefore-we-are-the-same.ss
    [19] Chapter 19: Absconding with the Jewels
         19-jewels.ss
    [20] Chapter 20: What's in Store?
         20-whats-in-store.ss


[11]-Chapter-11-Welcome-Back-to-the-Show--------------------------------------

See 11-welcome-back-to-the-show.ss file for code examples.

Chapter 11 welcomes you back to Scheme. It starts with a problem of
determining whether any atom occurs twice in a row. Next a problem of finding
the running sum of a list of numbers is solved.

From the solutions of these two problems the 11th commandment is postulated:

.----------------------------------------------------------------------------.
| The eleventh commandment:                                                  |
|                                                                            |
| Use additional arguments when a function needs to know what the other      |
| arguments to the function have been like so far.                           |
'----------------------------------------------------------------------------'

The chapter ends with the scramble problem. The problem is stated INCORRECTLY
and will confuse you.

It says "The result at each position is found by counting backward from the
current position to this index."

What it should say is this: "The result at each position is found by counting
backward from the NEXT position to this index." Or in other words:
a'_{i} = a{i+1 - a_{i}}.


[12]-Chapter-12-Take-Cover----------------------------------------------------

See 12-take-cover.ss file for code examples.

Chapter 12 introduces the letrec feature of Scheme that allows to remove
arguments that do not change for recursive applications and which hides and
protects functions (creates local scope).

Many examples are shows to make it clear how letrec works. Two commandments
are stated:

.----------------------------------------------------------------------------.
| The twelfth commandment                                                    |
|                                                                            |
| Use (letrec ...) to remove arguments that do not change for recursive      |
| applications.                                                              |
'----------------------------------------------------------------------------'

And

.----------------------------------------------------------------------------.
| The thirteenth commandment                                                 |
|                                                                            |
| Use (letrec ...) to hide and to protect functions.                         |
'----------------------------------------------------------------------------'

Now it's time for dessert.


[13]-Chapter-13-Hop-Skip-and-Jump---------------------------------------------

See 13-hop-skip-and-jump.ss file for code examples.

Chapter 13 teaches how to return values from functions abruptly and promptly
via letcc (aka. call-with-current-continuation).

.----------------------------------------------------------------------------.
| The fourteenth commandment                                                 |
|                                                                            |
| Use (letcc ...) to return values abruptly and prompty.                     |
'----------------------------------------------------------------------------'

This chapter revisits intersect and intersectall functions from The Little
Schemer and rewrites them by applying 12th, 13th and 14th commandments.


[14]-Chapter-14-Let-There-Be-Names--------------------------------------------

See 14-let-there-be-names.ss file for code examples.

Chapter 14 introduces the (let ...) expression that is used to give
expressions names and to avoid evaluating them twice.

The usage of let is first stated as a preliminary version of fifteenth
commandment:

.----------------------------------------------------------------------------.
| The fifteenth commandment (preliminary version)                            |
|                                                                            |
| Use (let ...) to name the values of repeated expressions.                  |
'----------------------------------------------------------------------------'

And as more examples are worked through, as a revised edition:

.----------------------------------------------------------------------------.
| The fifteenth commandment (revised version)                                |
|                                                                            |
| Use (let ...) to name the values of repeated expressions in a function     |
| definition if they may be evaluated twice for one and the same use of the  |
| function.                                                                  |
'----------------------------------------------------------------------------'

The final version of the fifteenth commandment is yet to come.


[15]-Chapter-15-The-Difference-Between-Men-and-Boys---------------------------

See 15-men-and-boys.ss file for code examples.

Chapter 15 teaches you how to use the (set! ...) expression and how to let
functions remember values.

It presents three commandments:

.----------------------------------------------------------------------------.
| The sixteenth commandment                                                  |
|                                                                            |
| Use (set! ...) only with names defined in (let ...)s.                      |
|                                                                            |
'----------------------------------------------------------------------------'

.----------------------------------------------------------------------------.
| The seventeenth commandment (preliminary version)                          |
|                                                                            |
| Use (set! ...) for (let ((x ...)) ...) only if there is at least one       |
| (lambda ... between it and the (let ((x ...)) ...).                        |
'----------------------------------------------------------------------------'

.----------------------------------------------------------------------------.
| The eighteenth commandment                                                 |
|                                                                            |
| Use (set! x ...) only when the value that x refers to is no longer needed. |
'----------------------------------------------------------------------------'


[16]-Chapter-16-Ready-Set-Bang!-----------------------------------------------

See 16-ready-set-bang.ss file for code examples.

Chapter 16 plays a lot around creating state variables via set!. A state
variable remembers its value between two distinct function calls.

The nineteenth commandment follows:

.----------------------------------------------------------------------------.
| The nineteenth commandment                                                 |
|                                                                            |
| Use (set! ...) to remember valuable things between two distinct uses of a  |
| function.                                                                  |
'----------------------------------------------------------------------------'

Then we return to Y-combinators again. The Y! (Y-bang) combinator is
introduced that's "the applicative-order, imperative Y combinator. It works
almost like the regular Y-combinator, but not quite.

The final version of the seventeenth commandment is presented:

.----------------------------------------------------------------------------.
| The seventeenth commandment (final version)                                |
|                                                                            |
| Use (set! x ...) for (let ((x ...)) ...) only if there is at least one     |
| (lambda ... between it and the (let ...), or if the new value for x is a   |
| function that refers to x                                                  |
'----------------------------------------------------------------------------'


[17]-Chapter-17-We-Change-Therefore-We-Are------------------------------------

See 17-we-change-therefore-we-are.ss file for code examples.

Chapter 17 continues to improve the paranthesis pizza wrapping function and
still teaches how to use set! and let correctly.

It also looks at how to count the number of cons calls in a function by using
set!.


[18]-Chapter-18-We-Change-Therefore-We-Are-The-Same---------------------------

See 18-we-change-therefore-we-are-the-same.ss file for code examples.

Chapter 18 implements car, cdr and cons through lambdas. It also talks about
what it means for two lists to be equal and introduces set-cdr! function.


[19]-Chapter-19-Absconding-with-the-Jewels------------------------------------

See 19-jewels.ss file for code examples.

Chapter 19 talks more about continuations and teaches how to use them to
create a generator get-next that yields one value on each function call.


[20]-Chapter-20-What's-in-Store?----------------------------------------------

See 20-whats-in-store.ss file for code examples.

... Going to write about it after I finish assembling code examples from The
Little Schemer, as it's based on knowledge of Chapter 10 of The Little
Schemer.


------------------------------------------------------------------------------

That's it. I hope you find these examples useful when reading "The Seasoned
Schemer" yourself! Go get it at http://bit.ly/8cyjgw, if you haven't already!


Sincerely,
Peteris Krumins
http://www.catonmat.net

