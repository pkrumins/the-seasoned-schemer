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
    ...
    work in progress, adding new chapters every other day


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


------------------------------------------------------------------------------

That's it. I hope you find these examples useful when reading "The Seasoned
Schemer" yourself! Go get it at http://bit.ly/8cyjgw, if you haven't already!


Sincerely,
Peteris Krumins
http://www.catonmat.net

