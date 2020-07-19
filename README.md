# Conjunctive Partial Deduction

An implementation of conjunctive partial deduction as described in the paper "Conjunctive Partial Deduction Revisited"

The implementation can be built and executed using stack.

The execution is a REPL, with the prompt "LOG> " and the following commands:

LOG> :help

:load filename  To load the given filename
:prog           To print the current program
:eval           To evaluate the current goal
:trans          To transform the current program
:quit           To quit
:help           To print this message

The first thing to do is to load a program file:

LOG> :load dappend

This will load the program dappend.pl (the.pl extension is assumed).

To see the contents of this program:

LOG> :prog
append([],YS,YS).
append([X|XS],YS,[X|ZS]) <- append(XS,YS,ZS).
<- append(XS,YS,ZS),append(ZS,VS,WS).

To apply the conjunctive partial deduction transformation to the current program:

LOG> :trans
p([X|XS'],YS,[X|ZS'],VS,[X|ZS'']) <- p(XS',YS,ZS',VS,ZS'').
p([],ZS,ZS,VS,WS) <- p'(ZS,VS,WS).
p'([X|XS'],VS,[X|ZS']) <- p'(XS',VS,ZS').
p'([],WS,WS).
<- p(XS,YS,ZS,VS,WS).

To evaluate the current program:

LOG> :eval

This will prompt for values of the free variables. 
The terms entered for these values may themselves contain free variables which will be instantiated during evaluation

XS = [1,2,3]
YS = [4,5,6]
ZS = ZS
VS = [7,8,9]
WS = WS
True
ZS = [1,2,3,4,5,6]
WS = [1,2,3,4,5,6,7,8,9]

To quit from the program:

LOG> :quit
