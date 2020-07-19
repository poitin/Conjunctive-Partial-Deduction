# Conjunctive Partial Deduction

An implementation of conjunctive partial deduction as described in the paper "Conjunctive Partial Deduction Revisited"

The implementation can be built and executed using stack.

The execution is a REPL, with the prompt "LOG> " and the following commands:

LOG> :help

:load filename  To load the given filename<br>
:prog           To print the current program<br>
:eval           To evaluate the current goal<br>
:trans          To transform the current program<br>
:quit           To quit<br>
:help           To print this message<br>

The first thing to do is to load a program file:

LOG> :load dappend

This will load the program dappend.pl (the.pl extension is assumed).

To see the contents of this program:

LOG> :prog<br>
append([],YS,YS).<br>
append([X|XS],YS,[X|ZS]) <- append(XS,YS,ZS).<br>
<- append(XS,YS,ZS),append(ZS,VS,WS).<br>

To apply the conjunctive partial deduction transformation to the current program:

LOG> :trans<br>
p([X|XS'],YS,[X|ZS'],VS,[X|ZS'']) <- p(XS',YS,ZS',VS,ZS'').<br>
p([],ZS,ZS,VS,WS) <- p'(ZS,VS,WS).<br>
p'([X|XS'],VS,[X|ZS']) <- p'(XS',VS,ZS').<br>
p'([],WS,WS).<br>
<- p(XS,YS,ZS,VS,WS).<br>

To evaluate the current program:

LOG> :eval

This will prompt for values of the free variables. <br>
The terms entered for these values may themselves contain free variables which will be instantiated during evaluation

XS = [1,2,3]<br>
YS = [4,5,6]<br>
ZS = ZS<br>
VS = [7,8,9]<br>
WS = WS<br>
True<br>
ZS = [1,2,3,4,5,6]<br>
WS = [1,2,3,4,5,6,7,8,9]<br>

To quit from the program:

LOG> :quit
