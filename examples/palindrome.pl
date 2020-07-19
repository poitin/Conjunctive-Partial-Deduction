nrev([],[]).
nrev([X|XS],YS) <- nrev(XS,ZS),append(ZS,[X],YS).

append([],YS,YS).
append([X|XS],YS,[X|ZS]) <- append(XS,YS,ZS).

palindrome(XS) <- nrev(XS,XS).

<- palindrome(XS).





