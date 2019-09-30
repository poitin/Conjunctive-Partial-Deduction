nrev([],[]).
nrev([X|XS],YS) <- nrev(XS,ZS),append(ZS,[X],YS).

append([],YS,YS).
append([X|XS],YS,[X|ZS]) <- append(XS,YS,ZS).

<- nrev(XS,YS).





