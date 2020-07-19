len([],zero).
len([H|T],succ(L)) <- len(T,L).

max(Ls,M) <- max1(Ls,zero,M).

max1([],M,M).
max1([H|T],N,M) <-
	max1(T,N,M),le(H,N).
max1([H|T],N,M) <-
	max1(T,H,M),gt(H,N).

le(zero,X).
le(succ(X),succ(Y)) <- le(X,Y).

gt(succ(X),zero).
gt(succ(X),succ(Y)) <- gt(X,Y).

maxlen(Ls,M,L) <- max(Ls,M),len(Ls,L).  

<- maxlen(Ls,M,L).





