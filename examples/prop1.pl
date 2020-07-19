eval(St,conj(X,Y),U) <- and(V,W,U), eval(St,X,V), eval(St,Y,W).
eval(St,disj(X,Y),U) <-  or(V,W,U), eval(St,X,V), eval(St,Y,W).
eval(St,var(X),U)    <- elem(X,St,U).

elem(o,[H|T],H).
elem(s(N),[H|T],V) <- elem(N,T,V).
and(f,f,f).
and(f,t,f).
and(t,f,f).
and(t,t,t).

or(f,f,f).
or(f,t,t).
or(t,f,t).
or(t,t,t).

<- eval(St,Fm,t).







