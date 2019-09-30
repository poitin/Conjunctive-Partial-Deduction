qrev(XS,YS) <- qrev2(XS,[],YS).

qrev2([],YS,YS).
qrev2([X|XS],YS,ZS) <- qrev2(XS,[X|YS],ZS).

<- qrev(XS,YS).





