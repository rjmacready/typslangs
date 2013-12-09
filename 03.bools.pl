
% true and false are builtin in prolog
ifthenelse(X,Y,Z) :- term(X), term(Y), term(Z).

term(true).
term(false).
term(ifthenelse(_,_,_)).

value(true).
value(false).

ev(ifthenelse(true, A, _), A) :- !.
ev(ifthenelse(false, _, B), B) :- !.
ev(ifthenelse(A, B, C), T) :- 
		ev(A, A2),
		ev(ifthenelse(A2, B, C), T).
