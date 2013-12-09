
% 0, true and false are builtin in prolog
ifthenelse(X,Y,Z) :- term(X), term(Y), term(Z).
succ(X) :- term(X).
pred(X) :- term(X).
iszero(X) :- term(X).

term(0).
term(true).
term(false).
term(ifthenelse(_,_,_)).

numericvalue(0).
numericvalue(succ(X)) :- numericvalue(X).

value(true).
value(false).
value(X) :- numericvalue(X).

%%% evaluation

% e-succ
ev(succ(A), T) :- 
		ev(A, B),
		ev(succ(B), T), !.

% e-pred
ev(pred(A), T) :- 
		ev(A, B), 
		ev(pred(B), T), !.
% e-predsucc
ev(pred(succ(A)), A).
% e-predzero
ev(pred(0), 0).

% e-iszero
ev(iszero(A), T) :- 
		ev(A, B), 
		ev(iszero(B), T), !.
% e-iszerozero
ev(iszero(0), true).
% e-iszerosucc
ev(iszero(succ(_)), false).

ev(ifthenelse(A, B, C), T) :- 
		ev(A, A2),
		ev(ifthenelse(A2, B, C), T), !.
ev(ifthenelse(true, A, _), A).
ev(ifthenelse(false, _, B), B).
