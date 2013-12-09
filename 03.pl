
%% terms

% true, false and 0 are prolog builtins and terms.
% 
ifthenelse(X,Y,Z) :- term(X), term(Y), term(Z).
succ(X) :- term(X).
pred(X) :- term(X).
iszero(X) :- term(X).


term(true).
term(false).
term(0).
term(ifthenelse(_, _, _)).
term(succ(_)).
term(pred(_)).
term(iszero(_)).


%%% Induction on terms

consts(true, [true]).
consts(false, [false]).
consts(0, [0]).
consts(succ(X), consts(X)).
consts(pred(X), consts(X)).
consts(iszero(X), consts(X)).
consts(ifthenelse(X, Y, Z), R) :- 
		consts(X, X2),
		consts(Y, Y2),
		consts(Z, Z2),
		union(X2, Y2, R2), 
		union(Z2, R2, R).

size(true, 1).
size(false, 1).
size(0, 1).
size(succ(X), R) :- size(X, R2), plus(1, R2, R).
size(pred(X), R) :- size(X, R2), plus(1, R2, R).
size(iszero(X), R) :- size(X, R2), plus(1, R2, R).
size(ifthenelse(X, Y, Z), R) :-
		size(X, RX),
		size(Y, RY),
		size(Z, RZ),
		plus(RY, RZ, T1),
		plus(RX, T1, T2),
		plus(1, T2, R).

max(X, Y, X) :- >(X, Y), !.
max(_, X, X).

depth(true, 1).
depth(false, 1).
depth(0, 1).
depth(succ(X), R) :- size(X, R2), plus(1, R2, R).
depth(pred(X), R) :- size(X, R2), plus(1, R2, R).
depth(iszero(X), R) :- size(X, R2), plus(1, R2, R).
depth(ifthenelse(X, Y, Z), R) :-
		depth(X, RX),
		depth(Y, RY),
		depth(Z, RZ),
		max(RX, RY, R2),
		max(RZ, R2, R3),
		plus(1, R3, R).

