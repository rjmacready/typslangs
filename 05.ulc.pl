
% ast

variable(_).
abstr(variable(_), E) :- term(E).
app(A, B) :- term(A), term(B).

% terms

term(variable(_)).
term(abstr(_, _)).
term(app(_, _)).

% values

value(abstr(_, _)). 

% helper functions

% ast-equals(variable(A), variable(A)).

%% substitution

% subst(the-variable, the-value, the-expr, result)

subst(V, B, V, B) :- 
		V = variable(_), !.

% Two different vars, we are here because
% The previous rule failed
subst(V, _, V2, V) :-
		V = variable(_),
		V2 = variable(_), !.

subst(V, _, A, A) :- 
		V = variable(_),
		A = abst(V, _), !.

subst(V, B, A, abst(V2, T)) :-
		V = variable(_),
		A = abst(V2, E),
		subst(V, B, E, T), !.

subst(V, B, app(L, R), app(TL, TR)):-
		V = variable(_),
		subst(V, B, L, TL),
		subst(V, B, R, TR), !.

% Fail through
subst(_, _, E, E).


% evaluate

ev(app(L, R), T) :- 
		ev(L, L2),
		ev(app(L2, R), T), !.

ev(app(L, R), T) :- 
		value(L),
		ev(R, R2),
		ev(app(L, R2), T), !.

ev(app(L, R), T) :-
		L = lambda(V, E),
		value(R),
		subst(V, R, E, T), !.



% evaluation

% e-app1
ev(app(A, B), T) :-
		 ev(A, A2),
		 ev(app(A2, B), T).

% e-app2
ev(app(A, B), T) :-
		 ev(B, B2),
		 ev(app(A, B2), T).

% e-appabs
ev(app(abs(_, _), A), A).

