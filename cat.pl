:- module(cat, [
    arg_in/2,
    cat_index/2,
    cat_number/3,
    res_in/2]).

:- use_module(slashes).

%%	cat_index(+Cat, -CO)
%
%	Replaces categories with category objects, where basic categories are
%	of the form co(Cat:F, Cat, I), where F is the (possibly variable)
%	feature and I is a variable index. The special categories conj, lrb,
%	rrb, ., ,, and ; are replaced by variables.
cat_index(conj, (_\_)/_) :-
  !.
cat_index(lrb, _) :-
  !.
cat_index(rrb, _) :-
  !.
cat_index(., _) :-
  !.
cat_index(,, _) :-
  !.
cat_index(;, _) :-
  !.
cat_index(X0\Y0, X\Y) :-
  !,
  cat_index(X0, X),
  cat_index(Y0, Y).
cat_index(X0/Y0, X/Y) :-
  !,
  cat_index(X0, X),
  cat_index(Y0, Y).
cat_index(B:F, co(B:F, B, _)) :-
  !.
cat_index(B, co(B:_, B, _)).

%%	cat_number(+CO, +M, -N)
%
%	Replaces variable indices in category objects with integers, starting
%	from M.
cat_number(co(_, _, I), M, N) :-
  !,
  (  var(I)
  -> I = M,
     N is M + 1
  ;  N = M
  ).
cat_number(X/Y, M, N) :-
  cat_number(X, M, O),
  cat_number(Y, O, N).
cat_number(X\Y, M, N) :-
  cat_number(X, M, O),
  cat_number(Y, O, N).

arg_in(Y, _\Y).
arg_in(Y, _/Y).
arg_in(Y, X\_) :-
  arg_in(Y, X).
arg_in(Y, X/_) :-
  arg_in(Y, X).

res_in(X, X).
res_in(X1, X2/_) :-
  res_in(X1, X2).
res_in(X1, X2\_) :-
  res_in(X1, X2).
