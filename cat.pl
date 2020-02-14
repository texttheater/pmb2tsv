:- module(cat, [
    arg_in/2,
    cat_dir/2,
    cat_id/2,
    cat_index/2,
    cat_is_pseudo/1,
    cat_match/2,
    cat_role/2,
    res_in/2]).

:- use_module(slashes).

%%	cat_index(+Cat, -CO)
%
%	Replaces categories with category objects. The category object for
%	=|X0/Y0|= (=|X0\Y0|=) is =|f(_, X, Y)|= (=|b(_, X, Y)|=) where X (Y) is
%	the category object for X0 (Y0). The category object for a basic
%	category =|A:F|= with a feature F is =|a(_, A:F, A)|=. The category
%	for a featureless basic category A is =|a(_, A:_, A)|=. The category
%	object for the special category =conj= is =|f(_, b(_, _, _), _)|=. The
%	category object for the special categories =lrb=, =rrb=, =.=, =,=, and
%	=;= is a variable.
cat_index(conj, f(_, b(_, _, _), _)) :-
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
cat_index(X0\Y0, b(_, X, Y)) :-
  !,
  cat_index(X0, X),
  cat_index(Y0, Y).
cat_index(X0/Y0, f(_, X, Y)) :-
  !,
  cat_index(X0, X),
  cat_index(Y0, Y).
cat_index(B:F, a(_, B:F, B)) :-
  !.
cat_index(B, a(_, B:_, B)). 

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

cat_dir(X/_, Dir) :-
  cat_dir(X, Dir).
cat_dir(X\_, Dir) :-
  cat_dir(X, Dir).
cat_dir(co(_, _, _, Dir, _), Dir).

cat_role(X/_, Role) :-
  cat_role(X, Role).
cat_role(X\_, Role) :-
  cat_role(X, Role).
cat_role(co(_, _, _, _, Role), Role).

cat_id(X/_, ID) :-
  cat_id(X, ID).
cat_id(X\_, ID) :-
  cat_id(X, ID).
cat_id(co(_, _, ID, _, _), ID).

cat_is_pseudo((A/(B\C))/D) :-
  A == B,
  C == D,
  !.
cat_is_pseudo((A\(B/C))/D) :-
  A == B,
  C == D,
  !.

% match category objects against plain categories
cat_match(X0/Y0, X/Y) :-
  cat_match(X0, X),
  cat_match(Y0, Y).
cat_match(X0\Y0, X\Y) :-
  cat_match(X0, X),
  cat_match(Y0, Y).
cat_match(co(F0:A0, _, _, _, _), F:A) :-
  nonvar(F0),
  !,
  F0:A0 = F:A.
cat_match(co(F0:_, _, _, _, _), F) :-
  nonvar(F0),
  F0 = F.
