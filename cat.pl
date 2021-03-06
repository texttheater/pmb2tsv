:- module(cat, [
    arg_in/2,
    cat_dir/2,
    cat_id/2,
    cat_index/2,
    cat_is_pseudo/1,
    cat_match/2,
    cat_number/1,
    cat_number/3,
    cat_role/2,
    res_in/2]).

:- use_module(slashes).

%%	cat_index(+Cat, -CO)
%
%	Replaces categories with category objects, where basic categories are
%	of the form co(Cat:F, Cat, I, Dir, Role), where F is the (possibly
%       variable) feature, I is a variable index, Dir is a variable dependency
%       direction annotation, and Role is a variable role annotation. The
%       special categories conj, lrb,rrb, ., ,, and ; are replaced by
%       variables.
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
cat_index(B:F, co(B:F, B, _, _, _)) :- % TODO do we need the second argument of co/4?
  !.
cat_index(B, co(B:_, B, _, _, _)). 

cat_number(CO) :-
  cat_number(CO, 1, _).

%%	cat_number(+CO, +M, -N)
%
%	Replaces variable indices in category objects with integers, starting
%	from M.
cat_number(co(_, _, I, _, _), M, N) :-
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
