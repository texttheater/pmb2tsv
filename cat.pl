:- module(cat, [
    arg_in/2,
    cat_dir/2,
    cat_id/2,
    cat_index/2,
    cat_number/1,
    cat_number/3,
    res_in/2]).

:- use_module(slashes).

%%	cat_index(+Cat, -CO)
%
%	Replaces categories with category objects, where basic categories are
%	of the form co(Cat:F, Cat, I, Dir), where F is the (possibly variable)
%	feature, I is a variable index, and Dir is a variable dependency direction
%	annotation. The special categories conj, lrb,
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
cat_index(B:F, co(B:F, B, _, _)) :- % TODO do we need the second argument of co/4?
  !.
cat_index(B, co(B:_, B, _, _)). 

cat_number(CO) :-
  cat_number(CO, 1, _).

%%	cat_number(+CO, +M, -N)
%
%	Replaces variable indices in category objects with integers, starting
%	from M.
cat_number(co(_, _, I, _), M, N) :-
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
cat_dir(co(_, _, _, Dir), Dir).

cat_id(X/_, ID) :-
  cat_id(X, ID).
cat_id(X\_, ID) :-
  cat_id(X, ID).
cat_id(co(_, _, ID, _), ID).
