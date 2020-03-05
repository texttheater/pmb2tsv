:- module(co, [
    co_match/2,
    co_role/2,
    co_topid/2]).

/** <module> Category objects (categories annotated with IDs)
*/

% match category objects against plain categories
co_match(f(_, X0, Y0), X/Y) :-
  co_match(X0, X),
  co_match(Y0, Y).
co_match(b(_, X0, Y0), X\Y) :-
  co_match(X0, X),
  co_match(Y0, Y).
co_match(a(_, F0:A0, _), F:A) :-
  nonvar(F0),
  !,
  F0:A0 = F:A.
co_match(a(_, F0:_, _), F) :-
  nonvar(F0),
  F0 = F.

co_role(_, _). % TODO

co_topid(a(ID, _, _), ID).
co_topid(f(_, Res, _), ID) :-
  co_topid(Res, ID).
co_topid(b(_, Res, _), ID) :-
  co_topid(Res, ID).
