:- module(dep, [
    cat_depdirs/2,
    t_depdirs/2]).

:- use_module(slashes).

%%  t_depdirs(+Token, -Dirs)
%
%   Given a C&C-style t/3 term (representing a token) with an indexed category,
%   returns a list indicating for the first N arguments whether they have
%   dependency inversion. N can be smaller than the number of arguments, in
%   which case the remaining directions are those of the unfilled arguments of
%   the argument.
% pseudo categories
t_depdirs(t(((_\_)/_)\_, _, Atts), [inv, inv, inv]) :-
  member(super:Super, Atts),
  member(Super, [conj, lrb, rrb, ., ,, ;]),
  !.
t_depdirs(t((_\_)/_, _, Atts), [inv, inv]) :-
  member(super:Super, Atts),
  member(Super, [conj, lrb, rrb, ., ,, ;]),
  !.
t_depdirs(t(_, _, Atts), [inv]) :-
  member(super:Super, Atts),
  member(Super, [lrb, rrb, ., ,, ;]),
  !.
% noun copulas
%t_depdirs(t(_, _, Atts), [inv, noninv]) :-
%  member(super:Super, Atts),
%  member(Super, [(s:_\np)/np, (s:_\np)\np, (s:q/np)/np]),
%  member(lemma:be, Atts),
%  !.
%% auxiliaries
%t_depdirs(t(_, _, Atts), [inv]) :-
%  member(super:Super, Atts),
%  member(Super, [(s:_\np)/(s:_\np), (s:_\np)\(s:_\np)]),
%  member(sem:Sem, Atts),
%  member(Sem, ['NOW', 'PST', 'FUT', 'PRG', 'PFT']),
%  !.
%t_depdirs(t(_, _, Atts), [noninv, inv]) :-
%  member(super:(s:q/(s:_\np))/np, Atts), % HACK noun should really depend on main verb
%  member(sem:Sem, Atts),
%  member(Sem, ['NOW', 'PST', 'FUT', 'PRG', 'PFT']),
%  !.
% other
t_depdirs(t(_, _, Atts), Dirs) :-
  member(super:Super, Atts),
  cat_depdirs(Super, Dirs).

% type-raising pseudo-tokens
cat_depdirs((X\(X/Y))/Y, [inv|Rest]) :-
  !,
  cat_depdirs(X\(X/Y), Rest).
cat_depdirs((X/(X\Y))/Y, [inv|Rest]) :-
  !,
  cat_depdirs(X/(X\Y), Rest).
% type-raised categories
cat_depdirs(X\(X/_), [inv]) :-
  !.
cat_depdirs(X/(X\_), [inv]) :-
  !.
% modifiers
cat_depdirs(X\X, [inv]) :-
  inv(X\X),
  !.
cat_depdirs(X/X, [inv]) :-
  inv(X/X),
  !.
% function categories
cat_depdirs(X/Y, [inv|Rest]) :-
  inv(X/Y),
  !,
  cat_depdirs(X, Rest).
cat_depdirs(X/_, [noninv|Rest]) :-
  !,
  cat_depdirs(X, Rest).
cat_depdirs(X\Y, [inv|Rest]) :-
  inv(X\Y),
  !,
  cat_depdirs(X, Rest).
cat_depdirs(X\_, [noninv|Rest]) :-
  !,
  cat_depdirs(X, Rest).
% basic categories
cat_depdirs(_, []).

%%  inv(+Cat)
%
%   True if Cat is a "dependency-inverting functor", i.e., its lexical head
%   should become a dependent of that of its outermost argument, rather than
%   the other way around.
%
%   The current implementation tries to generate "Universal Dependencies"-style
%   dependencies for the English, German, Italian, and Dutch derivations in the
%   Parallel Meaning Bank 2.2.0. Adapt as needed for newer versions.
% thing that kind of look like modifiers but aren't
% help John := (s:b\np)/(s:b\np)
inv((s:b\np)/(s:b\np)) :-
  !,
  fail.
inv(np/np) :-
  !,
  fail.
% modifiers
inv(X\X).
inv(X/X).
% adpositions
inv(X/np) :-
  inv(X).
inv(X\np) :-
  inv(X).
inv(pp/np).
inv(pp\np).
% determiners
inv(np/n).
inv(np/(n/pp)).
% subordinating conjunctions
inv(X/Y) :-
  member(X, [s\s, s/s, (s\np)\(s\np), (s\np)/(s\np), (s/np)\(s/np), (s/np)/(s/np)]), % left/right sentence/VP/question VP modification
  member(Y, [s:dcl, s:to, s:ng\np, s:ng/np]). % type of embedded clause
% complementizers
inv(s:em/s:dcl).
inv(s:em\s:dcl).
inv((s:to\np)/(s:b\np)).
inv((s:to\np)\(s:b\np)).
% relative pronouns
inv(X/Y) :-
  member(X, [n\n, np\np]), % restrictive or nonrestrictive
  member(Y, [s:dcl\np, s:dcl/np]). % subject or object relative clause
% pseudo tokens starting reduced relative clauses
inv((n\n)/(s:ng\np)) :-
  !.
inv((n\n)/(s:pss\np)) :-
  !.
inv((n\n)/(s:adj\np)) :-
  !.
inv((n\n)/(s:dcl/np)) :-
  !.
% other pseudo tokens
inv((n/n)/(s:adj\np)) :-
  !.
inv((s/s)/(s:to\np)) :-
  !.
inv((s/s)/(s:pss\np)) :-
  !.
% question words
inv(s:wq/_).
inv((s:wq/_)/_).
% adjective copulas
%inv((s:_\np)/(s:adj\np)).
%inv((s:_\np)\(s:adj\np)).
%inv((s:q/(s:adj\np))/np).
