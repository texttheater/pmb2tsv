:- module(anno, [
    cac_annotate/1]).

/** <module> Annotate categories with dependency directions and semantic roles
*/

:- use_module(cac, [
    cac_cat/2]).
:- use_module(slashes).
:- use_module(util, [
    must/1]).

cac_annotate(t(Cat, _, Atts)) :-
  !,
  (  var(Cat)
  -> Cat = a(_, _, _)
  ;  true
  ),
  member(sem:Sem, Atts),
  member(lemma:Lemma, Atts),
  (  member(verbnet:Roles, Atts)
  -> true
  ;  Roles = []
  ),
  must(cat_annotate(Cat, Sem, Lemma, Roles)).
cac_annotate(lx(_, _, D)) :-
  !,
  cac_annotate(D).
cac_annotate(Const) :-
  Const =.. [_, _, L, R],
  cac_annotate(L),
  cac_annotate(R).

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

inv(f(_, Res, Arg)) :-
  arg(1, Res, ID),
  arg(1, Arg, ID).
inv(b(_, Res, Arg)) :-
  arg(1, Res, ID),
  arg(1, Arg, ID).

noninv(f(FunID, Res, _)) :-
  arg(1, Res, FunID).
noninv(b(FunID, Res, _)) :-
  arg(1, Res, FunID).

% conjunctions and punctuation
cat_annotate(CO, Sem, Lemma, []) :-
  member(CO, [f(_, Res, Arg), b(_, Res, Arg)]),
  member(Sem, ['NIL', 'QUE', 'GRP', 'COO']),
  !,
  inv(CO),
  cat_annotate(Res, Sem, Lemma, []).
% adjective copulas
cat_annotate(CO, Sem, Lemma, []) :-
  CO = b(_, Res, _),
  co_match(CO, (s:_\np)\(s:adj\np)),
  member(Lemma, [be, ai]),
  !,
  inv(CO),
  cat_annotate(Res, Sem, Lemma, []).
cat_annotate(CO, Sem, Lemma, []) :-
  CO = f(_, Res, _),
  co_match(CO, (s:_\np)/(s:adj\np)),
  member(Lemma, [be, ai]),
  !,
  inv(CO),
  cat_annotate(Res, Sem, Lemma, []).
cat_annotate(f(I, f(I, a(J, s:q, s), b(K, a(L, s:adj, s), a(M, np, np))), a(M, np, np)), Sem, Lemma, []) :-
  member(Lemma, [be, ai]),
  !,
  cat_annotate(f(I, a(J, s:q, s), b(K, a(L, s:adj, s), a(M, np, np))), Sem, Lemma, []).
% noun copulas
cat_annotate(CO, Sem, Lemma, []) :-
  CO = b(_, Res, _),
  co_match(CO, (s:_\np:F)/np),
  F \== thr,
  member(Lemma, [be, ai]),
  !,
  inv(CO),
  cat_annotate(Res, Sem, Lemma, []).
cat_annotate(CO, Sem, Lemma, []) :-
  CO = f(_, Res, _),
  co_match(CO, (s:_\np:F)\np),
  F \== thr,
  member(Lemma, [be, ai]),
  !,
  inv(CO),
  cat_annotate(Res, Sem, Lemma, []).
% adposition copulas
cat_annotate(CO, Sem, Lemma, []) :-
  co = f(_, Res, _),
  co_match(CO, (s:_\np:F)/pp),
  F \== thr,
  member(Lemma, [be, ai]),
  !,
  inv(CO),
  cat_annotate(Res, Sem, Lemma, []).
cat_annotate(CO, Sem, Lemma, []) :-
  co = b(_, Res, _),
  co_match(CO, (s:_\np:F)\pp),
  F \== thr,
  member(Lemma, [be, ai]),
  !,
  inv(CO),
  cat_annotate(Res, Sem, Lemma, []).
% auxiliaries
cat_annotate(CO, Sem, Lemma, []) :-
  CO = f(_, Res, _),
  co_match(CO, (s:_\np)/(s:_\np)),
  member(Sem, ['NOW', 'PST', 'FUT', 'PRG', 'PFT']),
  !,
  inv(CO),
  cat_annotate(Res, Sem, Lemma, []).
cat_annotate(CO, Sem, Lemma, []) :-
  CO = b(_, Res, _),
  co_match(CO, (s:_\np)\(s:_\np)),
  member(Sem, ['NOW', 'PST', 'FUT', 'PRG', 'PFT']),
  !,
  inv(CO),
  cat_annotate(Res, Sem, Lemma, []).
cat_annotate(f(I, f(I, a(J, s:q, s), b(K, a(L, s:F, s), a(M, np, np))), a(M, np, np)), Sem, Lemma, []) :-
  member(Sem, ['NOW', 'PST', 'FUT', 'PRG', 'PFT']),
  !,
  cat_annotate(f(I, a(J, s:q, s), b(K, a(L, s:F, s), a(M, np, np))), Sem, Lemma, []).
% TODO "have" with s:pss\np argument? ("He had his tooth pulled")
% TODO modal verbs?
% infinitives with infinitive arguments (not modifiers)
cat_annotate(CO, Sem, Lemma, [Role|Roles]) :-
  CO = f(_, Res, _),
  co_match(CO, (s:b\np)/(s:b\np)),
  member(Sem, ['EXS', 'ENS', 'EPS', 'EXG', 'EXT']),
  !,
  noninv(CO),
  co_role(CO, Role),
  cat_annotate(Res, Sem, Lemma, Roles).
cat_annotate(CO, Sem, Lemma, [Role|Roles]) :-
  CO = b(_, Res, _),
  co_match(CO, (s:b\np)\(s:b\np)),
  member(Sem, ['EXS', 'ENS', 'EPS', 'EXG', 'EXT']),
  !,
  noninv(CO),
  co_role(CO, Role),
  cat_annotate(Res, Sem, Lemma, Roles).
% NPs with NP arguments (not modifiers)
cat_annotate(CO, Sem, Lemma, [Role|Roles]) :-
  CO = f(_, Res, _),
  co_match(CO, np/np),
  member(Sem, ['EXG']),
  !,
  noninv(CO),
  co_role(CO, Role),
  cat_annotate(Res, Sem, Lemma, Roles).
cat_annotate(CO, Sem, Lemma, [Role|Roles]) :-
  CO = b(_, Res, _),
  co_match(CO, np\np),
  member(Sem, ['EXG']),
  !,
  noninv(CO),
  co_role(CO, Role),
  cat_annotate(Res, Sem, Lemma, Roles).
% "modifiers" of relational nouns
cat_annotate(CO, _, _, [Role]) :-
  co_match(CO, n/(n/pp)),
  !,
  inv(CO),
  co_role(CO, Role).
% role-assigning modifiers
cat_annotate(CO, _, _, [Role]) :-
  CO = b(_, X, X),
  !,
  inv(CO),
  co_role(CO, Role).
cat_annotate(CO, _, _, [Role]) :-
  CO = f(_, X, X),
  !,
  inv(CO),
  co_role(CO, Role).
% non-role-assigning modifiers
cat_annotate(CO, _, _, []) :-
  CO = b(_, X, X),
  !,
  inv(CO).
cat_annotate(CO, _, _, []) :-
  CO = f(_, X, X),
  !,
  inv(CO).
% role-assigning adpositions
cat_annotate(CO, Sem, Lemma, [Role|Roles]) :-
  CO = f(_, Res, _),
  co_match(CO, pp/_),
  !,
  inv(CO),
  co_role(CO, Role),
  cat_annotate(Res, Sem, Lemma, Roles).
cat_annotate(CO, Sem, Lemma, [Role|Roles]) :-
  CO = b(_, Res, _),
  co_match(CO, pp\_),
  !,
  inv(CO),
  co_role(CO, Role),
  cat_annotate(Res, Sem, Lemma, Roles).
cat_annotate(CO, Sem, Lemma, [Role|Roles]) :-
  CO = f(_, Res, _),
  co_match(CO, (A\A)/_),
  !,
  inv(CO),
  co_role(CO, Role),
  cat_annotate(Res, Sem, Lemma, Roles).
cat_annotate(CO, Sem, Lemma, [Role|Roles]) :-
  CO = f(_, Res, _),
  co_match(CO, (A/A)/_),
  !,
  inv(CO),
  co_role(CO, Role),
  cat_annotate(Res, Sem, Lemma, Roles).
cat_annotate(CO, Sem, Lemma, [Role|Roles]) :-
  CO = b(_, Res, _),
  co_match(CO, (A\A)\_),
  !,
  inv(CO),
  co_role(CO, Role),
  cat_annotate(Res, Sem, Lemma, Roles).
cat_annotate(CO, Sem, Lemma, [Role|Roles]) :-
  CO = b(_, Res, _),
  co_match(CO, (A/A)\_),
  !,
  inv(CO),
  co_role(CO, Role),
  cat_annotate(Res, Sem, Lemma, Roles).
% categories with PP argument
cat_annotate(CO, Sem, Lemma, Roles) :-
  CO = f(_, Res, _),
  co_match(CO, _/pp),
  !,
  noninv(CO),
  cat_annotate(Res, Sem, Lemma, Roles).
cat_annotate(CO, Sem, Lemma, Roles) :-
  CO = b(_, Res, _),
  co_match(CO, _\pp),
  !,
  noninv(CO),
  cat_annotate(Res, Sem, Lemma, Roles).
% other role-assigning categories
cat_annotate(CO, Sem, Lemma, [Role|Roles]) :-
  CO = b(_, Res, _),
  !,
  noninv(CO),
  co_role(CO, Role),
  cat_annotate(Res, Sem, Lemma, Roles).
cat_annotate(CO, Sem, Lemma, [Role|Roles]) :-
  CO = f(_, Res, _),
  !,
  noninv(CO),
  co_role(CO, Role),
  cat_annotate(Res, Sem, Lemma, Roles).
% determiners
cat_annotate(CO, Sem, Lemma, []) :-
  CO = f(_, Res, _),
  co_match(CO, np/n),
  !,
  inv(CO),
  cat_annotate(Res, Sem, Lemma, []).
cat_annotate(CO, Sem, Lemma, []) :-
  CO = f(_, Res, _),
  co_match(CO, np/(n/pp)),
  !,
  inv(CO),
  cat_annotate(Res, Sem, Lemma, []).
% subordinating conjunctions
cat_annotate(CO, Sem, Lemma, []) :-
  CO = f(_, Res, Arg),
  % left/right sentence/VP/question VP modification
  ( co_match(Res, s\s)
  ; co_match(Res, s/s)
  ; co_match(Res, (s\np)\(s\np))
  ; co_match(Res, (s\np)/(s\np))
  ; co_match(Res, (s/np)\(s/np))
  ; co_match(Res, (s/np)/(s/np))
  ),
  % type of embedded clause
  ( co_match(Arg, s:dcl)
  ; co_match(Arg, s:to)
  ; co_match(Arg, s:ng\np)
  ; co_match(Arg, s:ng/np)
  ),
  !,
  inv(CO),
  cat_annotate(Res, Sem, Lemma, []).
% complementizers
cat_annotate(CO, Sem, Lemma, []) :-
  CO = f(_, Res, _),
  co_match(CO, s:em/s:dcl),
  !,
  inv(CO),
  cat_annotate(Res, Sem, Lemma, []).
cat_annotate(CO, Sem, Lemma, []) :-
  CO = f(_, Res, _),
  co_match(CO, (s:to\np)/(s:b\np)),
  !,
  inv(CO),
  cat_annotate(Res, Sem, Lemma, []).
% relative pronouns
cat_annotate(CO, Sem, Lemma, []) :-
  CO = f(_, Res, Arg),
  ( co_match(Res, n\n)
  ; co_match(Res, np\np)
  ),
  ( co_match(Arg, s:dcl\np)
  ; co_match(Arg, s:dcl/np)
  ),
  !,
  inv(CO),
  cat_annotate(Res, Sem, Lemma, []).
% wh-words
cat_annotate(CO, Sem, Lemma, []) :-
  CO = f(_, Res, _),
  co_match(CO, s:wq/_),
  !,
  inv(CO),
  cat_annotate(Res, Sem, Lemma, []).
% other function categories
cat_annotate(CO, Sem, Lemma, []) :-
  CO = b(_, Res, _),
  !,
  noninv(CO),
  cat_annotate(Res, Sem, Lemma, []).
cat_annotate(CO, Sem, Lemma, []) :-
  CO = f(_, Res, _),
  !,
  noninv(CO),
  cat_annotate(Res, Sem, Lemma, []).
% basic categories
cat_annotate(_, _, _, _). % TODO Rolelists should be empty here, but sometimes aren't - what's up with that?
