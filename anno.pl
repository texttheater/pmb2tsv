:- module(anno, [
    cac_annotate/2]).

/** <module> Annotate categories with dependency directions and semantic roles
*/

:- use_module(cac, [
    cac_cat/2]).
:- use_module(cat, [
    cat_dir/2,
    cat_id/2,
    cat_match/2,
    cat_role/2]).
:- use_module(slashes).
:- use_module(util, [
    must/1]).

cac_annotate(t(Cat, _, Atts), Opts) :-
  !,
  member(sem:Sem, Atts),
  member(lemma:Lemma, Atts),
  (  member(verbnet:Roles, Atts)
  -> true
  ;  Roles = []
  ),
  must(cat_annotate(Cat, Sem, Lemma, Roles, Opts)).
cac_annotate(Const, Opts) :-
  Const =.. [_, _, L, R],
  cac_annotate(L, Opts),
  cac_annotate(R, Opts).

% type-raising pseudo tokens
cat_annotate((X/(X\Y))/Y, _, _, _, _) :-
  !.
cat_annotate((X\(X/Y))/Y, _, _, _, _) :-
  !.
% conjunctions and punctuation
cat_annotate(((A\B)/C)\D, Sem, _, _, _) :-
  member(Sem, ['NIL', 'QUE', 'GRP', 'COO']),
  cat_match(A, Cat),
  cat_match(B, Cat),
  !,
  cat_dir(D, inv),
  cat_dir(C, inv),
  cat_dir(B, inv),
  cat_annotate_mod(A, B).
cat_annotate((A\B)\C, Sem, _, _, _) :-
  member(Sem, ['NIL', 'QUE', 'GRP', 'COO']),
  cat_match(A, Cat),
  cat_match(B, Cat),
  !,
  cat_dir(C, inv),
  cat_dir(B, inv),
  cat_annotate_mod(A, B).
cat_annotate((A\B)/C, Sem, _, _, _) :-
  member(Sem, ['NIL', 'QUE', 'GRP', 'COO']),
  cat_match(A, Cat),
  cat_match(B, Cat),
  !,
  cat_dir(C, inv),
  cat_dir(B, inv),
  cat_annotate_mod(A, B).
% adjective copulas
cat_annotate((A\B)/(C\D), Sem, be, [], Opts) :-
  cat_match(A\B, s:_\np),
  cat_match(C\D, s:adj\np),
  !,
  cat_dir(C\D, inv),
  cat_role(D, Role),
  cat_role(B, Role),
  cat_annotate(A\B, Sem, be, [], Opts).
cat_annotate((A\B)\(C\D), Sem, be, [], Opts) :-
  cat_match(A\B, s:_\np),
  cat_match(C\D, s:adj\np),
  !,
  cat_dir(C\D, inv),
  cat_role(D, Role),
  cat_role(B, Role),
  cat_annotate(A\B, Sem, be, [], Opts).
cat_annotate((A/B)\(C\D), Sem, be, [], Opts) :-
  cat_match(A/B, s:_/np),
  cat_match(C\D, s:adj\np),
  !,
  cat_dir(C\D, inv),
  cat_role(D, Role),
  cat_role(B, Role),
  cat_annotate(A\B, Sem, be, [], Opts).
cat_annotate((A/(B\C))/D, Sem, be, [], Opts) :-
  cat_match(A, s:_),
  cat_match(B\C, s:adj\np),
  cat_match(D, np),
  !,
  cat_dir(D, noninv),
  cat_dir(B\C, flip),
  cat_role(C, Role),
  cat_role(D, Role),
  cat_annotate(A, Sem, be, [], Opts).
cat_annotate((A/(B\C))/D, Sem, be, [], Opts) :-
  cat_match(A, s:q),
  cat_match(B\C, s:adj\np),
  cat_match(D, np),
  !,
  cat_dir(D, noninv),
  cat_dir(B\C, flip),
  cat_role(C, Role),
  cat_role(D, Role),
  cat_annotate(A, Sem, be, [], Opts).
% noun copulas
cat_annotate(X/Y, Sem, be, Roles0, Opts) :-
  member(copula(true), Opts),
  cat_match(X, s:_\np:F),
  F \== thr,
  ( cat_match(Y, np)
  ; cat_match(Y, np/pp)
  ),
  !,
  cat_dir(Y, inv),
  handle_roles(Y, Roles0, Roles),
  cat_annotate(X, Sem, be, Roles, Opts).
cat_annotate(X\Y, Sem, be, Roles0, Opts) :-
  member(copula(true), Opts),
  cat_match(X, s:_\np:F),
  F \== thr,
  ( cat_match(Y, np)
  ; cat_match(Y, np/pp)
  ),
  !,
  cat_dir(Y, inv),
  handle_roles(Y, Roles0, Roles),
  cat_annotate(X, Sem, be, Roles, Opts).
cat_annotate((X/Y)/Z, Sem, be, Roles0, Opts) :-
  member(copula(true), Opts),
  cat_match(X, s:q),
  cat_match(Y, np),
  ( cat_match(Z, np)
  ; cat_match(Z, np/pp)
  ),
  !,
  cat_dir(Z, noninv),
  cat_dir(Y, flip),
  handle_roles(Y, Roles0, Roles),
  cat_annotate(X, Sem, be, Roles, Opts).
% adposition copulas
cat_annotate(X/Y, Sem, be, Roles, Opts) :-
  cat_match(X, s:_\np:F),
  F \== thr,
  cat_match(Y, pp),
  !,
  cat_dir(Y, inv),
  cat_annotate(X, Sem, be, Roles, Opts).
cat_annotate(X\Y, Sem, be, Roles, Opts) :-
  cat_match(X, s:_\np:F),
  F \== thr,
  cat_match(Y, pp),
  !,
  cat_dir(Y, inv),
  cat_annotate(X, Sem, be, Roles, Opts).
cat_annotate((X/Y)/Z, Sem, be, [Role], Opts) :-
  cat_match(X, s:q),
  cat_match(Y, pp),
  cat_match(Z, np),
  !,
  cat_dir(Z, noninv),
  cat_role(Z, Role),
  cat_dir(Y, flip),
  cat_annotate(X, Sem, be, [], Opts).
% auxiliaries and modals
cat_annotate((A\B)/(C\D), Sem, Lemma, [], Opts) :-
  member(Sem, ['NOW', 'PST', 'FUT', 'PRG', 'PFT', 'NEC', 'POS', 'NIL']),
  cat_match(A\B, s:_\np),
  cat_match(C\D, s:_\np),
  \+ member(Lemma, [use, get, go]),
  !,
  cat_dir(C\D, inv),
  cat_role(D, Role),
  cat_role(B, Role),
  cat_annotate(A\B, Sem, Lemma, [], Opts).
cat_annotate((A\B)\(C\D), Sem, Lemma, [], Opts) :-
  member(Sem, ['NOW', 'PST', 'FUT', 'PRG', 'PFT', 'NEC', 'POS', 'NIL']),
  cat_match(A\B, s:_\np),
  cat_match(C\D, s:_\np),
  \+ member(Lemma, [use, get, go]),
  !,
  cat_dir(C\D, inv),
  cat_role(D, Role),
  cat_role(B, Role),
  cat_annotate(A\B, Sem, Lemma, [], Opts).
cat_annotate((A/(B\C))/D, Sem, Lemma, [], Opts) :-
  member(Sem, ['NOW', 'PST', 'FUT', 'PRG', 'PFT', 'NEC', 'POS']),
  cat_match(A, s:q),
  cat_match(B\C, s:_\np),
  \+ member(Lemma, [use, get, go]),
  !,
  cat_dir(D, noninv),
  cat_dir(B\C, flip),
  cat_role(C, Role),
  cat_role(D, Role),
  cat_annotate(A, Sem, Lemma, [], Opts).
% Italian middle voice reflexive pronoun, analyzed like an auxiliary
cat_annotate(X/Y, 'REF', Lemma, [], Opts) :-
  cat_match(X, s:dcl\np),
  cat_match(Y, s:pss\np),
  !,
  cat_dir(Y, inv),
  cat_annotate(X, 'REF', Lemma, [], Opts).
% reflexive pronouns
cat_annotate(X\Y, 'REF', Lemma, [], Opts) :-
  cat_match(X, s:_\np),
  cat_match(Y, (s:_\np)/np),
  !,
  cat_dir(Y, inv),
  cat_annotate(X, 'REF', Lemma, [], Opts).
% "have" with s:pss\np argument ("He had his tooth pulled")
cat_annotate(((A\B)/(C\D))/E, Sem, Lemma, [SubjRole], Opts) :-
  member(Sem, ['NOW', 'PST', 'FUT', 'PRG', 'PFT']),
  cat_match(A\B, s:_\np),
  cat_match(C\D, s:pss\np),
  cat_match(E, np),
  !,
  cat_dir(E, noninv),
  cat_dir(C\D, flip),
  cat_dir(B, noninv),
  cat_role(B, SubjRole),
  cat_role(D, ObjRole),
  cat_role(E, ObjRole),
  cat_annotate(A, Sem, Lemma, [], Opts).
% "need" with s:ng\np argument ("The shirt needs ironing")
cat_annotate((A\B)/(C\D), 'NEC', Lemma, [], Opts) :-
  cat_match(A\B, s:_\np),
  cat_match(C\D, s:ng\np),
  !,
  cat_dir(C\D, flip),
  cat_dir(B, noninv),
  cat_role(D, Role),
  cat_role(B, Role),
  cat_annotate(A, 'NEC', Lemma, [], Opts).
cat_annotate((A\B)\(C\D), 'NEC', Lemma, [], Opts) :-
  cat_match(A\B, s:_\np),
  cat_match(C\D, s:ng\np),
  !,
  cat_dir(C\D, flip),
  cat_dir(B, noninv),
  cat_role(D, Role),
  cat_role(B, Role),
  cat_annotate(A, 'NEC', Lemma, [], Opts).
% TODO what about "need to"?
% TODO modal verbs?
% modal verbs
%cat_annotate((A\B)/(C\D), Sem, Lemma, []) :-
%  member(Sem, ['NEC', 'POS']),
%  cat_match(A\B, s:_\np),
%  cat_match(C\D, s:b\np),
%  !,
%  cat_dir(C\D, noninv),
%  cat_role(D, Role),
%  cat_role(B, Role),
%  cat_annotate(A\B, Sem, Lemma, []).
%cat_annotate((A\B)\(C\D), Sem, Lemma, []) :-
%  member(Sem, ['NEC', 'POS']),
%  cat_match(A\B, s:_\np),
%  cat_match(C\D, s:b\np),
%  !,
%  cat_dir(C\D, noninv),
%  cat_role(D, Role),
%  cat_role(B, Role),
%  cat_annotate(A\B, Sem, Lemma, []).
%cat_annotate((A/(B\C))/D, Sem, Lemma, []) :-
%  member(Sem, ['NEC', 'POS']),
%  cat_match(A, s:q),
%  cat_match(B\C, s:b\np),
%  !,
%  cat_dir(D, noninv),
%  cat_dir(B\C, flip),
%  cat_role(C, Role),
%  cat_role(D, Role),
%  cat_annotate(A, Sem, Lemma, []).
% verbs with VP arguments (special case so they are not mistaken for
% modifiers)
cat_annotate(X/Y, Sem, Lemma, Roles0, Opts) :-
  cat_match(X/Y, (s:b\np)/(s:b\np)),
  member(Sem, ['EXS', 'ENS', 'EPS', 'EXG', 'EXT']),
  !,
  cat_dir(Y, noninv),
  handle_roles(Y, Roles0, Roles),
  cat_annotate(X, Sem, Lemma, Roles, Opts).
cat_annotate(X\Y, Sem, Lemma, Roles0, Opts) :-
  cat_match(X\Y, (s:b\np)\(s:b\np)),
  member(Sem, ['EXS', 'ENS', 'EPS', 'EXG', 'EXT']),
  !,
  cat_dir(Y, noninv),
  handle_roles(Y, Roles0, Roles),
  cat_annotate(X, Sem, Lemma, Roles, Opts).
% NPs with NP arguments
cat_annotate(X/Y, Sem, Lemma, Roles0, Opts) :-
  member(Sem, ['EXG']),
  cat_match(X/Y, np/np),
  !,
  cat_dir(Y, noninv),
  handle_roles(Y, Roles0, Roles),
  cat_annotate(X, Sem, Lemma, Roles, Opts).
cat_annotate(X\Y, Sem, Lemma, Roles0, Opts) :-
  member(Sem, ['EXG']),
  cat_match(X\Y, np\np),
  !,
  cat_dir(Y, noninv),
  handle_roles(Y, Roles0, Roles),
  cat_annotate(X, Sem, Lemma, Roles, Opts).
% adpositions
cat_annotate(X/Y, Sem, Lemma, Roles0, Opts) :-
  cat_match(X, pp),
  !,
  cat_dir(Y, inv),
  handle_roles(Y, Roles0, Roles),
  cat_annotate(X, Sem, Lemma, Roles, Opts).
cat_annotate(X\Y, Sem, Lemma, Roles0, Opts) :-
  cat_match(X, pp),
  !,
  cat_dir(Y, inv),
  handle_roles(Y, Roles0, Roles),
  cat_annotate(X, Sem, Lemma, Roles, Opts).
cat_annotate((X\Y)/Z, Sem, Lemma, Roles0, Opts) :-
  cat_match(X\Y, A\A),
  !,
  cat_dir(Z, inv),
  handle_roles(Y, Roles0, Roles),
  cat_annotate(X\Y, Sem, Lemma, Roles, Opts).
cat_annotate((X/Y)/Z, Sem, Lemma, Roles0, Opts) :-
  cat_match(X/Y, A/A),
  !,
  cat_dir(Z, inv),
  handle_roles(Y, Roles0, Roles),
  cat_annotate(X/Y, Sem, Lemma, Roles, Opts).
cat_annotate((X\Y)\Z, Sem, Lemma, Roles0, Opts) :-
  cat_match(X\Y, A\A),
  !,
  cat_dir(Z, inv),
  handle_roles(Y, Roles0, Roles),
  cat_annotate(X\Y, Sem, Lemma, Roles, Opts).
cat_annotate((X/Y)\Z, Sem, Lemma, Roles0, Opts) :-
  cat_match(X/Y, A/A),
  !,
  cat_dir(Z, inv),
  handle_roles(Y, Roles0, Roles),
  cat_annotate(X/Y, Sem, Lemma, Roles, Opts).
% determiners
cat_annotate(X/Y, Sem, Lemma, Roles0, Opts) :-
  cat_match(X/Y, np/n),
  !,
  cat_dir(Y, inv),
  handle_roles(Y, Roles0, Roles),
  cat_annotate(X, Sem, Lemma, Roles, Opts).
cat_annotate(X\Y, Sem, Lemma, Roles0, Opts) :-
  cat_match(X\Y, np\n), % "Many a man"
  !,
  cat_dir(Y, inv),
  handle_roles(Y, Roles0, Roles),
  cat_annotate(X, Sem, Lemma, Roles, Opts).
cat_annotate(X/Y, Sem, Lemma, Roles0, Opts) :-
  cat_match(X/Y, np/(n/pp)),
  !,
  cat_dir(Y, inv),
  handle_roles(Y, Roles0, Roles),
  cat_annotate(X, Sem, Lemma, Roles, Opts).
% possessive suffix
cat_annotate(X\Y, Sem, Lemma, [Role|Roles], Opts) :-
  cat_match(X\Y, (np/(n/pp))\np),
  !,
  cat_dir(Y, inv),
  cat_role(Y, Role),
  cat_annotate(X, Sem, Lemma, Roles, Opts).
% subordinating conjunctions
cat_annotate(X/Y, Sem, Lemma, Roles0, Opts) :-
  % left/right sentence/VP/question VP modification
  ( cat_match(X, s\s)
  ; cat_match(X, s/s)
  ; cat_match(X, (s\np)\(s\np))
  ; cat_match(X, (s\np)/(s\np))
  ; cat_match(X, (s/np)\(s/np))
  ; cat_match(X, (s/np)/(s/np))
  ),
  % type of embedded clause
  ( cat_match(Y, s:dcl)
  ; cat_match(Y, s:to)
  ; cat_match(Y, s:ng\np)
  ; cat_match(Y, s:ng/np)
  ),
  !,
  cat_dir(Y, inv),
  handle_roles(Y, Roles0, Roles),
  cat_annotate(X, Sem, Lemma, Roles, Opts).
% complementizers
cat_annotate(X/Y, Sem, Lemma, Roles0, Opts) :-
  cat_match(X/Y, s:em/s:dcl),
  !,
  cat_dir(Y, inv),
  handle_roles(Y, Roles0, Roles),
  cat_annotate(X, Sem, Lemma, Roles, Opts).
cat_annotate(X\Y, Sem, Lemma, Roles0, Opts) :-
  cat_match(X\Y, s:em\s:dcl),
  !,
  cat_dir(Y, inv),
  handle_roles(Y, Roles0, Roles),
  cat_annotate(X, Sem, Lemma, Roles, Opts).
cat_annotate(X/Y, Sem, Lemma, Roles0, Opts) :-
  cat_match(X/Y, (s:to\np)/(s:b\np)),
  !,
  cat_dir(Y, inv),
  handle_roles(Y, Roles0, Roles),
  cat_annotate(X, Sem, Lemma, Roles, Opts).
cat_annotate(X\Y, Sem, Lemma, Roles0, Opts) :-
  cat_match(X\Y, (s:to\np)\(s:b\np)),
  !,
  cat_dir(Y, inv),
  handle_roles(Y, Roles0, Roles),
  cat_annotate(X, Sem, Lemma, Roles, Opts).
% relativizers
cat_annotate(X/Y, Sem, Lemma, Roles0, Opts) :-
  ( cat_match(X, n\n)
  ; cat_match(X, np\np)
  ),
  ( cat_match(Y, s:dcl\np)
  ; cat_match(Y, s:dcl/np)
  ),
  !,
  cat_dir(Y, inv),
  handle_roles(Y, Roles0, Roles),
  cat_annotate(X, Sem, Lemma, Roles, Opts).
cat_annotate((X/Y)/Z, Sem, Lemma, Roles0, Opts) :-
  ( cat_match(X, n\n)
  ; cat_match(X, np\np)
  ),
  ( cat_match(Y, s:dcl\np)
  ; cat_match(Y, s:dcl/np)
  ),
  cat_match(Z, n),
  !,
  cat_dir(Z, inv),
  handle_roles(Y, Roles0, Roles),
  cat_annotate(X/Y, Sem, Lemma, Roles, Opts).
% pseudo tokens starting reduced relative clauses
cat_annotate(X/Y, Sem, Lemma, Roles0, Opts) :-
  cat_match(X, n\n),
  ( cat_match(Y, s:ng\np)
  ; cat_match(Y, s:pss\np)
  ; cat_match(Y, s:adj\np)
  ; cat_match(Y, s:dcl\np)
  ),
  !,
  cat_dir(Y, inv),
  handle_roles(Roles0, Roles),
  cat_annotate(X, Sem, Lemma, Roles, Opts).
% other pseudo tokens
cat_annotate(X/Y, Sem, Lemma, Roles0, Opts) :-
  ( cat_match(X/Y, (n/n)/(s:adj\np))
  ; cat_match(X/Y, (s/s)/(s:to\np))
  ; cat_match(X/Y, (s/s)/(s:pss\np))
  ),
  !,
  cat_dir(Y, inv),
  handle_roles(Roles0, Roles),
  cat_annotate(X, Sem, Lemma, Roles, Opts).
% question words
cat_annotate(X/Y, 'EMP', Lemma, [], Opts) :-
  cat_match(X/Y, s:dcl/np),
  !,
  cat_dir(Y, inv),
  cat_annotate(X, 'EMP', Lemma, [], Opts).
cat_annotate(X/(Y/Z), Sem, Lemma, Roles0, Opts) :-
  cat_match(X, s:wq),
  cat_match(Y/Z, s:q/(s:adj\np)),
  !,
  cat_dir(Z, Dir),
  when(nonvar(Dir), % HACK
      (  Dir = flip
      -> cat_dir(Y, noninv)
      ;  cat_dir(Y, inv)
      ) ),
  handle_roles(Y, Roles0, Roles),
  cat_annotate(X, Sem, Lemma, Roles, Opts).
cat_annotate(X/(Y/Z), Sem, Lemma, Roles0, Opts) :-
  cat_match(X, s:wq),
  cat_match(Y/Z, s:q/np),
  !,
  cat_dir(Z, Dir),
  when(nonvar(Dir), % HACK
      (  Dir = flip
      -> cat_dir(Y, noninv)
      ;  cat_dir(Y, inv)
      ) ),
  handle_roles(Y, Roles0, Roles),
  cat_annotate(X, Sem, Lemma, Roles, Opts).
cat_annotate(X/(Y/Z), Sem, Lemma, Roles0, Opts) :-
  cat_match(X, s:wq),
  cat_match(Y/Z, s:q/pp),
  !,
  cat_dir(Z, Dir),
  when(nonvar(Dir), % HACK
      (  Dir = flip
      -> cat_dir(Y, noninv)
      ;  cat_dir(Y, inv)
      ) ),
  handle_roles(Y, Roles0, Roles),
  cat_annotate(X, Sem, Lemma, Roles, Opts).
cat_annotate(X/Y, Sem, Lemma, Roles0, Opts) :-
  ( cat_match(X, s:wq)
  ; cat_match(X, s:wq/_)
  ),
  !,
  cat_dir(Y, inv),
  handle_roles(Y, Roles0, Roles),
  cat_annotate(X, Sem, Lemma, Roles, Opts).
% modifiers
cat_annotate(X\Y, _, _, Roles0, _Opts) :-
  cat_match(X, Cat),
  cat_match(Y, Cat),
  !,
  cat_dir(Y, inv),
  handle_roles(Y, Roles0, _),
  cat_annotate_mod(X, Y).
cat_annotate(X/Y, _, _, Roles0, _Opts) :-
  cat_match(X, Cat),
  cat_match(Y, Cat),
  !,
  cat_dir(Y, inv),
  handle_roles(Y, Roles0, _),
  cat_annotate_mod(X, Y).
% "modifiers" of relational nouns
cat_annotate(X/Y, Sem, Lemma, Roles0, Opts) :-
  cat_match(X, n),
  cat_match(Y, n/pp),
  !,
  cat_dir(Y, inv),
  handle_roles(Y, Roles0, Roles),
  cat_annotate(X, Sem, Lemma, Roles, Opts).
cat_annotate(X\Y, Sem, Lemma, Roles0, Opts) :-
  cat_match(X, n),
  cat_match(Y, n/pp),
  !,
  cat_dir(Y, inv),
  handle_roles(Y, Roles0, Roles),
  cat_annotate(X, Sem, Lemma, Roles, Opts).
% role-assigning verbs with PP argument
cat_annotate(X/Y, Sem, Lemma, [Role|Roles], Opts) :-
  cat_match(Y, pp), % PP roles are assigned by adpositions
  !,
  cat_dir(Y, noninv),
  cat_annotate(X, Sem, Lemma, [Role|Roles], Opts).
cat_annotate(X\Y, Sem, Lemma, [Role|Roles], Opts) :-
  cat_match(Y, pp), % PP roles are assigned by adpositions
  !,
  cat_dir(Y, noninv),
  cat_role(Y, Role),
  cat_annotate(X, Sem, Lemma, [Role|Roles], Opts).
% role-assigning verbs with other argument
cat_annotate(X/Y, Sem, Lemma, [Role|Roles], Opts) :-
  !,
  cat_dir(Y, noninv),
  cat_role(Y, Role),
  cat_annotate(X, Sem, Lemma, Roles, Opts).
cat_annotate(X\Y, Sem, Lemma, [Role|Roles], Opts) :-
  !,
  cat_dir(Y, noninv),
  cat_role(Y, Role),
  cat_annotate(X, Sem, Lemma, Roles, Opts).
% other function categories
cat_annotate(X\Y, Sem, Lemma, Roles0, Opts) :-
  !,
  cat_dir(Y, noninv),
  handle_roles(Y, Roles0, Roles),
  cat_annotate(X, Sem, Lemma, Roles, Opts).
cat_annotate(X/Y, Sem, Lemma, Roles0, Opts) :-
  !,
  cat_dir(Y, noninv),
  handle_roles(Y, Roles0, Roles),
  cat_annotate(X, Sem, Lemma, Roles, Opts).
% basic categories
cat_annotate(_, _, _, _, _).

%%	cat_annotate_mod(?X, ?Y)
%
%	Copies directionality annotations from Y to X.
cat_annotate_mod(A\B, C\D) :-
  cat_dir(D, Dir),
  cat_dir(B, Dir),
  cat_role(D, Role),
  cat_role(B, Role),
  cat_annotate_mod(A, C).
cat_annotate_mod(A/B, C/D) :-
  cat_dir(D, Dir),
  cat_dir(B, Dir),
  cat_role(D, Role),
  cat_role(B, Role),
  cat_annotate_mod(A, C).
cat_annotate_mod(co(_, _, _, _, _), co(_, _, _, _, _)).

handle_roles(Y, [Role|Roles], Roles) :-
  cat_role(Y, Role).
handle_roles(_, Roles, Roles).

:- begin_tests(dir).

:- use_module(cat, [
    cat_index/2,
    cat_number/1]).

test(match1) :-
  cat_index(s\s, Cat),
  cat_number(Cat),
  cat_match(Cat, s\s).

test(dir1) :-
  cat_index(np/n, Cat),
  cat_number(Cat),
  cat_annotate(Cat, 'DEF', the),
  Cat = co(np:_, np, 1, _, _)/co(n:_, n, 2, inv, _).

test(dir2) :-
  cat_index((s:dcl\np)/np, Cat),
  cat_number(Cat),
  cat_annotate(Cat, 'ENS', see),
  Cat = (co(s:dcl, _, _, _, _)\co(np:_, _, _, noninv, _))/co(np:_, _, _, noninv, _).

test(dir2a) :-
  cat_index(s\s, Cat),
  cat_number(Cat),
  cat_annotate(Cat, 'REL', '.'),
  Cat = co(s:_, _, _, _, _)\co(s:_, _, _, inv, _).

test(dir3) :-
  cat_index((s\s)/np, Cat),
  cat_number(Cat),
  cat_annotate(Cat, 'REL', during),
  Cat = (co(s:_, _, _, _, _)\co(s:_, _, _, inv, _))/co(np:_, _, _, inv, _).

:- end_tests(dir).
