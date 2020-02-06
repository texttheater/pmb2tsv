:- module(dir, [
    cac_annotate/1]).

:- use_module(cac, [
    cac_cat/2]).
:- use_module(cat, [
    cat_dir/2,
    cat_id/2]).
:- use_module(slashes).
:- use_module(util, [
    must/1]).

cac_annotate(t(Cat, _, Atts)) :-
  !,
  member(sem:Sem, Atts),
  member(lemma:Lemma, Atts),
  must(cat_annotate(Cat, Sem, Lemma)).
cac_annotate(Const) :-
  Const =.. [_, _, L, R],
  cac_annotate(L),
  cac_annotate(R).

cat_match(X0/Y0, X/Y) :-
  cat_match(X0, X),
  cat_match(Y0, Y).
cat_match(X0\Y0, X\Y) :-
  cat_match(X0, X),
  cat_match(Y0, Y).
cat_match(co(F0:A0, _, _, _), F:A) :-
  nonvar(F0),
  !,
  F0:A0 = F:A.
cat_match(co(F0:_, _, _, _), F) :-
  nonvar(F0),
  F0 = F.

% type-raising pseudo tokens
cat_annotate((X/(X\Y))/Y, _, _) :-
  !.
cat_annotate((X\(X/Y))/Y, _, _) :-
  !.
% conjunctions and punctuation
cat_annotate(((A\B)/C)\D, Sem, _) :-
  member(Sem, ['NIL', 'QUE', 'GRP', 'COO']),
  cat_match(A, Cat),
  cat_match(B, Cat),
  !,
  cat_dir(D, inv),
  cat_dir(C, inv),
  cat_dir(B, inv),
  cat_annotate_mod(A, B).
cat_annotate((A\B)\C, Sem, _) :-
  member(Sem, ['NIL', 'QUE', 'GRP', 'COO']),
  cat_match(A, Cat),
  cat_match(B, Cat),
  !,
  cat_dir(C, inv),
  cat_dir(B, inv),
  cat_annotate_mod(A, B).
cat_annotate((A\B)/C, Sem, _) :-
  member(Sem, ['NIL', 'QUE', 'GRP', 'COO']),
  cat_match(A, Cat),
  cat_match(B, Cat),
  !,
  cat_dir(C, inv),
  cat_dir(B, inv),
  cat_annotate_mod(A, B).
% adjective copulas
cat_annotate(X/Y, Sem, Lemma) :-
  member(Lemma, [be, ai]),
  cat_match(X, s:_\np),
  cat_match(Y, s:adj\np),
  !,
  cat_dir(Y, inv),
  cat_annotate(X, Sem, Lemma).
cat_annotate(X\Y, Sem, Lemma) :-
  member(Lemma, [be, ai]),
  cat_match(X, s:_\np),
  cat_match(Y, s:adj\np),
  !,
  cat_dir(Y, inv),
  cat_annotate(X, Sem, Lemma).
cat_annotate(X/Y, Sem, Lemma) :-
  member(Lemma, [be, ai]),
  cat_match(X, s:q),
  cat_match(Y, s:adj\np),
  !,
  cat_dir(Y, flip),
  cat_annotate(X, Sem, Lemma).
% noun copulas
cat_annotate(X/Y, Sem, be) :-
  cat_match(X, s:_\np:F),
  F \== thr,
  cat_match(Y, np),
  !,
  cat_dir(Y, inv),
  cat_annotate(X, Sem, be).
cat_annotate(X\Y, Sem, be) :-
  cat_match(X, s:_\np:F),
  F \== thr,
  cat_match(Y, np),
  !,
  cat_dir(Y, inv),
  cat_annotate(X, Sem, be).
% preposition copulas
cat_annotate(X/Y, Sem, be) :-
  cat_match(X, s:_\np:F),
  F \== thr,
  cat_match(Y, pp),
  !,
  cat_dir(Y, inv),
  cat_annotate(X, Sem, be).
cat_annotate(X\Y, Sem, be) :-
  cat_match(X, s:_\np:F),
  F \== thr,
  cat_match(Y, pp),
  !,
  cat_dir(Y, inv),
  cat_annotate(X, Sem, be).
% auxiliaries
cat_annotate(X/Y, Sem, have) :-
  cat_match(X, s:_\np),
  cat_match(Y, s:pt\np),
  !,
  cat_dir(Y, inv),
  cat_annotate(X, Sem, have).
cat_annotate(X\Y, Sem, have) :-
  cat_match(X, s:_\np),
  cat_match(Y, s:pt\np),
  !,
  cat_dir(Y, inv),
  cat_annotate(X, Sem, have).
cat_annotate(X/Y, Sem, will) :-
  cat_match(X, s:_\np),
  cat_match(Y, s:b\np),
  !,
  cat_dir(Y, inv),
  cat_annotate(X, Sem, have).
cat_annotate(X\Y, Sem, will) :-
  cat_match(X, s:_\np),
  cat_match(Y, s:b\np),
  !,
  cat_dir(Y, inv),
  cat_annotate(X, Sem, have).
cat_annotate(X/Y, Sem, would) :-
  cat_match(X, s:_\np),
  cat_match(Y, s:b\np),
  !,
  cat_dir(Y, inv),
  cat_annotate(X, Sem, have).
cat_annotate(X\Y, Sem, would) :-
  cat_match(X, s:_\np),
  cat_match(Y, s:b\np),
  !,
  cat_dir(Y, inv),
  cat_annotate(X, Sem, have).
cat_annotate(X/Y, Sem, be) :-
  cat_match(X, s:_\np),
  cat_match(Y, s:ng\np),
  !,
  cat_dir(Y, inv),
  cat_annotate(X, Sem, have).
cat_annotate(X\Y, Sem, be) :-
  cat_match(X, s:_\np),
  cat_match(Y, s:ng\np),
  !,
  cat_dir(Y, inv),
  cat_annotate(X, Sem, have).
cat_annotate(X/Y, Sem, be) :-
  cat_match(X, s:_\np),
  cat_match(Y, s:pss\np),
  !,
  cat_dir(Y, inv),
  cat_annotate(X, Sem, have).
cat_annotate(X\Y, Sem, be) :-
  cat_match(X, s:_\np),
  cat_match(Y, s:pss\np),
  !,
  cat_dir(Y, inv),
  cat_annotate(X, Sem, have).
% verbs with VP arguments (special case so they are not mistaken for
% modifiers)
cat_annotate(X/Y, Sem, Lemma) :-
  cat_match(X/Y, (s:b\np)/(s:b\np)),
  member(Sem, ['EXS', 'ENS', 'EPS', 'EXG', 'EXT']),
  !,
  cat_dir(Y, noninv),
  cat_annotate(X, Sem, Lemma).
cat_annotate(X\Y, Sem, Lemma) :-
  cat_match(X\Y, (s:b\np)\(s:b\np)),
  member(Sem, ['EXS', 'ENS', 'EPS', 'EXG', 'EXT']),
  !,
  cat_dir(Y, noninv),
  cat_annotate(X, Sem, Lemma).
% NPs with NP arguments
cat_annotate(X/Y, Sem, Lemma) :-
  member(Sem, ['EXG']),
  cat_match(X/Y, np/np),
  !,
  cat_dir(Y, noninv),
  cat_annotate(X, Sem, Lemma).
cat_annotate(X\Y, Sem, Lemma) :-
  member(Sem, ['EXG']),
  cat_match(X\Y, np\np),
  !,
  cat_dir(Y, noninv),
  cat_annotate(X, Sem, Lemma).
% adpositions
cat_annotate(X/Y, Sem, Lemma) :-
  cat_match(X/Y, pp/np),
  !,
  cat_dir(Y, inv),
  cat_annotate(X, Sem, Lemma).
cat_annotate(X\Y, Sem, Lemma) :-
  cat_match(X\Y, pp\np),
  !,
  cat_dir(Y, inv),
  cat_annotate(X, Sem, Lemma).
cat_annotate((X\Y)/Z, Sem, Lemma) :-
  cat_match(X\Y, A\A),
  !,
  cat_dir(Z, inv),
  cat_annotate(X\Y, Sem, Lemma).
cat_annotate((X/Y)/Z, Sem, Lemma) :-
  cat_match(X/Y, A/A),
  !,
  cat_dir(Z, inv),
  cat_annotate(X/Y, Sem, Lemma).
cat_annotate((X\Y)\Z, Sem, Lemma) :-
  cat_match(X\Y, A\A),
  !,
  cat_dir(Z, inv),
  cat_annotate(X\Y, Sem, Lemma).
cat_annotate((X/Y)\Z, Sem, Lemma) :-
  cat_match(X/Y, A/A),
  !,
  cat_dir(Z, inv),
  cat_annotate(X/Y, Sem, Lemma).
% determiners
cat_annotate(X/Y, Sem, Lemma) :-
  cat_match(X/Y, np/n),
  !,
  cat_dir(Y, inv),
  cat_annotate(X, Sem, Lemma).
cat_annotate(X/Y, Sem, Lemma) :-
  cat_match(X/Y, np/(n/pp)),
  !,
  cat_dir(Y, inv),
  cat_annotate(X, Sem, Lemma).
% subordinating conjunctions
cat_annotate(X/Y, Sem, Lemma) :-
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
  cat_annotate(X, Sem, Lemma).
% complementizers
cat_annotate(X/Y, Sem, Lemma) :-
  cat_match(X/Y, s:em/s:dcl),
  !,
  cat_dir(Y, inv),
  cat_annotate(X, Sem, Lemma).
cat_annotate(X\Y, Sem, Lemma) :-
  cat_match(X\Y, s:em\s:dcl),
  !,
  cat_dir(Y, inv),
  cat_annotate(X, Sem, Lemma).
cat_annotate(X/Y, Sem, Lemma) :-
  cat_match(X/Y, (s:to\np)/(s:b\np)),
  !,
  cat_dir(Y, inv),
  cat_annotate(X, Sem, Lemma).
cat_annotate(X\Y, Sem, Lemma) :-
  cat_match(X\Y, (s:to\np)\(s:b\np)),
  !,
  cat_dir(Y, inv),
  cat_annotate(X, Sem, Lemma).
% relative pronouns
cat_annotate(X/Y, Sem, Lemma) :-
  ( cat_match(X, n\n)
  ; cat_match(X, np\np)
  ),
  ( cat_match(Y, s:dcl\np)
  ; cat_match(Y, s:dcl/np)
  ),
  !,
  cat_dir(Y, inv),
  cat_annotate(X, Sem, Lemma).
% pseudo tokens starting reduced relative clauses
cat_annotate(X/Y, Sem, Lemma) :-
  cat_match(X, n\n),
  ( cat_match(Y, s:ng\np)
  ; cat_match(Y, s:pss\np)
  ; cat_match(Y, s:adj\np)
  ; cat_match(Y, s:dcl\np)
  ),
  !,
  cat_dir(Y, inv),
  cat_annotate(X, Sem, Lemma).
% other pseudo tokens
cat_annotate(X/Y, Sem, Lemma) :-
  ( cat_match(X/Y, (n/n)/(s:adj\np))
  ; cat_match(X/Y, (s/s)/(s:to\np))
  ; cat_match(X/Y, (s/s)/(s:pss\np))
  ),
  !,
  cat_dir(Y, inv),
  cat_annotate(X, Sem, Lemma).
% question words
cat_annotate(X/Y, Sem, Lemma) :-
  ( cat_match(X, s:wq)
  ; cat_match(X, s:wq/_)
  ),
  !,
  cat_dir(Y, inv),
  cat_annotate(X, Sem, Lemma).
% modifiers
cat_annotate(X\Y, _, _) :-
  cat_match(X, Cat),
  cat_match(Y, Cat),
  !,
  cat_dir(Y, inv),
  cat_annotate_mod(X, Y).
cat_annotate(X/Y, _, _) :-
  cat_match(X, Cat),
  cat_match(Y, Cat),
  !,
  cat_dir(Y, inv),
  cat_annotate_mod(X, Y).
% "modifiers" of relational nouns
cat_annotate(X/Y, Sem, Lemma) :-
  cat_match(X, n),
  cat_match(Y, n/pp),
  !,
  cat_dir(Y, inv),
  cat_annotate(X, Sem, Lemma).
cat_annotate(X\Y, Sem, Lemma) :-
  cat_match(X, n),
  cat_match(Y, n/pp),
  !,
  cat_dir(Y, inv),
  cat_annotate(X, Sem, Lemma).
% other function categories
cat_annotate(X\Y, Sem, Lemma) :-
  !,
  cat_dir(Y, noninv),
  cat_annotate(X, Sem, Lemma).
cat_annotate(X/Y, Sem, Lemma) :-
  !,
  cat_dir(Y, noninv),
  cat_annotate(X, Sem, Lemma).
% basic categories
cat_annotate(_, _, _).

%%	cat_annotate_mod(?X, ?Y)
%
%	Copies directionality annotations from Y to X.
cat_annotate_mod(A\B, C\D) :-
  cat_dir(D, Dir),
  cat_dir(B, Dir),
  cat_annotate_mod(A, C).
cat_annotate_mod(A/B, C/D) :-
  cat_dir(D, Dir),
  cat_dir(B, Dir),
  cat_annotate_mod(A, C).
cat_annotate_mod(co(_, _, _, _), co(_, _, _, _)).

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
  cat_annotate(Cat, 'DEF'),
  Cat = co(np:_, np, 1, _)/co(n:_, n, 2, inv).

test(dir2) :-
  cat_index((s:dcl\np)/np, Cat),
  cat_number(Cat),
  cat_annotate(Cat, 'ENS'),
  Cat = (co(s:dcl, _, _, _)\co(np:_, _, _, noninv))/co(np:_, _, _, noninv).

test(dir2a) :-
  cat_index(s\s, Cat),
  cat_number(Cat),
  cat_annotate(Cat, 'REL'),
  Cat = co(s:_, _, _, _)\co(s:_, _, _, inv).

test(dir3) :-
  cat_index((s\s)/np, Cat),
  cat_number(Cat),
  cat_annotate(Cat, 'REL'),
  write(Cat),nl,
  Cat = (co(s:_, _, _, _)\co(s:_, _, _, inv))/co(np:_, _, _, inv).

:- end_tests(dir).
