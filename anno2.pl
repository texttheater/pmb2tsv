:- module(anno, [
    cac_annotate/1]).

:- use_module(co, [
    co_match/2]).

cac_annotate(t(Cat, _, Atts)) :-
  member(sem:Sem, Atts),
  member(lemma:Lemma, Atts),
  cat_annotate(Cat, Sem, Lemma).
cac_annotate(ba(_, D2, D1)) :-
  cac_annotate(D1),
  cac_annotate(D2).
cac_annotate(fa(_, D1, D2)) :-
  cac_annotate(D1),
  cac_annotate(D2).
cac_annotate(bc(_, D2, D1)) :-
  cac_annotate(D1),
  cac_annotate(D2).
cac_annotate(fc(_, D1, D2)) :-
  cac_annotate(D1),
  cac_annotate(D2).
cac_annotate(bxc(_, D2, D1)) :-
  cac_annotate(D1),
  cac_annotate(D2).
cac_annotate(fxc(_, D1, D2)) :-
  cac_annotate(D1),
  cac_annotate(D2).
cac_annotate(gbc(_, D2, D1)) :-
  cac_annotate(D1),
  cac_annotate(D2).
cac_annotate(gfc(_, D1, D2)) :-
  cac_annotate(D1),
  cac_annotate(D2).
cac_annotate(gbxc(_, D2, D1)) :-
  cac_annotate(D1),
  cac_annotate(D2).
cac_annotate(gfxc(_, D1, D2)) :-
  cac_annotate(D1),
  cac_annotate(D2).
cac_annotate(lx(_, _, D)) :-
  cac_annotate(D).

% conjunctions and punctuation
cat_annotate(b(ID, f(ID, b(ID, X, X), Y), Z), Sem, Lemma) :-
  member(Sem, ['NIL', 'QUE', 'GRP', 'COO']),
  !,
  arg(1, X, ID),
  arg(1, Y, ID),
  arg(1, Z, ID).
cat_annotate(b(ID, b(ID, X, X), Y), Sem, Lemma) :-
  member(Sem, ['NIL', 'QUE', 'GRP', 'COO']),
  !,
  arg(1, X, ID),
  arg(1, Z, ID).
cat_annotate(f(ID, b(ID, X, X), Y), Sem, Lemma) :-
  member(Sem, ['NIL', 'QUE', 'GRP', 'COO']),
  !,
  arg(1, X, ID),
  arg(1, Z, ID).
% adjective copulas
