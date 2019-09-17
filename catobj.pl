:- module(catobj, [
    cat_co/2,
    coder_bind/1,
    cos_bound/2,
    der_coder/2]).

:- use_module(der, [
    const_cat/2]).
:- use_module(slashes).
:- use_module(util, [
    subsumed_sub_term/2]).

cat_co(X0/Y0, X/Y) :-
  !,
  cat_co(X0, X),
  cat_co(Y0, Y).
cat_co(X0\Y0, X\Y) :-
  !,
  cat_co(X0, X),
  cat_co(Y0, Y).
cat_co(conj, conj(_)) :-
  !.
cat_co(B, co(B, _)).

der_coder(t(Sem, Cat, Token, Atts), t(Sem, CO, Token, Atts)) :-
  !,
  cat_co(Cat, CO).
der_coder(Der, CODer) :-
  Der =.. [Fun, Cat, Sem|Dtrs0],
  cat_co(Cat, CO),
  CODer =.. [Fun, CO, Sem|Dtrs],
  maplist(der_coder, Dtrs0, Dtrs).

coder_bind(fa(X, _, D1, D2)) :-
  const_cat(D1, X1/Y1),
  const_cat(D2, Y2),
  cos_bind(X, X1),
  cos_bind(Y1, Y2),
  coder_bind(D1),
  coder_bind(D2).
coder_bind(ba(X, _, D2, D1)) :-
  const_cat(D1, X1\Y1),
  const_cat(D2, Y2),
  cos_bind(X, X1),
  cos_bind(Y1, Y2),
  coder_bind(D1),
  coder_bind(D2).
coder_bind(fc(X/Z, _, D1, D2)) :-
  const_cat(D1, X1/Y1),
  const_cat(D2, Y2/Z2),
  cos_bind(X, X1),
  cos_bind(Y1, Y2),
  cos_bind(Z2, Z),
  coder_bind(D1),
  coder_bind(D2).
coder_bind(bc(X\Z, _, D1, D2)) :-
  const_cat(D1, X1\Y1),
  const_cat(D2, Y2\Z2),
  cos_bind(X, X1),
  cos_bind(Y1, Y2),
  cos_bind(Z2, Z),
  coder_bind(D1),
  coder_bind(D2).
coder_bind(fxc(X/Z, _, D1, D2)) :-
  const_cat(D1, X1/Y1),
  const_cat(D2, Y2\Z2),
  cos_bind(X, X1),
  cos_bind(Y1, Y2),
  cos_bind(Z2, Z),
  coder_bind(D1),
  coder_bind(D2).
coder_bind(bxc(X\Z, _, D1, D2)) :-
  const_cat(D1, X1\Y1),
  const_cat(D2, Y2/Z2),
  cos_bind(X, X1),
  cos_bind(Y1, Y2),
  cos_bind(Z2, Z),
  coder_bind(D1),
  coder_bind(D2).
coder_bind(gfc((X/Z)/A, _, D1, D2)) :-
  const_cat(D1, X1/Y1),
  const_cat(D2, (Y2/Z2)/A2),
  cos_bind(X, X1),
  cos_bind(Y1, Y2),
  cos_bind(Z2, Z),
  cos_bind(A2, A),
  coder_bind(D1),
  coder_bind(D2).
coder_bind(gbc((X\Z)\A, _, D1, D2)) :-
  const_cat(D1, X1\Y1),
  const_cat(D2, (Y2\Z2)\A2),
  cos_bind(X, X1),
  cos_bind(Y1, Y2),
  cos_bind(Z2, Z),
  cos_bind(A2, A),
  coder_bind(D1),
  coder_bind(D2).
coder_bind(gfxc((X\Z)\A, _, D1, D2)) :-
  const_cat(D1, X1/Y1),
  const_cat(D2, (Y2\Z2)\A2),
  cos_bind(X, X1),
  cos_bind(Y1, Y2),
  cos_bind(Z2, Z),
  cos_bind(A2, A),
  coder_bind(D1),
  coder_bind(D2).
coder_bind(gbxc((X/Z)/A, _, D1, D2)) :-
  const_cat(D1, X1\Y1),
  const_cat(D2, (Y2/Z2)/A2),
  cos_bind(X, X1),
  cos_bind(Y1, Y2),
  cos_bind(Z2, Z),
  cos_bind(A2, A),
  coder_bind(D1),
  coder_bind(D2).
coder_bind(ftr(A/(B\C), _, D1)) :-
  const_cat(D1, B1),
  cos_bind(A, C),
  cos_bind(B, B1),
  coder_bind(D1).
coder_bind(btr(A\(B/C), _, D1)) :-
  const_cat(D1, B1),
  cos_bind(A, C),
  cos_bind(B, B1),
  coder_bind(D1).
coder_bind(conj(_, _, D1, D2)) :-
  const_cat(D1, conj(Y1)),
  const_cat(D2, Y2),
  cos_bind(Y1, Y2),
  coder_bind(D1),
  coder_bind(D2).
coder_bind(t(_, _, _, _)).

cos_bind(X1/Y1, X2/Y2) :-
  cos_bind(X1, X2),
  cos_bind(Y1, Y2).
cos_bind(X1\Y1, X2\Y2) :-
  cos_bind(X1, X2),
  cos_bind(Y1, Y2).
cos_bind(conj(Y1), conj(Y2)) :-
  cos_bind(Y1, Y2).
cos_bind(co(_, I), co(_, I)).

cos_bound(X1/Y1, X2/Y2) :-
  cos_bound(X1, X2),
  cos_bound(Y1, Y2).
cos_bound(X1\Y1, X2\Y2) :-
  cos_bound(X1, X2),
  cos_bound(Y1, Y2).
cos_bound(conj(Y1), conj(Y2)) :-
  cos_bound(Y1, Y2).
cos_bound(co(_, I), co(_, J)) :-
  I == J.
