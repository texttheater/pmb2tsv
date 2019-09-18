:- module(catobj, [
    arg_in/2,
    cat_co/2,
    coder_bind/1,
    coder_number/1,
    cos_bind/2,
    der_coder/2,
    res_in/2]).

:- use_module(der, [
    const_cat/2]).
:- use_module(slashes).
:- use_module(util, [
    subsumed_sub_term/2]).

%%	cat_co(?Cat, ?CO)
%
%	True if Cat is a CCG category and CO is a corresponding "category
%	object", where basic categories are augmented with indices. In
%	generated category objects, the indices are variable.
cat_co(X0/Y0, X/Y) :-
  !,
  cat_co(X0, X),
  cat_co(Y0, Y).
cat_co(X0\Y0, X\Y) :-
  !,
  cat_co(X0, X),
  cat_co(Y0, Y).
cat_co(conj:Y0, conj((A\B)/C)) :-
  !,
  cat_co(Y0, C),
  cat_co(Y0, B),
  cat_co(Y0, A).
cat_co(B, co(B, _)).

%%	der_coder(+Der, -CODer)
%
%	Replaces categories in Boxer derivations with category objects.
der_coder(t(Sem, Cat, Token, Atts), t(Sem, CO, Token, Atts)) :-
  !,
  cat_co(Cat, CO).
der_coder(Der, CODer) :-
  Der =.. [Fun, Cat, Sem|Dtrs0],
  cat_co(Cat, CO),
  maplist(der_coder, Dtrs0, Dtrs),
  CODer =.. [Fun, CO, Sem|Dtrs].

%%	cos_bind(+CO1, +CO2)
%
%	Identifies the indices in the category objects CO1 and CO2.
cos_bind(X1/Y1, X2/Y2) :-
  cos_bind(X1, X2),
  cos_bind(Y1, Y2).
cos_bind(X1\Y1, X2\Y2) :-
  cos_bind(X1, X2),
  cos_bind(Y1, Y2).
cos_bind(conj(Y1), conj(Y2)) :-
  cos_bind(Y1, Y2).
cos_bind(co(_, I), co(_, I)).

%%	coder_bind(+CODer)
%
%	In a derivation with category objects with variable indices, binds the
%	variables to enforce equalities mandated by CCG's combinators.
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
coder_bind(bc(X\Z, _, D2, D1)) :-
  const_cat(D1, X1\Y1),
  const_cat(D2, Y2\Z2),
  cos_bind(X, X1),
  cos_bind(Y1, Y2),
  cos_bind(Z2, Z),
  coder_bind(D1),
  coder_bind(D2).
coder_bind(fxc(X\Z, _, D1, D2)) :-
  const_cat(D1, X1/Y1),
  const_cat(D2, Y2\Z2),
  cos_bind(X, X1),
  cos_bind(Y1, Y2),
  cos_bind(Z2, Z),
  coder_bind(D1),
  coder_bind(D2).
coder_bind(bxc(X/Z, _, D2, D1)) :-
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
coder_bind(gbc((X\Z)\A, _, D2, D1)) :-
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
coder_bind(gbxc((X/Z)/A, _, D2, D1)) :-
  const_cat(D1, X1\Y1),
  const_cat(D2, (Y2/Z2)/A2),
  cos_bind(X, X1),
  cos_bind(Y1, Y2),
  cos_bind(Z2, Z),
  cos_bind(A2, A),
  coder_bind(D1),
  coder_bind(D2).
coder_bind(ftr(A/(B\C), _, D1)) :-
  const_cat(D1, C1),
  cos_bind(A, B),
  cos_bind(C, C1),
  coder_bind(D1).
coder_bind(btr(A\(B/C), _, D1)) :-
  const_cat(D1, C1),
  cos_bind(A, B),
  cos_bind(C, C1),
  coder_bind(D1).
coder_bind(conj(X, _, D1, D2)) :-
  const_cat(D1, conj(X1/Y1)),
  const_cat(D2, Y2),
  cos_bind(X, X1),
  cos_bind(Y1, Y2),
  coder_bind(D1),
  coder_bind(D2).
coder_bind(t(_, _, _, _)).

%%	co_number(+CO, +M, -N)
%
%	Replaces variable indices in category objects with integers, starting
%	from M.
co_number(X/Y, M, N) :-
  co_number(X, M, O),
  co_number(Y, O, N).
co_number(X\Y, M, N) :-
  co_number(X, M, O),
  co_number(Y, O, N).
co_number(conj(Y), M, N) :-
  co_number(Y, M, N).
co_number(co(_, I), M, N) :-
  (  var(I)
  -> I = M,
     N is M + 1
  ;  N = M
  ).

%%	coder_number(+CODer)
%
%	In a derivation with category objects, replaces variable indices with
%	integers.
coder_number(CODer) :-
  coder_number(CODer, 1, _).

coder_number(t(_, CO, _, _), M, N) :-
  !,
  co_number(CO, M, N).
coder_number(Der, M, N) :-
  Der =.. [_, CO, _, D],
  !,
  co_number(CO, M, O),
  coder_number(D, O, N).
coder_number(Der, M, N) :-
  Der =.. [_, CO, _, D1, D2],
  co_number(CO, M, O),
  coder_number(D1, O, P),
  coder_number(D2, P, N).

%%	res_in(+X1, +X2)
%
%	For X1, X2 category objects with integer indices, true if X1 appears as
%	a result category in X2.
res_in(X1, X2) :-
  cos_bind(X1, X2).
res_in(X1, X2/_) :-
  res_in(X1, X2).
res_in(X1, X2\_) :-
  res_in(X1, X2).
res_in(X1, conj(X2)) :-
  res_in(X1, X2).

%%	arg_in(+Y1, +X2)
%
%	For Y1, X2 category objects with integer indices, true if Y1 appears as
%	an argument category in X2.
arg_in(Y1, _/Y2) :-
  cos_bind(Y1, Y2).
arg_in(Y1, _\Y2) :-
  cos_bind(Y1, Y2).
arg_in(Y1, conj(X2)) :-
  arg_in(Y1, X2).
arg_in(Y1, X2/_) :-
  arg_in(Y1, X2).
arg_in(Y1, X2\_) :-
  arg_in(Y1, X2).
