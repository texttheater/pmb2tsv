:- module(der, [
    const_cat/2,
    der_fix/2,
    der_pp/1]).

:- use_module(slashes).
:- use_module(util, [
    print_indented/3]).

const_cat(t(_, Cat, _, _), Cat) :-
  !.
const_cat(Const, Cat) :-
  arg(1, Const, Cat).

der_pp(Der) :-
  print_indented(Der, [t(_, _, _, _), lam(_, _), _\_, _/_, co(_, _), conj(_)], [module(slashes)]).

% Using a type-raised category as an argument (to punctuation) is an
% abomination to the Gods and a bane to our dependency conversion code.
der_fix(fa(X\(X/Y), Sem, t(Sem1, (X\(X/Y))/(X\(X/Y)), Token, Atts), btr(X\(X/Y), Sem2, D0)),
    fxc(X\(X/Y), Sem, t(Sem1, X/X, Token, Atts), btr(X\(X/Y), Sem2, D))) :-
  !,
  format(user_error, 'fixing~n', []),
  der_fix(D0, D).
der_fix(ba(X\(X/Y), Sem, btr(X\(X/Y), Sem2, D0), t(Sem1, (X\(X/Y))\(X\(X/Y)), Token, Atts)),
    bc(X\(X/Y), Sem, btr(X\(X/Y), Sem2, D), t(Sem1, X\X, Token, Atts))) :-
  !,
  format(user_error, 'fixing~n', []),
  der_fix(D0, D).
der_fix(t(Sem, Cat, Tok, Atts), t(Sem, Cat, Tok, Atts)) :-
  !.
der_fix(Der0, Der) :-
  Der0 =.. [Rule, Cat, Sem|Dtrs0],
  maplist(der_fix, Dtrs0, Dtrs),
  Der =.. [Rule, Cat, Sem|Dtrs].
