:- module(der, [
    const_cat/2,
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
