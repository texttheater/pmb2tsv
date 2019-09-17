:- module(der, [
    const_cat/2]).

const_cat(t(_, Cat, _, _), Cat) :-
  !.
const_cat(Const, Cat) :-
  arg(1, Const, Cat).
