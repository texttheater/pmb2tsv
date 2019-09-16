:- module(der2dep, [
    main/0]).

:- use_module(slashes).
:- use_module(util, [
    argv/1,
    funsort/3,
    must/1,
    term_in_file/2]).

main :-
  argv([Path]),
  findall(Der,
      ( term_in_file(Der, Path)
      ), Ders),
  der2dep(1, Ders),
  halt.
main :-
  format(user_error, 'USAGE: swipl -l der2dep.pl -g main DERFILE~n', []),
  halt(1).

% Process the derivations in order. Make sure that the numbers stay aligned
% with blocks. I.e., if derivation 645 is missing (because Boxer couldn't
% interpret it), then there will be an additional newline between the 644th and
% the 646th dependency block.
der2dep(_, []).
der2dep(M, [der(M, Der0)|Ders]) :-
  !,
  add_toknums(Der0, Der),
  der_deps(Der, Root, _, Deps, [Root-_]),
  funsort(depnum, Deps, SortedDeps),
  maplist(print_dep, SortedDeps),
  nl,
  N is M + 1,
  der2dep(N, Ders).
der2dep(M, [der(N, Der)|Ders]) :-
  assertion(N > M),
  nl,
  O is M + 1,
  der2dep(O, [der(N, Der)|Ders]).

add_toknums(Der0, Der) :-
  add_toknums(Der0, Der, 1, _).

add_toknums(t(Sem, Cat, 'ø', Atts), t(Sem, Cat, 'ø', Atts), M, M) :-
  !.
add_toknums(t(Sem, Cat, Token, Atts), t(Sem, Cat, Token, [toknum:M|Atts]), M, N) :-
  !,
  N is M + 1.
add_toknums(Const0, Const, M, N) :-
  Const0 =.. [Rule, Cat, Sem, D0],
  !,
  Const =.. [Rule, Cat, Sem, D],
  add_toknums(D0, D, M, N).
add_toknums(Const0, Const, M, N) :-
  Const0 =.. [Rule, Cat, Sem, L0, R0],
  Const =.. [Rule, Cat, Sem, L, R],
  add_toknums(L0, L, M, O),
  add_toknums(R0, R, O, N).

der_deps(t(Sem, Cat, Token, Atts), t(Sem, Cat, Token, Atts), Roles, Deps, Deps) :-
  !,
  atts_roles(Atts, Roles).
der_deps(ftr(_, _, D), Head, [], Deps0, Deps) :-
  !,
  must(der_deps(D, Head, _, Deps0, Deps)).
der_deps(btr(_, _, D), Head, [], Deps0, Deps) :-
  !,
  must(der_deps(D, Head, _, Deps0, Deps)).
der_deps(conj(_, _, C, D), ConjunctHead, [], [ConjunctionHead-ConjunctHead|Deps0], Deps) :-
  !,
  must(der_deps(C, ConjunctionHead, _, Deps0, Deps1)),
  must(der_deps(D, ConjunctHead, _, Deps1, Deps)).
der_deps(fa(_, _, t(_, _, 'ø', _), D), Head, Roles, Deps0, Deps) :-
  !,
  der_deps(D, Head, Roles, Deps0, Deps).
der_deps(Const, Head, Roles, [Dep|Deps0], Deps) :-
  comp(Const, Fun, Arg),
  must(der_deps(Fun, FunHead, FunRoles, Deps0, Deps1)),
  must(der_deps(Arg, ArgHead, ArgRoles, Deps1, Deps)),
  (  ( Arg = ftr(_, _, _)
     ; Arg = btr(_, _, _)
     ; const_cat(Fun, FunCat),
       ( is_modifier_cat(FunCat)
       ; is_determiner_cat(FunCat)
       ; is_subordinating_cat(FunCat)
       ; is_adposition_cat(FunCat)
       ; is_auxiliary_cat(FunCat)
       )
     )
  -> Head = ArgHead,
     Roles = ArgRoles,
     Dep = FunHead-ArgHead
  ;  Head = FunHead,
     (  FunRoles = [_|Roles]
     -> true
     ;  Roles = []
     ),
     Dep = ArgHead-FunHead
  ).

atts_roles(Atts, Roles) :-
  member(verbnet:Roles, Atts),
  !.
atts_roles(_, []).

comp(fa(_, _, Fun, Arg), Fun, Arg).
comp(ba(_, _, Arg, Fun), Fun, Arg).
comp(fc(_, _, Fun, Arg), Fun, Arg).
comp(bc(_, _, Arg, Fun), Fun, Arg).
comp(fxc(_, _, Fun, Arg), Fun, Arg).
comp(bxc(_, _, Arg, Fun), Fun, Arg).
comp(gfc(_, _, Fun, Arg), Fun, Arg).
comp(gbc(_, _, Arg, Fun), Fun, Arg).
comp(gfxc(_, _, Fun, Arg), Fun, Arg).
comp(gbxc(_, _, Arg, Fun), Fun, Arg).

const_cat(t(_, Cat, _, _), Cat) :-
  !.
const_cat(Const, Cat) :-
  arg(1, Const, Cat).

is_modifier_cat(X/X).
is_modifier_cat(X\X).

is_determiner_cat(np/n).
is_determiner_cat(np/(n/pp)).

is_subordinating_cat(Cat) :-
  is_subordinating_conjunction_cat(Cat).
is_subordinating_cat(Cat) :-
  is_complementizer_cat(Cat).
is_subordinating_cat(Cat) :-
  is_relative_pronoun_cat(Cat).

is_subordinating_conjunction_cat(Cat) :-
  member(Cat, [X/Y, X\Y]),
  member(X, [s\s, s/s, (s\np)\(s\np), (s\np)/(s\np), (s/np)\(s/np), (s/np)/(s/np)]),
  member(Y, [s:dcl, s:to, s:ng\np, s:ng/np]).

is_complementizer_cat(Cat) :-
  member(Cat, [s:em/s:dcl, s:em\s:dcl, (s:to\np)/(s:b\np), (s:to\np)\(s:b\np), (s:to/np)/(s:b/np), (s:to/np)\(s:b/np)]).

is_relative_pronoun_cat(Cat) :-
  member(Cat, [X/Y, X\Y]),
  member(X, [n\n, n/n, np\np, np/np]),
  member(Y, [s:dcl/np, s:dcl\np]).

is_adposition_cat(Cat) :-
  member(Cat, [PP/np, PP\np]),
  member(PP, [pp, n\n, n/n, np\np, np/np, s\s, s/s, (s\np)\(s\np), (s\np)/(s\np), (s/np)\(s/np), (s/np)/(s/np)]).

is_auxiliary_cat(Cat) :-
  member(Cat, [X/Y, X\Y]),
  member(X, [s:F\np, s:F/np]),
  member(Y, [s:G\np, s:G/np]),
  member(F-G, [dcl-b, b-ng, dcl-ng, ng-ng, pt-ng, b-pt, dcl-pt, ng-pt, pt-pt]). % HACK dcl-X could also be modal...

depnum(t(_, _, _, Atts)-_, From) :-
  member(from:From, Atts).

print_dep(_-t(_, _, _, HAtts)) :-
  member(toknum:HToknum, HAtts),
  (  var(HToknum)
  -> write(0)
  ;  write(HToknum)
  ),
  nl.
