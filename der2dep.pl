:- module(der2dep, [
    main/0]).

:- use_module(catobj, [
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
    argv/1,
    funsort/3,
    subsumed_sub_term/2,
    term_in_file/2]).

%%% MAIN %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
  format(user_error, '~w~n', [M]),
  add_toknums(Der0, Der),
  der_deps(Der, Deps),
  include(real_dep, Deps, RealDeps),
  funsort(depnum, RealDeps, SortedDeps),
  maplist(print_dep, SortedDeps),
  nl,
  N is M + 1,
  der2dep(N, Ders).
der2dep(M, [der(N, Der)|Ders]) :-
  assertion(N > M),
  nl,
  O is M + 1,
  der2dep(O, [der(N, Der)|Ders]).

%%% .der FORMAT HELPERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

atts_roles(Atts, Roles) :-
  member(verbnet:Roles, Atts),
  !.
atts_roles(_, []).

real_dep(dep(t(_, _, Token, _), _)) :-
  Token \= ø.

%%% CONVERSION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

der_deps(Der, Deps) :-
  der_coder(Der, CODer),
  coder_bind(CODer),
  coder_number(CODer),
  findall(t(Sem, CO, Token, Atts),
      ( subsumed_sub_term(t(Sem, CO, Token, Atts), CODer)
      ), Tokens0),
  find_top(TopToken, TopCO, Tokens0, Tokens),
  co_tokens_head_deps(TopCO, _, Tokens, [], TopToken, Head, Deps, [dep(Head, _)]).

find_top(TopToken, TopCO, Tokens0, Tokens) :-
  select(TopToken, Tokens0, Tokens),
  const_cat(TopToken, TopCO),
  \+ ( member(FunToken, Tokens),
       const_cat(FunToken, FunCO),
       arg_in(Y1, FunCO),
       res_in(Y2, TopCO),
       cos_bind(Y1, Y2)
     ).

co_tokens_head_deps(CO0, CO, Tokens0, Tokens, Head0, Head, [Dep|Deps0], Deps) :-
  ( CO0 = X/Y
  ; CO0 = X\Y
  ),
  select(ArgHead0, Tokens0, Tokens1),
  const_cat(ArgHead0, ArgCO0),
  co_tokens_head_deps(ArgCO0, ArgCO, Tokens1, Tokens2, ArgHead0, ArgHead, Deps0, Deps1),
  cos_bind(ArgCO, Y),
  !,
  (  cat_co(Cat, CO0),
     ( is_modifier_cat(Cat)
     ; is_adjective_cat(Cat)
     ; is_determiner_cat(Cat)
     ; is_subordinating_cat(Cat)
     ; is_complementizer_cat(Cat)
     ; is_relative_pronoun_cat(Cat)
     ; is_adposition_cat(Cat)
     ; is_auxiliary_cat(Cat)
     )
  -> Head1 = ArgHead,
     Dep = dep(Head0, ArgHead)
  ;  Head1 = Head0,
     Dep = dep(ArgHead, Head0)
  ),
  co_tokens_head_deps(X, CO, Tokens2, Tokens, Head1, Head, Deps1, Deps).
co_tokens_head_deps(conj(Y), conj(Y), Tokens0, Tokens, Head0, ArgHead, [dep(Head0, ArgHead)|Deps0], Deps) :-
  select(ArgHead0, Tokens0, Tokens1),
  const_cat(ArgHead0, ArgCO0),
  co_tokens_head_deps(ArgCO0, ArgCO, Tokens1, Tokens, ArgHead0, ArgHead, Deps0, Deps),
  cos_bound(ArgCO, Y),
  !.
co_tokens_head_deps(CO, CO, Tokens, Tokens, Head, Head, Deps, Deps).

%%% CCG HELPERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_modifier_cat(X/X).
is_modifier_cat(X\X).

is_adjective_cat(n/(n/pp)).
is_adjective_cat(n\(n/pp)).

is_determiner_cat(np/n).
is_determiner_cat(np/(n/pp)).

is_coordinating_conjunction_cat(conj).

is_subordinating_cat(Cat) :-
  is_subordinating_conjunction_cat(Cat).
is_subordinating_cat(Cat) :-
  is_complementizer_cat(Cat).
is_subordinating_cat(Cat) :-
  is_relative_pronoun_cat(Cat).

is_subordinating_conjunction_cat(Cat) :-
  member(Cat, [X/Y, X\Y]),
  member(X, [s:A\s:A, s:A/s:A, (s:A\np)\(s:A\np), (s:A\np)/(s:A\np), (s:A/np)\(s:A/np), (s:A/np)/(s:A/np)]),
  member(Y, [s:dcl, s:to, s:ng\np, s:ng/np]).

is_complementizer_cat(Cat) :-
  member(Cat, [s:em/s:dcl, s:em\s:dcl, (s:to\np)/(s:b\np), (s:to\np)\(s:b\np), (s:to/np)/(s:b/np), (s:to/np)\(s:b/np)]).

is_relative_pronoun_cat(Cat) :-
  member(Cat, [X/Y, X\Y]),
  member(X, [n\n, n/n, np\np, np/np]),
  member(Y, [s:dcl/np, s:dcl\np]).

is_adposition_cat(Cat) :-
  member(Cat, [PP/np, PP\np]),
  member(PP, [pp, n\n, n/n, np\np, np/np, s:A\s:A, s:A/s:A, (s:A\np)\(s:A\np), (s:A\np)/(s:A\np), (s:A/np)\(s:A/np), (s:A/np)/(s:A/np)]).

is_auxiliary_cat(Cat) :-
  member(Cat, [X/Y, X\Y]),
  member(X, [s:F\np, s:F/np]),
  member(Y, [s:G\np, s:G/np]),
  member(F-G, [dcl-b, b-ng, dcl-ng, ng-ng, pt-ng, b-pt, dcl-pt, ng-pt, pt-pt]). % HACK dcl-X could also be modal...

depnum(dep(t(_, _, _, Atts), _), From) :-
  member(from:From, Atts).

%%% OUTPUT HELPERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_dep(dep(_, t(_, _, _, HAtts))) :-
  once(member(toknum:HToknum, HAtts)),
  (  var(HToknum)
  -> write(0)
  ;  write(HToknum)
  ),
  nl.
