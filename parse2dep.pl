:- module(parse2dep, [
    main/0]).

/** <module> Convert C&C-style CCG derivations to dependency trees
*/

:- use_module(cac, [
    cac_add_toknums/2,
    cac_cat/2,
    cac_index/2,
    cac_bind/1,
    cac_number/1,
    cac_pp/1,
    cac_top/2]).
:- use_module(cat, [
    arg_in/2,
    cat_dir/2,
    cat_id/2,
    cat_is_pseudo/1,
    cat_match/2,
    cat_role/2,
    res_in/2]).
:- use_module(anno, [
    cac_annotate/2]).
:- use_module(slashes).
:- use_module(util, [
    funsort/3,
    must/1,
    subsumed_sub_term/2,
    term_in_file/3]).

%:- debug(snum).
%:- debug(original_const).
%:- debug(const_with_toknums).
%:- debug(indexed_const).
%:- debug(bound_const).
%:- debug(numbered_const).
%:- debug(annotated_const).
%:- debug(top).
%:- debug(dep).
%:- debug(flip).
%:- debug(opts).

opts_spec([
    [opt(det), type(boolean), default(true), longflags([det]),
     help('attach determiners to their nouns')],
    [opt(adp), type(boolean), default(true), longflags([adp]),
     help('attach prepositions to their objects')],
    [opt(cop), type(boolean), default(true), longflags([cop]),
     help('attach noun/adposition copula to noun/adposition')],
    [opt(adj), type(boolean), default(true), longflags([adj]),
     help('attach attributive adjective to noun')],
    [opt(aux), type(boolean), default(true), longflags([aux]),
     help('attach auxiliaries and adjective copulas to their complement')],
    [opt(quasiaux), type(boolean), default(false),
     longflags([quasiaux]),
     help('attach quasi-auxiliaries (used to, going to) to verb (only with aux option)')],
    [opt(coord), type(boolean), default(true), longflags([coord]),
     help('attach coordinating conjunctions to the second conjunct')]]).

main :-
  opts_spec(OptsSpec),
  opt_arguments(OptsSpec, Opts, PosArgs),
  debug(opts, '~w', [Opts]),
  PosArgs = [CacFile],
  forall(
      ( term_in_file(ccg(N, Const), CacFile, [module(slashes)])
      ),
      ( cac2dep(N, Const, Opts)
      ) ),
  halt.
main :-
  format(user_error, 'USAGE: swipl -l parse2dep -g main CACFILE~n', []),
  format(user_error, 'Options:~n', []),
  opts_spec(OptsSpec),
  opt_help(OptsSpec, Help),
  format(user_error, '~w', [Help]),
  halt(1).

cac2dep(N, Const0, Opts) :-
  debug(snum, '~w', [N]),
  debug(original_const, 'original: ~@', [cac_pp(Const0)]),
  cac_add_toknums(Const0, Const1),
  debug(const_with_toknums, 'with toknums: ~@', [cac_pp(Const1)]),
  cac_index(Const1, Const),
  debug(indexed_const, 'indexed: ~@', [cac_pp(Const)]),
  (  cac_bind(Const)
  -> debug(bound_const, 'bound: ~@', [cac_pp(Const)]),
     cac_number(Const),
     debug(numbered_const, 'numbered: ~@', [cac_pp(Const)]),
     cac_annotate(Const, Opts),
     debug(annotated_const, 'annotated: ~@', [cac_pp(Const)]),
     %( N = 1 -> gtrace ; true ),
     (  cac2dep(Const)
     -> true
     ;  format(user_error, 'WARNING: failed to convert derivation ~w, skipping~n', [N]),
        nl
     )
  ;  format(user_error, 'WARNING: failed to preprocess derivation ~w, skipping~n', [N]),
     nl
  ).

cac2dep(Const) :-
  % find top token
  findall(t(Cat, Form, Atts),
      ( subsumed_sub_term(t(Cat, Form, Atts), Const)
      ), Tokens),
  find_top(Const, Tokens, Top),
  debug(top, 'top: ~w', [Top]),
  % convert to dependencies
  t2dep(Top, Tokens, _, Deps, []),
  % output for debugging
  forall(
      ( member(dep(D, Role, H), Deps)
      ),
      ( debug(dep, '~@ <-~w- ~@', [cac_pp(D), Role, cac_pp(H)])
      ) ),
  % postprocess
  (  Deps = []
  -> Tokens = [Token],
     RootedDeps = [dep(Token, _, _)]
  ;  add_root_dep(Deps, RootedDeps)
  ),
  exclude(pseudo_dep, RootedDeps, RealDeps),
  flip_deps(RealDeps, FlippedDeps),
  rerole_deps(FlippedDeps, ReroledDeps),
  funsort(depfrom, ReroledDeps, SortedDeps),
  % sanity check (HACK)
  exclude(pseudo_tok, Tokens, RealTokens),
  length(RealTokens, NumTokens),
  length(SortedDeps, NumDeps),
  (  NumTokens == NumDeps
  -> true
  ;  fail
  ),
  % output
  maplist(dep_pp, SortedDeps),
  nl.

% reassign role labels from adpositions to their arguments
rerole_deps(Deps0, Deps) :-
  select(dep(P, Role, A), Deps0, Deps1),
  nonvar(A),
  nonvar(Role),
  cac_cat(P, PCat),
  ( cat_match(PCat, pp/np)
  ; cat_match(PCat, pp\np)
  ),
  select(dep(A, _, H), Deps1, Deps2),
  !,
  rerole_deps([dep(P, _, A), dep(A, Role, H)|Deps2], Deps).
rerole_deps(Deps, Deps).

% reattach words with "flip" annotation to their grandparents
% (e.g., reattach subjects that are currently attached to an adjective copula
% to the adjective)
flip_deps(Deps0, Deps) :-
  select(dep(C, CRole, A), Deps0, Deps1),
  nonvar(A),
  cac_cat(C, CCat),
  arg_in(Arg, CCat),
  res_in(co(_, _, _, flip, _), Arg),
  select(dep(S, SRole, H), Deps1, Deps2),
  nonvar(H),
  H = C,
  !,
  debug(flip, 'moving ~w from ~w to ~w', [S, C, A]),
  flip_deps([dep(C, CRole, A), dep(S, SRole, A)|Deps2], Deps).
flip_deps(Deps, Deps).

replace_head(Old, New, dep(D, Role, Old0), dep(D, Role, New)) :-
  nonvar(Old0),
  Old = Old0,
  !.
replace_head(_, _, dep(D, Role, H), dep(D, Role, H)).

%%	add_root_dep(+Deps, +Top, -RootedDeps)
%
%	Above the highest node in the dependency tree Deps, add another
%	dependency to an artificial root node, represented by an unbound
%	variable.
add_root_dep(Deps, [dep(H, _, _)|Deps]) :-
  select_highest_dep(dep(_, _, H), Deps, _).

select_highest_dep(dep(D, Role, H), Deps0, Deps) :-
  select(dep(D, Role, H), Deps0, Deps),
  \+ member(dep(H, _, _), Deps0).

t2dep(Token, Tokens, Head, Deps0, Deps) :-
  cac_cat(Token, Cat),
  cat2dep(Cat, Tokens, Token, Head, Deps0, Deps).

% Find a token whose top category does not appear as an argument.
% Prefer real tokens, but return a pseudotoken if no other exists.
find_top(Const, Tokens, Top) :-
  cac_top(Const, Top0),
  cac_cat(Top0, Cat0),
  cat_id(Cat0, ID),
  (  member(Top, Tokens),
     cac_cat(Top, Cat),
     \+ cat_is_pseudo(Cat),
     cat_id(Cat, ID)
  -> true
  ;  Top = Top0
  ).

% Find the argument corresponding to the given category Y.
% Prefer real tokens, but return pseudotokens if no other exists.
find_arg(Y, Tokens, Arg) :-
  cat_id(Y, ArgID),
  member(Arg, Tokens),
  cac_cat(Arg, ArgCat),
  cat_id(ArgCat, ArgID),
  \+ cat_is_pseudo(ArgCat),
  !.
find_arg(Y, Tokens, Arg) :-
  cat_id(Y, ArgID),
  member(Arg, Tokens),
  cac_cat(Arg, ArgCat),
  cat_id(ArgCat, ArgID).

% right modifier of backward type-raised category
cat2dep((A\(B/C))\(X\(X/Y)), Tokens, Head0, Head, Deps0, Deps) :-
  !,
  arg2dep(X\(X/Y), Tokens, Head0, Head1, Deps0, Deps1),
  cat_dir(C, Dir),
  (  Dir = noninv
  -> ( arg2dep_inv(B/C, Tokens, Head1, Head2, Deps1, Deps2)
     ; Head1 = Head2,
       Deps1 = Deps2
     )
  ;  ( arg2dep_noninv(B/C, Tokens, Head1, Head2, Deps1, Deps2)
     ; Head1 = Head2,
       Deps1 = Deps2
     )
  ),
  cat2dep(A, Tokens, Head2, Head, Deps2, Deps).
% left modifier of backward type-raised category
cat2dep((A\(B/C))/(X\(X/Y)), Tokens, Head0, Head, Deps0, Deps) :-
  !,
  arg2dep(X\(X/Y), Tokens, Head0, Head1, Deps0, Deps1),
  cat_dir(C, Dir),
  (  Dir = noninv
  -> ( arg2dep_inv(B/C, Tokens, Head1, Head2, Deps1, Deps2)
     ; Head1 = Head2,
       Deps1 = Deps2
     )
  ;  ( arg2dep_noninv(B/C, Tokens, Head1, Head2, Deps1, Deps2)
     ; Head1 = Head2,
       Deps1 = Deps2
     )
  ),
  cat2dep(A, Tokens, Head2, Head, Deps2, Deps).
% right modifier of forward type-raised category
cat2dep((A/(B\C))\(X/(X\Y)), Tokens, Head0, Head, Deps0, Deps) :-
  !,
  arg2dep(X/(X\Y), Tokens, Head0, Head1, Deps0, Deps1),
  cat_dir(C, Dir),
  (  Dir = noninv
  -> arg2dep_inv(B\C, Tokens, Head1, Head2, Deps1, Deps2)
  ;  arg2dep_noninv(B\C, Tokens, Head1, Head2, Deps1, Deps2)
  ),
  cat2dep(A, Tokens, Head2, Head, Deps2, Deps).
% left modifier of forward type-raised category
cat2dep((A/(B\C))/(X/(X\Y)), Tokens, Head0, Head, Deps0, Deps) :-
  !,
  arg2dep(X/(X\Y), Tokens, Head0, Head1, Deps0, Deps1),
  cat_dir(C, Dir),
  (  Dir = noninv
  -> arg2dep_inv(B\C, Tokens, Head1, Head2, Deps1, Deps2)
  ;  arg2dep_noninv(B\C, Tokens, Head1, Head2, Deps1, Deps2)
  ),
  cat2dep(A, Tokens, Head2, Head, Deps2, Deps).
% TODO
% forward type-raising pseudo-slash
cat2dep((X/(X\Y))/Y, Tokens, Head0, Head, Deps0, Deps) :-
  !,
  arg2dep_inv(Y, Tokens, Head0, Head, Deps0, Deps).
% backward type-raising pseudo-slash
cat2dep((X\(X/Y))/Y, Tokens, Head0, Head, Deps0, Deps) :-
  !,
  arg2dep_inv(Y, Tokens, Head0, Head, Deps0, Deps).
% forward slash
cat2dep(X/Y, Tokens, Head0, Head, Deps0, Deps) :-
  arg2dep(Y, Tokens, Head0, Head1, Deps0, Deps1),
  !,
  cat2dep(X, Tokens, Head1, Head, Deps1, Deps).
% backward slash
cat2dep(X\Y, Tokens, Head0, Head, Deps0, Deps) :-
  arg2dep(Y, Tokens, Head0, Head1, Deps0, Deps1),
  !,
  cat2dep(X, Tokens, Head1, Head, Deps1, Deps).
% no slash
cat2dep(_, _, Head, Head, Deps, Deps).

arg2dep(Y, Tokens, Head0, Head, Deps0, Deps) :-
  findall(Arg,
      ( find_arg(Y, Tokens, Arg)
      ), Args),
  cat_dir(Y, Dir),
  cat_role(Y, Role),
  (  Dir = noninv
  -> args2deps_noninv(Role, Args, Tokens, Head0, Head, Deps0, Deps)
  ;  args2deps_inv(Role, Args, Tokens, Head0, Head, Deps0, Deps)
  ).

% version of the above that inverts the dependency regardless of annotation
arg2dep_inv(Y, Tokens, Head0, Head, Deps0, Deps) :-
  findall(Arg,
      ( find_arg(Y, Tokens, Arg)
      ), Args),
  Args \= [],
  cat_role(Y, Role),
  args2deps_inv(Role, Args, Tokens, Head0, Head, Deps0, Deps).

% version of the above that does not invert the dependency regardless of annotation
arg2dep_noninv(Y, Tokens, Head0, Head, Deps0, Deps) :-
  findall(Arg,
      ( find_arg(Y, Tokens, Arg)
      ), Args),
  Args \= [],
  cat_role(Y, Role),
  args2deps_noninv(Role, Args, Tokens, Head0, Head, Deps0, Deps).

args2deps_noninv(_, [], _, Head, Head, Deps, Deps).
args2deps_noninv(Role, [Arg|Args], Tokens, Head0, Head, [dep(ArgHead, Role, Head0)|Deps0], Deps) :-
  t2dep(Arg, Tokens, ArgHead, Deps0, Deps1),
  args2deps_noninv(Role, Args, Tokens, Head0, Head, Deps1, Deps).

args2deps_inv(_, [], _, Head, Head, Deps, Deps).
args2deps_inv(Role, [Arg|Args], Tokens, Head0, ArgHead, [dep(Head0, Role, ArgHead)|Deps0], Deps) :-
  t2dep(Arg, Tokens, ArgHead, Deps0, Deps1),
  args2deps_inv(Role, Args, Tokens, Head0, _, Deps1, Deps).

pseudo_dep(dep(t(_, ø, _), _, _)).

pseudo_tok(t(_, ø, _)).

depfrom(dep(t(_, _, Atts), _, _), From) :-
  member(from:From, Atts).

dep_pp(dep(_, Role, t(_, _, HeadAtts))) :-
  (  var(HeadAtts)
  -> HeadToknum = 0
  ;  member(toknum:HeadToknum, HeadAtts)
  ),
  (  var(Role)
  -> _RoleAtom = 'O'
  ;  _RoleAtom = Role
  ),
  format('~w~n', [HeadToknum]). % not outputting roles for now
