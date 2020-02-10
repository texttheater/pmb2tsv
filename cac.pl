:- module(cac, [
    cac_add_toknums/2,
    cac_bind/1,
    cac_cat/2,
    cac_index/2,
    cac_number/1,
    cac_pp/1,
    cac_top/2]).

:- use_module(cat, [
    cat_id/2,
    cat_index/2,
    cat_number/3]).
:- use_module(slashes).
:- use_module(util, [
    print_indented/3]).

cac_index(t(Cat0, Form, Atts), t(Cat, Form, Atts)) :-
  cat_index(Cat0, Cat).
cac_index(conj(Cat0, D10, D20), fa(Cat, D1, D2)) :-
  cat_index(Cat0, Cat),
  cac_index(D10, D1),
  cac_index(D20, D2).
cac_index(rp(Cat0, D20, D10), ba(Cat, D2, D1)) :-
  cat_index(Cat0, Cat),
  cac_index(D10, D1),
  cac_index(D20, D2).
cac_index(lp(Cat0, D10, D20), fa(Cat, D1, D2)) :-
  cat_index(Cat0, Cat),
  cac_index(D10, D1),
  cac_index(D20, D2).
cac_index(ba(Cat0, D20, D10), ba(Cat, D2, D1)) :-
  cat_index(Cat0, Cat),
  cac_index(D10, D1),
  cac_index(D20, D2).
cac_index(fa(Cat0, D10, D20), fa(Cat, D1, D2)) :-
  cat_index(Cat0, Cat),
  cac_index(D10, D1),
  cac_index(D20, D2).
cac_index(bc(Cat0, D20, D10), bc(Cat, D2, D1)) :-
  cat_index(Cat0, Cat),
  cac_index(D10, D1),
  cac_index(D20, D2).
cac_index(fc(Cat0, D10, D20), fc(Cat, D1, D2)) :-
  cat_index(Cat0, Cat),
  cac_index(D10, D1),
  cac_index(D20, D2).
cac_index(bxc(Cat0, D20, D10), bxc(Cat, D2, D1)) :-
  cat_index(Cat0, Cat),
  cac_index(D10, D1),
  cac_index(D20, D2).
cac_index(fxc(Cat0, D10, D20), fxc(Cat, D1, D2)) :-
  cat_index(Cat0, Cat),
  cac_index(D10, D1),
  cac_index(D20, D2).
cac_index(gbc(Cat0, D20, D10), gbc(Cat, D2, D1)) :-
  cat_index(Cat0, Cat),
  cac_index(D10, D1),
  cac_index(D20, D2).
cac_index(gfc(Cat0, D10, D20), gfc(Cat, D1, D2)) :-
  cat_index(Cat0, Cat),
  cac_index(D10, D1),
  cac_index(D20, D2).
cac_index(gbxc(Cat0, D20, D10), gbxc(Cat, D2, D1)) :-
  cat_index(Cat0, Cat),
  cac_index(D10, D1),
  cac_index(D20, D2).
cac_index(gfxc(Cat0, D10, D20), gfxc(Cat, D1, D2)) :-
  cat_index(Cat0, Cat),
  cac_index(D10, D1),
  cac_index(D20, D2).
% replace type raising/changing by forward application of an empty
% element, like Boxer does for type changing.
cac_index(lx(X0/(X0\Y0), Y0, D0), fa(X/(X\Y), t((X/(X\Y))/Y, ø, [lemma:ø, sem:'NIL']), D)) :- % HACK put dummy attributes in for dir.pl; Boxer uses different semtags
  !,
  cat_index(X0, X),
  cat_index(Y0, Y),
  cac_index(D0, D).
cac_index(lx(X0\(X0/Y0), Y0, D0), fa(X\(X/Y), t((X\(X/Y))/Y, ø, [lemma:ø, sem:'NIL']), D)) :- % HACK put dummy attributes in for dir.pl; Boxer uses different semtags
  !,
  cat_index(X0, X),
  cat_index(Y0, Y),
  cac_index(D0, D).
cac_index(lx(New0, Old0, D0), fa(New, t(New/Old, ø, [lemma:ø, sem:'NIL']), D)) :- % HACK put dummy attributes in for dir.pl; Boxer uses different semtags
  cat_index(New0, New),
  cat_index(Old0, Old),
  cac_index(D0, D).

% token
cac_bind(t(_, _, _)).
% application
cac_bind(ba(X, D2, D1)) :-
  cac_cat(D1, X\Y),
  cac_cat(D2, Y),
  cac_bind(D1),
  cac_bind(D2).
cac_bind(fa(X, D1, D2)) :-
  cac_cat(D1, X/Y),
  cac_cat(D2, Y),
  cac_bind(D1),
  cac_bind(D2).
% harmonic composition
cac_bind(bc(X\Z, D2, D1)) :-
  cac_cat(D1, X\Y),
  cac_cat(D2, Y\Z),
  cac_bind(D1),
  cac_bind(D2).
cac_bind(fc(X/Z, D1, D2)) :-
  cac_cat(D1, X/Y),
  cac_cat(D2, Y/Z),
  cac_bind(D1),
  cac_bind(D2).
% crossing composition
cac_bind(bxc(X/Z, D2, D1)) :-
  cac_cat(D1, X\Y),
  cac_cat(D2, Y/Z),
  cac_bind(D1),
  cac_bind(D2).
cac_bind(fxc(X\Z, D1, D2)) :-
  cac_cat(D1, X/Y),
  cac_cat(D2, Y\Z),
  cac_bind(D1),
  cac_bind(D2).
% generalized harmonic composition (degree 2)
cac_bind(gbc((X\Z)\A, D2, D1)) :-
  cac_cat(D1, X\Y),
  cac_cat(D2, (Y\Z)\A),
  cac_bind(D1),
  cac_bind(D2).
cac_bind(gfc((X/Z)/A, D1, D2)) :-
  cac_cat(D1, X/Y),
  cac_cat(D2, (Y/Z)/A),
  cac_bind(D1),
  cac_bind(D2).
cac_bind(gbc((X\Z)/A, D2, D1)) :-
  cac_cat(D1, X\Y),
  cac_cat(D2, (Y\Z)/A),
  cac_bind(D1),
  cac_bind(D2).
cac_bind(gfc((X/Z)\A, D1, D2)) :-
  cac_cat(D1, X/Y),
  cac_cat(D2, (Y/Z)\A),
  cac_bind(D1),
  cac_bind(D2).
% generalized crossing composition (degree 2)
cac_bind(gbxc((X/Z)/A, D2, D1)) :-
  cac_cat(D1, X\Y),
  cac_cat(D2, (Y/Z)/A),
  cac_bind(D1),
  cac_bind(D2).
cac_bind(gfxc((X\Z)\A, D1, D2)) :-
  cac_cat(D1, X/Y),
  cac_cat(D2, (Y\Z)\A),
  cac_bind(D1),
  cac_bind(D2).
cac_bind(gbxc((X/Z)\A, D2, D1)) :-
  cac_cat(D1, X\Y),
  cac_cat(D2, (Y/Z)\A),
  cac_bind(D1),
  cac_bind(D2).
cac_bind(gfxc((X\Z)/A, D1, D2)) :-
  cac_cat(D1, X/Y),
  cac_cat(D2, (Y\Z)/A),
  cac_bind(D1),
  cac_bind(D2).

cac_cat(Const, Cat) :-
  arg(1, Const, Cat).

cac_number(Const) :-
  cac_number(Const, 1, _).

cac_number(t(Cat, _, _), M, N) :-
  cat_number(Cat, M, N).
cac_number(ba(Cat, D2, D1), M, N) :-
  cat_number(Cat, M, O),
  cac_number(D1, O, P),
  cac_number(D2, P, N).
cac_number(fa(Cat, D1, D2), M, N) :-
  cat_number(Cat, M, O),
  cac_number(D1, O, P),
  cac_number(D2, P, N).
cac_number(bc(Cat, D2, D1), M, N) :-
  cat_number(Cat, M, O),
  cac_number(D1, O, P),
  cac_number(D2, P, N).
cac_number(fc(Cat, D1, D2), M, N) :-
  cat_number(Cat, M, O),
  cac_number(D1, O, P),
  cac_number(D2, P, N).
cac_number(bxc(Cat, D2, D1), M, N) :-
  cat_number(Cat, M, O),
  cac_number(D1, O, P),
  cac_number(D2, P, N).
cac_number(fxc(Cat, D1, D2), M, N) :-
  cat_number(Cat, M, O),
  cac_number(D1, O, P),
  cac_number(D2, P, N).
cac_number(gbc(Cat, D2, D1), M, N) :-
  cat_number(Cat, M, O),
  cac_number(D1, O, P),
  cac_number(D2, P, N).
cac_number(gfc(Cat, D1, D2), M, N) :-
  cat_number(Cat, M, O),
  cac_number(D1, O, P),
  cac_number(D2, P, N).
cac_number(gbxc(Cat, D2, D1), M, N) :-
  cat_number(Cat, M, O),
  cac_number(D1, O, P),
  cac_number(D2, P, N).
cac_number(gfxc(Cat, D1, D2), M, N) :-
  cat_number(Cat, M, O),
  cac_number(D1, O, P),
  cac_number(D2, P, N).

cac_top(t(Cat, Form, Atts), t(Cat, Form, Atts)).
cac_top(ba(_, _, D2), Top) :-
  cac_top(D2, Top).
cac_top(fa(_, D1, _), Top) :-
  cac_top(D1, Top).
cac_top(bc(_, _, D2), Top) :-
  cac_top(D2, Top).
cac_top(fc(_, D1, _), Top) :-
  cac_top(D1, Top).
cac_top(bxc(_, _, D2), Top) :-
  cac_top(D2, Top).
cac_top(fxc(_, D1, _), Top) :-
  cac_top(D1, Top).
cac_top(gbc(_, _, D2), Top) :-
  cac_top(D2, Top).
cac_top(gfc(_, D1, _), Top) :-
  cac_top(D1, Top).
cac_top(gbxc(_, _, D2), Top) :-
  cac_top(D2, Top).
cac_top(gfxc(_, D1, _), Top) :-
  cac_top(D1, Top).

cac_pp(Const) :-
  print_indented(Const, [t(_, _, _), _/_, _\_, _:_, co(_, _, _, _, _)], [module(slashes)]).

cac_add_toknums(Der0, Der) :-
  cac_add_toknums(Der0, Der, 1, _).

cac_add_toknums(t(Cat, 'ø', Atts), t(Cat, 'ø', Atts), M, M) :-
  !.
cac_add_toknums(t(Cat, Token, Atts), t(Cat, Token, [toknum:M|Atts]), M, N) :-
  !,
  N is M + 1.
cac_add_toknums(lx(New, Old, D0), lx(New, Old, D), M, N) :-
  !,
  cac_add_toknums(D0, D, M, N).
cac_add_toknums(Const0, Const, M, N) :-
  Const0 =.. [Rule, Cat, L0, R0],
  Const =.. [Rule, Cat, L, R],
  cac_add_toknums(L0, L, M, O),
  cac_add_toknums(R0, R, O, N).
