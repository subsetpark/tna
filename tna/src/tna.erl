-module(tna).
-export([parse/1]).


-define(IS_SEPARATOR(C), C =:= $\s; C =:= $,; C =:= $.; C =:= $'; C =:= $;).
-define(CONSUME(Level, S), Level(Rest, [list_to_atom(S)|Acc])).
-define(CONSUME_PRE(Level, S, Keep), Level(Rest, [list_to_atom(S), Keep|Acc])).
-define(CONSUME_POST(Level, S, Keep), Level(Rest, [Keep, list_to_atom(S)|Acc])).

-define(WHOLETOKEN(Level, S),
        Level(S, _) -> [list_to_atom(S)];).
-define(DEF(Level, S),
        Level((S ++ Rest), Acc) -> ?CONSUME(Level, S);).
-define(DEF2(Level, S),
        Level((S ++ [Sep|Rest]), Acc) when ?IS_SEPARATOR(Sep) ->
            ?CONSUME_POST(Level, S, Sep);).
-define(VOW(Level, S),
        Level(("a" ++ S ++ Rest), Acc) -> ?CONSUME_PRE(Level, S, $a);
        Level(("e" ++ S ++ Rest), Acc) -> ?CONSUME_PRE(Level, S, $e);
        Level(("i" ++ S ++ Rest), Acc) -> ?CONSUME_PRE(Level, S, $i);
        Level(("o" ++ S ++ Rest), Acc) -> ?CONSUME_PRE(Level, S, $o);
        Level(("u" ++ S ++ Rest), Acc) -> ?CONSUME_PRE(Level, S, $u);
        ).

-define(TAIL(Level),
        Level([C|Rest], Acc) -> Level(Rest, [C|Acc]);
        Level([], Acc) -> lists:reverse(Acc)).
-define(WHOLETAIL(Level),
        Level(S, _) -> S).

-define(DEFZERO(S), ?WHOLETOKEN(zero, S)).
?DEFZERO("is")
?DEFZERO("me")
?DEFZERO("and")
?DEFZERO("are")
?DEFZERO("am")
?WHOLETAIL(zero).

-define(DEFONE(S), ?DEF(one, S)).
?DEFONE("about")
?DEFONE("with")
?DEFONE("always")
?DEFONE("what")
?DEFONE("where")
?DEFONE("when")
?DEFONE("why")
?DEFONE("well")
?DEFONE("were")
?DEFONE("was")
?DEFONE("how")
?DEFONE("your")
?DEFONE("you")
?DEFONE("this")
?DEFONE("that")
?DEFONE("these")
?DEFONE("those")
?DEFONE("there")
?DEFONE("their")
?DEFONE("they")
?DEFONE("then")
?DEFONE("them")
?DEFONE("though")
?DEFONE("to")
?DEFONE("the")
?TAIL(one).

-define(DEFTWO(S), ?DEF2(two, S)).
?DEFTWO("ically")
?DEFTWO("ially")
?DEFTWO("ally")
?DEFTWO("ly")
?DEFTWO("ry")
?DEFTWO("y")
?DEFTWO("w")
?DEFTWO("nce")
?DEFTWO("ice")
?DEFTWO("ace")
?DEFTWO("ite")
?DEFTWO("ate")
?DEFTWO("e")
?DEFTWO("r")
?TAIL(two).

-define(DEFTHREE(S), ?DEF(three, S)).
?DEFTHREE("ll")
?DEFTHREE("sh")
?DEFTHREE("ch")
?DEFTHREE("th")
?DEFTHREE("ct")
?VOW(three, "nd")
?VOW(three, "nt")
?TAIL(three).

zero(S) -> zero(S, []).
one(S) -> one(S, []).
two(S) -> two(S, []).
three(S) -> three(S, []).

parse(String) ->
    Tokens = string:tokens(String, " "),
    F = fun(S) ->
                S0 = zero(S),
                S1 = one(S0),
                S2 = two(S1),
                three(S2)
        end,
    lists:map(F, Tokens).

