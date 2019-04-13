-module(prop_crosswords).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

prop_true() ->
    ?FORALL(_, term(),
        begin
            true
        end).

generate_crossword_test() ->
    GridSize = 7,
    render(GridSize, crosswords:generate_crossword(GridSize, 1)).

render(GridSize, {ChosenWords, Crossword}) ->
    ?debugFmt("~p", [lists:reverse(ChosenWords)]),
    lists:foreach(fun(X) ->
        Ys = lists:seq(0, GridSize - 1),
        Row = lists:map(fun(Y) -> maps:get({X, Y}, Crossword) end, Ys),
        ?debugFmt("~s", [Row])
    end, lists:seq(0, GridSize - 1)).
