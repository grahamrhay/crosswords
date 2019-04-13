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
    WordList = crosswords:word_list(GridSize),
    render(GridSize, crosswords:generate_crossword(WordList, GridSize)).

render(GridSize, _Crossword) ->
    Grid = empty_grid(GridSize),
    lists:foreach(fun(X) ->
        Ys = lists:seq(0, GridSize - 1),
        Row = lists:map(fun(Y) -> maps:get({X, Y}, Grid) end, Ys),
        ?debugFmt("~p", [Row])
    end, lists:seq(0, GridSize - 1)).

empty_grid(GridSize) ->
    Xs = lists:seq(0, GridSize - 1),
    maps:from_list(lists:flatten(lists:map(fun(X) ->
        Ys = lists:seq(0, GridSize - 1),
        lists:map(fun(Y) -> {{X, Y}, <<" ">>} end, Ys)
    end, Xs))).
