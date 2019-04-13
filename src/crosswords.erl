-module(crosswords).

-include_lib("eunit/include/eunit.hrl").

-export([generate_crossword/2]).

load_words() ->
    {ok, Data} = file:read_file("words.txt"),
    lists:map(fun(W) -> binary_to_list(W) end, binary:split(Data, [<<"\n">>], [global])).

filter_words(WordList, MaxLength) ->
    lists:filter(fun(W) -> length(W) =< MaxLength end, WordList).

generate_crossword(GridSize, TotalWords) ->
    WordList = load_words(),
    generate_crossword(filter_words(WordList, GridSize), GridSize, TotalWords, [], empty_grid(GridSize)).

generate_crossword(WordList, GridSize, TotalWords, ChosenWords, Crossword) ->
    case add_word(WordList, GridSize, ChosenWords, Crossword, 100) of
        {true, NewChosenWords, NewCrossword} ->
            case length(NewChosenWords) =:= TotalWords of
                true -> {NewChosenWords, NewCrossword};
                false -> generate_crossword(WordList, GridSize, TotalWords, NewChosenWords, NewCrossword)
            end;
        false -> {ChosenWords, Crossword}
    end.

empty_grid(GridSize) ->
    Xs = lists:seq(0, GridSize - 1),
    maps:from_list(lists:flatten(lists:map(fun(X) ->
        Ys = lists:seq(0, GridSize - 1),
        lists:map(fun(Y) -> {{X, Y}, <<" ">>} end, Ys)
    end, Xs))).

add_word(WordList, GridSize, ChosenWords, Crossword, DepthLimit) ->
    case DepthLimit of
        0 -> false;
        _ ->
            X = choose(lists:seq(0, GridSize - 1)),
            Y = choose(lists:seq(0, GridSize - 1)),
            Direction = choose([up, down, left, right]),
            PossibleWords = filter_words(WordList, max_word_length({X, Y}, Direction, GridSize)),
            case length(PossibleWords) of
                0 -> false;
                _ ->
                    Word = {{X,Y}, Direction, choose(PossibleWords)},
                    {true, [Word | ChosenWords], add_word_to_grid(Word, Crossword)}
            end
    end.

add_word_to_grid({{X, Y}, Direction, [Letter|Letters]}, Grid) ->
    NewGrid = maps:put({X, Y}, Letter, Grid),
    NextPosition = case Direction of
        up ->
            {X - 1, Y};
        down ->
            {X + 1, Y};
        left ->
            {X, Y - 1};
        right ->
            {X, Y + 1}
    end,
    add_word_to_grid({NextPosition, Direction, Letters}, NewGrid);

add_word_to_grid({_Pos, _Direction, []}, Grid) ->
    Grid.

choose(X) ->
    lists:nth(rand:uniform(length(X)), X).

max_word_length({X, Y}, Direction, GridSize) ->
    case Direction of
        up ->
            X + 1;
        down ->
            GridSize - X;
        left ->
            Y + 1;
        right ->
            GridSize - Y
    end.
