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
    generate_crossword(filter_words(WordList, GridSize), GridSize, TotalWords, [], #{}).

generate_crossword(WordList, GridSize, TotalWords, ChosenWords, Crossword) ->
    case add_word(WordList, GridSize, ChosenWords, Crossword, 100) of
        {true, NewChosenWords, NewCrossword} ->
            case length(NewChosenWords) =:= TotalWords of
                true -> {NewChosenWords, NewCrossword};
                false -> generate_crossword(WordList, GridSize, TotalWords, NewChosenWords, NewCrossword)
            end;
        false -> {ChosenWords, Crossword}
    end.

add_word(WordList, GridSize, ChosenWords, Crossword, DepthLimit) ->
    case DepthLimit of
        0 -> false;
        _ ->
            X = choose(lists:seq(0, GridSize - 1)),
            Y = choose(lists:seq(0, GridSize - 1)),
            Direction = choose([up, down, left, right]),
            PossibleWords = filter_words(WordList, max_word_length({X, Y}, Direction, GridSize)),
            add_new_word(PossibleWords, {X, Y}, Direction, Crossword, ChosenWords)
    end.

add_new_word(PossibleWords, Pos, Direction, Crossword, ChosenWords) ->
    case length(PossibleWords) of
        0 -> false;
        _ ->
            Word = {Pos, Direction, choose(PossibleWords)},
            case add_word_to_grid(Word, Crossword) of
                {true, NewCrossword} -> {true, [Word | ChosenWords], NewCrossword};
                false -> add_new_word(PossibleWords, Pos, Direction, Crossword, ChosenWords)
            end
    end.

add_word_to_grid({{X, Y} = Position, Direction, [Letter|Letters]}, Grid) ->
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
    case maps:is_key(Position, Grid) of
        false ->
            NewGrid = maps:put({X, Y}, Letter, Grid),
            add_word_to_grid({NextPosition, Direction, Letters}, NewGrid);
        true ->
            case maps:get(Position, Grid) of
                Letter ->
                    add_word_to_grid({NextPosition, Direction, Letters}, Grid);
                _ -> false
            end
    end;

add_word_to_grid({_Pos, _Direction, []}, Grid) ->
    {true, Grid}.

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
