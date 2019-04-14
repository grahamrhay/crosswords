-module(crosswords).

-include_lib("eunit/include/eunit.hrl").

-export([generate_crossword/1]).

load_words() ->
    {ok, Data} = file:read_file("words.txt"),
    lists:map(fun(W) -> binary_to_list(W) end, binary:split(Data, [<<"\n">>], [global])).

filter_words(WordList, MaxLength) ->
    lists:filter(fun(W) -> length(W) =< MaxLength end, WordList).

generate_crossword(GridSize) ->
    WordList = load_words(),
    generate_crossword(filter_words(WordList, GridSize), GridSize, [], #{}, 0).

generate_crossword(WordList, GridSize, ChosenWords, Crossword, Failures) ->
    case length(ChosenWords) of
        5 ->
            {ChosenWords, Crossword};
        _ ->
            case Failures of
                5 ->
                    {ChosenWords, Crossword};
                _ ->
                    case add_word(WordList, GridSize, ChosenWords, Crossword) of
                        {true, NewChosenWords, NewCrossword} ->
                            ?debugFmt("~p", [NewChosenWords]),
                            AllWordsValid = check_words(WordList, NewCrossword, GridSize),
                            ?debugFmt("All words valid ~p", [AllWordsValid]),
                            case AllWordsValid of
                                true ->
                                    generate_crossword(WordList, GridSize, NewChosenWords, NewCrossword, 0);
                                false ->
                                    generate_crossword(WordList, GridSize, ChosenWords, Crossword, Failures + 1)
                            end;
                        false ->
                            generate_crossword(WordList, GridSize, ChosenWords, Crossword, Failures + 1)
                    end
            end
    end.

add_word(WordList, GridSize, ChosenWords, Crossword) ->
    X = choose(lists:seq(0, GridSize - 1)),
    Y = choose(lists:seq(0, GridSize - 1)),
    Direction = choose([across, down]),
    PossibleWords = filter_words(WordList, max_word_length({X, Y}, Direction, GridSize)),
    add_new_word(PossibleWords, {X, Y}, Direction, Crossword, ChosenWords).

add_new_word(PossibleWords, Pos, Direction, Crossword, ChosenWords) ->
    case length(PossibleWords) of
        0 -> false;
        _ ->
            ChosenWord = choose(PossibleWords),
            Word = {Pos, Direction, ChosenWord},
            case add_word_to_grid(Word, Crossword) of
                {true, NewCrossword} ->
                    {true, [Word | ChosenWords], NewCrossword};
                false ->
                    ?debugFmt("Unable to fit: ~s", [ChosenWord]),
                    RemainingWords = lists:filter(fun(W) -> W =/= ChosenWord end, PossibleWords),
                    add_new_word(RemainingWords, Pos, Direction, Crossword, ChosenWords)
            end
    end.

add_word_to_grid({{X, Y} = Position, Direction, [Letter|Letters]}, Grid) ->
    NextPosition = case Direction of
        across ->
            {X, Y + 1};
        down ->
            {X + 1, Y}
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
        across ->
            GridSize - Y;
        down ->
            GridSize - X
    end.

check_words(WordList, Crossword, GridSize) ->
    case check_words_across(WordList, Crossword, GridSize) of
        true ->
            check_words_down(WordList, Crossword, GridSize);
        false ->
            false
    end.

check_words_across(WordList, Crossword, GridSize) ->
    check_words_across(WordList, Crossword, GridSize, {0, 0}, [], []).

check_words_across(WordList, Crossword, GridSize, {X, Y} = Pos, Letters, Words) ->
    {NewLetters, NewWords} = case maps:is_key(Pos, Crossword) of
        true ->
            {[maps:get(Pos, Crossword) | Letters], Words};
        false ->
            case length(Letters) of
                0 ->
                    {[], Words};
                1 ->
                    {[], Words};
                _ ->
                    {[], [lists:reverse(Letters) | Words]}
            end
    end,
    NewY = Y + 1,
    case NewY =:= GridSize of
        true ->
            AllWords = case length(Letters) of
                0 -> NewWords;
                _ -> [lists:reverse(NewLetters) | NewWords]
            end,
            ?debugFmt("Words for row ~p: ~p", [X, AllWords]),
            NewX = X + 1,
            case NewX =:= GridSize of
                true ->
                    validate_words(WordList, AllWords);
                false ->
                    check_words_across(WordList, Crossword, GridSize, {NewX, 0}, [], AllWords)
            end;
        false ->
            check_words_across(WordList, Crossword, GridSize, {X, NewY}, NewLetters, NewWords)
    end.

check_words_down(WordList, Crossword, GridSize) ->
    check_words_down(WordList, Crossword, GridSize, {0, 0}, [], []).

check_words_down(WordList, Crossword, GridSize, {X, Y} = Pos, Letters, Words) ->
    {NewLetters, NewWords} = case maps:is_key(Pos, Crossword) of
        true ->
            {[maps:get(Pos, Crossword) | Letters], Words};
        false ->
            case length(Letters) of
                0 ->
                    {[], Words};
                1 ->
                    {[], Words};
                _ ->
                    {[], [lists:reverse(Letters) | Words]}
            end
    end,
    NewX = X + 1,
    case NewX =:= GridSize of
        true ->
            AllWords = case length(NewLetters) of
                0 -> NewWords;
                _ -> [lists:reverse(NewLetters) | NewWords]
            end,
            ?debugFmt("Words for col ~p: ~p", [Y, AllWords]),
            NewY = Y + 1,
            case NewY =:= GridSize of
                true ->
                    validate_words(WordList, AllWords);
                false ->
                    check_words_down(WordList, Crossword, GridSize, {0, NewY}, [], AllWords)
            end;
        false ->
            check_words_down(WordList, Crossword, GridSize, {NewX, Y}, NewLetters, NewWords)
    end.

validate_words(WordList, Words) ->
    lists:all(fun(W) -> lists:member(W, WordList) end, Words).
