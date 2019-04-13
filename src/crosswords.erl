-module(crosswords).

-export([word_list/1, generate_crossword/2]).

word_list(MaxLength) ->
    {ok, Data} = file:read_file("words.txt"),
    WordList = lists:map(fun(W) -> binary_to_list(W) end, binary:split(Data, [<<"\n">>], [global])),
    lists:filter(fun(W) -> length(W) =:= MaxLength end, WordList).

generate_crossword(_WordList, _GridSize) ->
    [].
