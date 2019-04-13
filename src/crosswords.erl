-module(crosswords).

-export([word_list/0]).

word_list() ->
    {ok, Data} = file:read_file("words.txt"),
    lists:map(fun(W) -> binary_to_list(W) end, binary:split(Data, [<<"\n">>], [global])).
