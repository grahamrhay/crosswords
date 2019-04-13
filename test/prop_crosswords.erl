-module(prop_crosswords).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

prop_true() ->
    ?FORALL(_, term(),
        begin
            true
        end).
