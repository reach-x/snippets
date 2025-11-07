-module(utils).
-export([
    demo/0,
    % List utilities
    range/2, take/2, drop/2, chunk/2,
    flatten/1, unique/1, frequencies/1,
    rotate/2, interleave/2,
    % String utilities
    split/2, join/2, trim/1,
    % Math utilities
    sum/1, product/1, average/1, square/1, cube/1,
    % Function utilities
    compose/2, partial/2, curry/1
]).

%% Utility functions library for Erlang

demo() ->
    io:format("~n=== Utils Library Demo ===~n~n"),

    %% List utilities
    io:format("Range 1-10: ~p~n", [range(1, 10)]),
    io:format("Take 3: ~p~n", [take(3, [1, 2, 3, 4, 5])]),
    io:format("Drop 2: ~p~n", [drop(2, [1, 2, 3, 4, 5])]),
    io:format("Chunk 3: ~p~n", [chunk(3, range(1, 11))]),
    io:format("Flatten: ~p~n", [flatten([[1, 2], [3, 4], [5]])]),
    io:format("Unique: ~p~n", [unique([1, 2, 2, 3, 3, 3, 4])]),
    io:format("Frequencies: ~p~n", [frequencies([a, b, a, c, b, a])]),
    io:format("Rotate 2: ~p~n", [rotate(2, [1, 2, 3, 4, 5])]),
    io:format("Interleave: ~p~n", [interleave([1, 2, 3], [a, b, c])]),

    %% String utilities
    io:format("~nSplit: ~p~n", [split("hello,world,erlang", ",")]),
    io:format("Join: ~p~n", [join(["hello", "world"], " ")]),
    io:format("Trim: ~p~n", [trim("  hello  ")]),

    %% Math utilities
    io:format("~nSum: ~p~n", [sum([1, 2, 3, 4, 5])]),
    io:format("Product: ~p~n", [product([1, 2, 3, 4, 5])]),
    io:format("Average: ~p~n", [average([1, 2, 3, 4, 5])]),
    io:format("Square 5: ~p~n", [square(5)]),
    io:format("Cube 3: ~p~n", [cube(3)]),

    %% Function utilities
    io:format("~nCompose (add1 . double):~n"),
    Add1 = fun(X) -> X + 1 end,
    Double = fun(X) -> X * 2 end,
    Composed = compose(Double, Add1),
    io:format("  Result for 5: ~p~n", [Composed(5)]),

    ok.

%% List utilities

range(Start, End) when Start >= End ->
    [];
range(Start, End) ->
    [Start | range(Start + 1, End)].

take(0, _) ->
    [];
take(_, []) ->
    [];
take(N, [H | T]) when N > 0 ->
    [H | take(N - 1, T)].

drop(0, List) ->
    List;
drop(_, []) ->
    [];
drop(N, [_ | T]) when N > 0 ->
    drop(N - 1, T).

chunk(_, []) ->
    [];
chunk(N, List) ->
    {Chunk, Rest} = split_at(N, List),
    [Chunk | chunk(N, Rest)].

split_at(N, List) ->
    {take(N, List), drop(N, List)}.

flatten([]) ->
    [];
flatten([H | T]) when is_list(H) ->
    flatten(H) ++ flatten(T);
flatten([H | T]) ->
    [H | flatten(T)].

unique(List) ->
    unique(List, []).

unique([], Acc) ->
    lists:reverse(Acc);
unique([H | T], Acc) ->
    case lists:member(H, Acc) of
        true -> unique(T, Acc);
        false -> unique(T, [H | Acc])
    end.

frequencies(List) ->
    Dict = lists:foldl(
        fun(Item, Acc) ->
            dict:update_counter(Item, 1, Acc)
        end,
        dict:new(),
        List),
    dict:to_list(Dict).

rotate(N, List) ->
    Len = length(List),
    case Len of
        0 -> List;
        _ ->
            N1 = N rem Len,
            drop(N1, List) ++ take(N1, List)
    end.

interleave([], List2) ->
    List2;
interleave(List1, []) ->
    List1;
interleave([H1 | T1], [H2 | T2]) ->
    [H1, H2 | interleave(T1, T2)].

%% String utilities

split(String, Delimiter) ->
    string:split(String, Delimiter, all).

join([], _) ->
    "";
join([H], _) ->
    H;
join([H | T], Sep) ->
    lists:flatten([H, Sep, join(T, Sep)]).

trim(String) ->
    string:trim(String).

%% Math utilities

sum(List) ->
    lists:foldl(fun(X, Acc) -> X + Acc end, 0, List).

product(List) ->
    lists:foldl(fun(X, Acc) -> X * Acc end, 1, List).

average(List) ->
    sum(List) / length(List).

square(X) ->
    X * X.

cube(X) ->
    X * X * X.

%% Function utilities

compose(F, G) ->
    fun(X) -> F(G(X)) end.

partial(Fun, Arg1) ->
    fun(Arg2) -> Fun(Arg1, Arg2) end.

curry(Fun) ->
    fun(X) ->
        fun(Y) ->
            Fun(X, Y)
        end
    end.

%% Additional utilities

zip([], _) ->
    [];
zip(_, []) ->
    [];
zip([H1 | T1], [H2 | T2]) ->
    [{H1, H2} | zip(T1, T2)].

filter_map(Fun, List) ->
    lists:filtermap(Fun, List).

partition(Pred, List) ->
    lists:partition(Pred, List).

group_by(KeyFun, List) ->
    Dict = lists:foldl(
        fun(Item, Acc) ->
            Key = KeyFun(Item),
            dict:append(Key, Item, Acc)
        end,
        dict:new(),
        List),
    dict:to_list(Dict).

take_while(_, []) ->
    [];
take_while(Pred, [H | T]) ->
    case Pred(H) of
        true -> [H | take_while(Pred, T)];
        false -> []
    end.

drop_while(_, []) ->
    [];
drop_while(Pred, [H | T] = List) ->
    case Pred(H) of
        true -> drop_while(Pred, T);
        false -> List
    end.

%% Map with index
map_indexed(Fun, List) ->
    map_indexed(Fun, List, 0).

map_indexed(_, [], _) ->
    [];
map_indexed(Fun, [H | T], Index) ->
    [Fun(H, Index) | map_indexed(Fun, T, Index + 1)].

%% Cartesian product
cartesian_product(List1, List2) ->
    [{X, Y} || X <- List1, Y <- List2].
