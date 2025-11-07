-module(list_operations).
-export([demo/0]).

%% List operations in Erlang
%% Demonstrating functional programming with lists

demo() ->
    io:format("~n=== List Operations in Erlang ===~n~n"),

    Numbers = [1, 2, 3, 4, 5],
    Fruits = ["apple", "banana", "cherry"],

    io:format("Numbers: ~p~n", [Numbers]),
    io:format("Fruits: ~p~n", [Fruits]),

    %% List length
    io:format("~nLength: ~p~n", [length(Numbers)]),

    %% Head (first element)
    [Head | _] = Numbers,
    io:format("First element (head): ~p~n", [Head]),

    %% Tail (rest of list)
    [_ | Tail] = Numbers,
    io:format("Rest of list (tail): ~p~n", [Tail]),

    %% Prepend element
    io:format("~nPrepend 0: ~p~n", [[0 | Numbers]]),

    %% Append lists
    io:format("Append [6,7,8]: ~p~n", [Numbers ++ [6, 7, 8]]),

    %% Reverse list
    io:format("Reversed: ~p~n", [lists:reverse(Numbers)]),

    %% Member check
    io:format("~nMember 3: ~p~n", [lists:member(3, Numbers)]),
    io:format("Member 10: ~p~n", [lists:member(10, Numbers)]),

    %% Map function over list
    io:format("~nMap (square): ~p~n", [lists:map(fun(X) -> X * X end, Numbers)]),

    %% Filter list
    io:format("Filter (even): ~p~n",
              [lists:filter(fun(X) -> X rem 2 =:= 0 end, Numbers)]),

    %% Fold (reduce) left
    io:format("~nFold left (sum): ~p~n",
              [lists:foldl(fun(X, Acc) -> X + Acc end, 0, Numbers)]),

    io:format("Fold left (product): ~p~n",
              [lists:foldl(fun(X, Acc) -> X * Acc end, 1, Numbers)]),

    %% Fold right
    io:format("Fold right (list): ~p~n",
              [lists:foldr(fun(X, Acc) -> [X | Acc] end, [], Numbers)]),

    %% Find element
    case lists:search(fun(X) -> X > 3 end, Numbers) of
        {value, Val} -> io:format("~nFirst > 3: ~p~n", [Val]);
        false -> io:format("~nNo element > 3~n")
    end,

    %% Any and all
    io:format("Any > 4: ~p~n", [lists:any(fun(X) -> X > 4 end, Numbers)]),
    io:format("All positive: ~p~n", [lists:all(fun(X) -> X > 0 end, Numbers)]),

    %% Sort list
    Unsorted = [5, 2, 8, 1, 9, 3],
    io:format("~nUnsorted: ~p~n", [Unsorted]),
    io:format("Sorted: ~p~n", [lists:sort(Unsorted)]),

    %% Nth element (1-indexed in Erlang)
    io:format("~nThird element (nth): ~p~n", [lists:nth(3, Numbers)]),

    %% Take first n elements
    io:format("First 3 (sublist): ~p~n", [lists:sublist(Numbers, 3)]),

    %% Drop first n elements
    io:format("Drop 2 (nthtail): ~p~n", [lists:nthtail(2, Numbers)]),

    %% Split list
    {Left, Right} = lists:split(3, Numbers),
    io:format("~nSplit at 3: {~p, ~p}~n", [Left, Right]),

    %% Remove duplicates
    WithDupes = [1, 2, 2, 3, 3, 3, 4],
    io:format("~nWith duplicates: ~p~n", [WithDupes]),
    io:format("Unique (usort): ~p~n", [lists:usort(WithDupes)]),

    %% Zip two lists
    Letters = [a, b, c, d, e],
    io:format("~nZip: ~p~n", [lists:zip(Numbers, Letters)]),

    %% Unzip
    Zipped = [{1, a}, {2, b}, {3, c}],
    {Nums, Lets} = lists:unzip(Zipped),
    io:format("Unzip: {~p, ~p}~n", [Nums, Lets]),

    %% Flatten nested lists
    Nested = [[1, 2], [3, 4], [5]],
    io:format("~nFlatten: ~p~n", [lists:flatten(Nested)]),

    %% Partition
    {Evens, Odds} = lists:partition(fun(X) -> X rem 2 =:= 0 end, Numbers),
    io:format("~nPartition even:~n"),
    io:format("  Evens: ~p~n", [Evens]),
    io:format("  Odds: ~p~n", [Odds]),

    %% List comprehension
    Squares = [X * X || X <- [1, 2, 3, 4, 5]],
    io:format("~nList comprehension (squares): ~p~n", [Squares]),

    EvenSquares = [X * X || X <- [1, 2, 3, 4, 5, 6, 7, 8, 9, 10], X rem 2 =:= 0],
    io:format("Even squares: ~p~n", [EvenSquares]),

    %% Pattern matching in list comprehension
    People = [{person, "Alice", 30}, {person, "Bob", 25}, {person, "Charlie", 35}],
    Names = [Name || {person, Name, _Age} <- People],
    io:format("~nNames from people: ~p~n", [Names]),

    %% Helper functions demo
    io:format("~n=== Helper Functions ===~n"),
    io:format("Sum: ~p~n", [sum(Numbers)]),
    io:format("Product: ~p~n", [product(Numbers)]),
    io:format("Maximum: ~p~n", [maximum(Numbers)]),
    io:format("Minimum: ~p~n", [minimum(Numbers)]),
    io:format("Range 1-10: ~p~n", [range(1, 10)]),
    io:format("Take 3: ~p~n", [take(3, Numbers)]),
    io:format("Drop 2: ~p~n", [drop(2, Numbers)]),

    ok.

%% Helper functions

sum(List) ->
    lists:foldl(fun(X, Acc) -> X + Acc end, 0, List).

product(List) ->
    lists:foldl(fun(X, Acc) -> X * Acc end, 1, List).

maximum([H | T]) ->
    lists:foldl(fun erlang:max/2, H, T).

minimum([H | T]) ->
    lists:foldl(fun erlang:min/2, H, T).

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
