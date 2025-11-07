% List operations in Prolog

% List predicates
demo :-
    writeln('\n=== List Operations in Prolog ===\n'),

    % Create list
    Numbers = [1, 2, 3, 4, 5],
    format('Numbers: ~w~n', [Numbers]),

    % List length
    length(Numbers, Len),
    format('Length: ~w~n', [Len]),

    % Head and tail
    [H|T] = Numbers,
    format('\nHead: ~w~n', [H]),
    format('Tail: ~w~n', [T]),

    % Append
    append(Numbers, [6, 7, 8], Appended),
    format('\nAppend: ~w~n', [Appended]),

    % Member
    (member(3, Numbers) -> writeln('Member 3: true') ; writeln('Member 3: false')),
    (member(10, Numbers) -> writeln('Member 10: true') ; writeln('Member 10: false')),

    % Reverse
    reverse(Numbers, Reversed),
    format('\nReversed: ~w~n', [Reversed]),

    % Sort
    Unsorted = [5, 2, 8, 1, 9, 3],
    sort(Unsorted, Sorted),
    format('\nSorted: ~w~n', [Sorted]),

    % Sum (using fold)
    sumlist(Numbers, Sum),
    format('Sum: ~w~n', [Sum]),

    % Max element
    max_list(Numbers, Max),
    format('Max: ~w~n', [Max]),

    % Min element
    min_list(Numbers, Min),
    format('Min: ~w~n', [Min]),

    % Take first n elements
    length(Prefix, 3),
    append(Prefix, _, Numbers),
    format('\nFirst 3: ~w~n', [Prefix]),

    % Map (using maplist)
    maplist(square, Numbers, Squared),
    format('\nSquared: ~w~n', [Squared]),

    % Filter even numbers
    include(even_number, Numbers, Evens),
    format('Evens: ~w~n', [Evens]),

    halt.

% Helper predicates
square(X, Y) :- Y is X * X.

even_number(X) :- 0 is X mod 2.

% Run demo
:- initialization(demo).
