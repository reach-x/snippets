-module(concurrency).
-export([demo/0, counter/1, echo/0, ping/2, pong/0]).

%% Concurrency in Erlang
%% Demonstrating processes, message passing, and concurrent programming

demo() ->
    io:format("~n=== Concurrency in Erlang ===~n~n"),

    %% Simple process spawning
    io:format("=== Simple Process ===~n"),
    Pid = spawn(fun() ->
                        io:format("Hello from process ~p~n", [self()])
                end),
    io:format("Spawned process: ~p~n", [Pid]),

    timer:sleep(100), % Wait for process to finish

    %% Process with message passing
    io:format("~n=== Message Passing ===~n"),
    EchoPid = spawn(?MODULE, echo, []),
    EchoPid ! {self(), "Hello"},
    receive
        {EchoPid, Msg} ->
            io:format("Received reply: ~p~n", [Msg])
    after 1000 ->
            io:format("Timeout waiting for reply~n")
    end,
    EchoPid ! stop,

    timer:sleep(100),

    %% Counter process with state
    io:format("~n=== Stateful Process (Counter) ===~n"),
    CounterPid = spawn(?MODULE, counter, [0]),
    CounterPid ! {self(), increment},
    receive
        {counter, Count1} ->
            io:format("Counter: ~p~n", [Count1])
    end,

    CounterPid ! {self(), increment},
    receive
        {counter, Count2} ->
            io:format("Counter: ~p~n", [Count2])
    end,

    CounterPid ! {self(), get},
    receive
        {counter, Count3} ->
            io:format("Final counter: ~p~n", [Count3])
    end,

    CounterPid ! stop,

    %% Ping-pong between processes
    io:format("~n=== Ping Pong ===~n"),
    PongPid = spawn(?MODULE, pong, []),
    PingPid = spawn(?MODULE, ping, [PongPid, 3]),

    timer:sleep(500), % Wait for ping-pong to complete

    %% Parallel map
    io:format("~n=== Parallel Map ===~n"),
    Numbers = [1, 2, 3, 4, 5],
    Results = parallel_map(fun(X) -> X * X end, Numbers),
    io:format("Parallel map (square): ~p~n", [Results]),

    %% Process links and monitors
    io:format("~n=== Process Monitoring ===~n"),
    WorkerPid = spawn_link(fun() ->
                                    io:format("Worker started~n"),
                                    timer:sleep(200),
                                    io:format("Worker finished~n")
                            end),
    io:format("Linked to worker: ~p~n", [WorkerPid]),

    MonitoredPid = spawn(fun() ->
                                 timer:sleep(100)
                         end),
    MonitorRef = monitor(process, MonitoredPid),
    io:format("Monitoring process: ~p with ref ~p~n", [MonitoredPid, MonitorRef]),

    receive
        {'DOWN', MonitorRef, process, MonitoredPid, Reason} ->
            io:format("Monitored process died: ~p~n", [Reason])
    after 500 ->
            io:format("Timeout~n")
    end,

    %% Selective receive
    io:format("~n=== Selective Receive ===~n"),
    self() ! {priority, "Important message"},
    self() ! {normal, "Normal message"},
    self() ! {priority, "Another important"},

    selective_receive(3),

    io:format("~nConcurrency demo complete~n"),
    ok.

%% Echo process - receives messages and sends them back
echo() ->
    receive
        {From, Message} ->
            io:format("Echo received: ~p~n", [Message]),
            From ! {self(), Message},
            echo();
        stop ->
            io:format("Echo stopping~n"),
            ok
    end.

%% Counter process - maintains state
counter(Count) ->
    receive
        {From, increment} ->
            NewCount = Count + 1,
            From ! {counter, NewCount},
            counter(NewCount);
        {From, decrement} ->
            NewCount = Count - 1,
            From ! {counter, NewCount},
            counter(NewCount);
        {From, get} ->
            From ! {counter, Count},
            counter(Count);
        stop ->
            io:format("Counter stopping at ~p~n", [Count]),
            ok
    end.

%% Ping process
ping(PongPid, 0) ->
    io:format("Ping: finished~n"),
    PongPid ! stop;
ping(PongPid, N) ->
    PongPid ! {self(), ping},
    receive
        pong ->
            io:format("Ping: received pong~n"),
            timer:sleep(100),
            ping(PongPid, N - 1)
    end.

%% Pong process
pong() ->
    receive
        {From, ping} ->
            io:format("Pong: received ping~n"),
            From ! pong,
            pong();
        stop ->
            io:format("Pong: stopping~n"),
            ok
    end.

%% Parallel map - apply function to list in parallel
parallel_map(Fun, List) ->
    Parent = self(),
    Pids = [spawn(fun() ->
                          Result = Fun(Item),
                          Parent ! {self(), Result}
                  end) || Item <- List],

    [receive
         {Pid, Result} -> Result
     end || Pid <- Pids].

%% Selective receive - prioritize certain messages
selective_receive(0) ->
    ok;
selective_receive(N) ->
    receive
        {priority, Msg} ->
            io:format("Priority: ~p~n", [Msg]),
            selective_receive(N - 1)
    after 0 ->
            receive
                {normal, Msg} ->
                    io:format("Normal: ~p~n", [Msg]),
                    selective_receive(N - 1)
            after 0 ->
                    selective_receive(N - 1)
            end
    end.

%% Process pool example
process_pool(PoolSize, WorkFun, WorkList) ->
    Parent = self(),

    %% Create worker processes
    Workers = [spawn(fun() -> worker(Parent, WorkFun) end)
               || _ <- lists:seq(1, PoolSize)],

    %% Distribute work
    [Worker ! {work, Work} || {Worker, Work} <- lists:zip(Workers, WorkList)],

    %% Collect results
    Results = [receive
                   {result, Result} -> Result
               end || _ <- WorkList],

    %% Stop workers
    [Worker ! stop || Worker <- Workers],

    Results.

worker(Parent, Fun) ->
    receive
        {work, Work} ->
            Result = Fun(Work),
            Parent ! {result, Result},
            worker(Parent, Fun);
        stop ->
            ok
    end.
