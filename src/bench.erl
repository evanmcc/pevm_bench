-module(bench).

-compile(export_all).

%%% stupid stuff

%% pid that adds numbers
sub_pid() ->
    Start = erlang:monotonic_time(),
    L = fun Loop(0) ->
                Time = erlang:monotonic_time() - Start,
                io:format("took ~p ms, ~~~p ns per iteration~n",
                          [erlang:convert_time_unit(Time,
                                                native, millisecond),
                           erlang:convert_time_unit(Time div 10000000,
                                                    native, nanosecond)]);
            Loop(N) ->
                Loop(N - 1)
        end,
    L(10000000).

sub_after() ->
    Start = erlang:monotonic_time(),
    L = fun Loop(0) ->
                Time = erlang:monotonic_time() - Start,
                io:format("took ~p ms, ~~~p ns per iteration~n",
                          [erlang:convert_time_unit(Time,
                                                native, millisecond),
                           erlang:convert_time_unit(Time div 10000000,
                                                    native, nanosecond)]);
            Loop(N) ->
                receive
                    E -> E
                after 0 ->
                        ok
                end,
                Loop(N - 1)
        end,
    L(10000000).

sub_fun() ->
    Start = erlang:monotonic_time(),
    L = fun Loop(0) ->
                Time = erlang:monotonic_time() - Start,
                io:format("took ~p ms, ~~~p ns per iteration~n",
                          [erlang:convert_time_unit(Time,
                                                native, millisecond),
                           erlang:convert_time_unit(Time div 10000000,
                                                    native, nanosecond)]);
            Loop(N) ->
                N1 = sub(N),
                Loop(N1)
        end,
    L(10000000).

sub(N) ->
    N - 1.

%% pid that grabs a small thing from ets
loop_ets_small() ->
    Tab = ets:new(blort, []),
    ets:insert(Tab, {foo, bar}),
    Start = erlang:monotonic_time(),
    L = fun Loop(0) ->
                Time = erlang:monotonic_time() - Start,
                io:format("took ~p ms, ~~~p ns per iteration~n",
                          [erlang:convert_time_unit(Time,
                                                native, millisecond),
                           erlang:convert_time_unit(Time div 10000000,
                                                    native, nanosecond)]);
            Loop(N) ->
                [_] = ets:lookup(Tab, foo),
                Loop(N - 1)
        end,
    L(10000000),
    ets:delete(Tab).

%% pid that grabs a large thing from ets
loop_ets_large() ->
    Tab = ets:new(blort, []),
    ets:insert(Tab, {foo, iolist_to_binary(lists:duplicate(10000, <<"asdasdasdasd">>))}),
    Start = erlang:monotonic_time(),
    L = fun Loop(0) ->
                Time = erlang:monotonic_time() - Start,
                io:format("took ~p ms, ~~~p ns per iteration~n",
                          [erlang:convert_time_unit(Time,
                                                native, millisecond),
                           erlang:convert_time_unit(Time div 10000000,
                                                    native, nanosecond)]);
            Loop(N) ->
                [_] = ets:lookup(Tab, foo),
                Loop(N - 1)
        end,
    L(10000000),
    ets:delete(Tab).

%% pid that gets something from a small map
loop_map_small() ->
    loop_map_n(2).

%% pid that gets something from a large map
loop_map_large() ->
    loop_map_n(130).

loop_map_really_large() ->
    loop_map_n(13000).

loop_map_n(Size) ->
    Map = maps:from_list([{I, iolist_to_binary(lists:duplicate(10000, <<"asdasdasdasd">>))}
                          || I <- lists:seq(1, Size)]),
    Start = erlang:monotonic_time(),
    L = fun Loop(0) ->
                Time = erlang:monotonic_time() - Start,
                io:format("took ~p ms, ~~~p ns per iteration~n",
                          [erlang:convert_time_unit(Time,
                                                native, millisecond),
                           erlang:convert_time_unit(Time div 10000000,
                                                    native, nanosecond)]);
            Loop(N) ->
                {ok, _} = maps:find(Size, Map),
                Loop(N - 1)
        end,
    L(10000000).


%% gen_server that subs numbers -- timeout
server_timeout() ->
    {ok, _Sink} = sink:start_link(),

    sink:go(timeout),
    receive
        {complete, Time} ->
            io:format("took ~p ms, ~~~p ns per iteration~n",
                      [erlang:convert_time_unit(Time,
                                                native, millisecond),
                       erlang:convert_time_unit(Time div 10000000,
                                                native, nanosecond)
                      ])
    end,
    sink:stop().

%% gen_server that subs numbers -- calls
server_call_lookup() ->
    {ok, _Sink} = sink:start_link(),
    sink:go(call),
    spawn(fun Loop() ->
                  case sink:call_dec() of
                      ok ->
                          Loop();
                      done ->
                          ok
                  end
          end),
    receive
        {complete, Time} ->
            io:format("took ~p ms, ~~~p ns per iteration~n",
                      [erlang:convert_time_unit(Time,
                                                native, millisecond),
                       erlang:convert_time_unit(Time div 10000000,
                                                native, nanosecond)
                      ])
    end,
    sink:stop().

server_call_pid() ->
    {ok, Sink} = sink:start_link(),
    sink:go(call),
    spawn(fun Loop() ->
                  case sink:call_dec(Sink) of
                      ok ->
                          Loop();
                      done ->
                          ok
                  end
          end),
    receive
        {complete, Time} ->
            io:format("took ~p ms, ~~~p ns per iteration~n",
                      [erlang:convert_time_unit(Time,
                                                native, millisecond),
                       erlang:convert_time_unit(Time div 10000000,
                                                native, nanosecond)
                      ])
    end,
    sink:stop().

%% gen_server that subs numbers -- cast
server_cast_lookup() ->
    {ok, _Sink} = sink:start_link(),
    sink:go(call),
    P = spawn(fun Loop() ->
                      sink:cast_dec(),
                      receive
                          stop ->
                              ok
                      after 0 ->
                              Loop()
                      end
              end),
    receive
        {complete, Time} ->
            P ! stop,
            io:format("took ~p ms, ~~~p ns per iteration~n",
                      [erlang:convert_time_unit(Time,
                                                native, millisecond),
                       erlang:convert_time_unit(Time div 10000000,
                                                native, nanosecond)
                      ])
    end,
    sink:stop().

%% gen_server that subs numbers -- message
server_msg() ->
    {ok, Sink} = sink:start_link(),
    sink:go(call),
    P = spawn(fun () ->
                      F = fun Loop(0) ->
                                  io:format("msgr done~n"),
                                  ok;
                              Loop(N) ->
                                  Wait = case N rem 20000 of
                                             1 ->
                                                 1;
                                             _ ->
                                                 0
                                         end,
                                  Sink ! dec,
                                  receive
                                      stop ->
                                          ok
                                  after Wait ->
                                          Loop(N - 1)
                                  end
                          end,
                      F(10000001)
              end),
    receive
        {complete, Time} ->
            P ! stop,
            io:format("took ~p ms, ~~~p ns per iteration~n",
                      [erlang:convert_time_unit(Time,
                                                native, millisecond),
                       erlang:convert_time_unit(Time div 10000000,
                                                native, nanosecond)
                      ])
    end,
    sink:stop().

server_pingpong() ->
    {ok, Sink} = sink:start_link(),
    sink:go(call),
    P = spawn(fun Loop() ->
                      Sink ! {ping, self()},
                      receive
                          pong ->
                              Loop();
                          done ->
                              ok;
                          stop ->
                              ok
                      end
              end),
    receive
        {complete, Time} ->
            P ! stop,
            io:format("took ~p ms, ~~~p ns per iteration~n",
                      [erlang:convert_time_unit(Time,
                                                native, millisecond),
                       erlang:convert_time_unit(Time div 10000000,
                                                native, nanosecond)
                      ])
    end,
    sink:stop().


%%% less stupid stuff

%% N pids `call`ing a gen_server

%% N pids `cast`ing a gen_server
server_n_msg(N) ->
    {ok, Sink} = sink:start_link(),
    Scheds = erlang:system_info(schedulers),
    sink:go(call),
    Pids = [spawn(fun () ->
                          erlang:process_flag(scheduler,
                                              (N rem (Scheds - 1)) + 1),
                          F = fun Loop(I) ->
                                      Rem = I rem 1000,
                                      Sink ! {ping, self(), Rem},
                                      Wait = case Rem of
                                                 1 ->
                                                     100000;
                                                 _ ->
                                                     0
                                             end,
                                      receive
                                          stop ->
                                              ok;
                                          pong ->
                                          Loop(I - 1)
                                      after Wait ->
                                              Loop(I - 1)
                                      end
                              end,
                          F((10000000 div N) + 10)
                  end) || _ <- lists:seq(1, N)],
    receive
        {complete, Time} ->
            [P ! stop || P <- Pids],
            io:format("took ~p ms, ~~~p ns per iteration~n",
                      [Time div 1000000,
                       erlang:convert_time_unit(Time div 10000000,
                                                native, nanosecond)
                      ])
    end,
    sink:stop().

%% N pids sending raw messages to a gen_server
server_n_call(N) ->
    {ok, _Sink} = sink:start_link(),
    Scheds = erlang:system_info(schedulers),
    sink:go(call),
    Pids = [spawn(fun () ->
                          erlang:process_flag(scheduler,
                                              (N rem (Scheds - 1)) + 1),
                          F = fun Loop(I) ->
                                      case sink:call_dec() of
                                          ok ->
                                              Loop(I - 1);
                                          done ->
                                              ok
                                      end
                              end,
                          F((10000000 div N) + 10)
                  end) || _ <- lists:seq(1, N)],
    receive
        {complete, Time} ->
            [P ! stop || P <- Pids],
            io:format("took ~p ms, ~~~p ns per iteration~n",
                      [Time div 1000000,
                       erlang:convert_time_unit(Time div 10000000,
                                                native, nanosecond)
                      ])
    end,
    sink:stop().
