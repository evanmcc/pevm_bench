-module(sink).

-behaviour(gen_server).

-compile(export_all).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state,
        {
          start,
          ready = false,
          mode,
          caller,
          count = 10000000
        }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

go(Mode) ->
    gen_server:call(?SERVER, {go, self(), Mode}, infinity).

stop() ->
    gen_server:call(?SERVER, stop, infinity).

call_dec() ->
    call_dec(?SERVER).

call_dec(Pid) ->
    gen_server:call(Pid, dec, infinity).

cast_dec() ->
    cast_dec(?SERVER).

cast_dec(Pid) ->
    gen_server:cast(Pid, dec).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    erlang:process_flag(scheduler, 0),
    {ok, #state{}}.

handle_call({go, Caller, Mode}, _From, State) ->
    Start = erlang:monotonic_time(),
    %% maybe change the heap status flag?
    case Mode of
        timeout ->
            {reply, ok, State#state{ready = true,
                                    start = Start,
                                    caller = Caller,
                                    mode = Mode}, 0};
        _ ->
            {reply, ok, State#state{ready = true,
                                    start = Start,
                                    caller = Caller,
                                    mode = Mode}}
    end;
handle_call(_Request, _From, #state{ready = false} = State) ->
    {reply, {error, not_ready}, State};
handle_call(dec, _From, State) when State#state.count =< 0 ->
    {reply, done, State};
handle_call(dec, _From, #state{caller = Caller,
                               start = Start,
                               count = Count} = State) ->
    Count1 = Count - 1,
    case Count1 of
        0 ->
            Caller ! {complete,
                      erlang:monotonic_time() - Start};
        _ ->
            ok
    end,
    {reply, ok, State#state{count = Count1}};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    lager:warning("unexpected call ~p from ~p", [_Request, _From]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(dec, #state{caller = Caller,
                        start = Start,
                        count = Count} = State) ->
    Count1 = Count - 1,
    case Count1 of
        0 ->
            Caller ! {complete,
                      erlang:monotonic_time() - Start};
        _ ->
            ok
    end,
    {noreply, State#state{count = Count1}};
handle_cast(_Msg, State) ->
    lager:warning("unexpected cast ~p", [_Msg]),
    {noreply, State}.

handle_info(dec, #state{caller = Caller,
                        start = Start,
                        count = Count} = State) ->
    Count1 = Count - 1,
    case Count1 of
        0 ->
            io:format("done~n"),
            Caller ! {complete,
                      erlang:monotonic_time() - Start};
        _ ->
            ok
    end,
    {noreply, State#state{count = Count1}};
handle_info({ping, Sender}, #state{caller = Caller,
                                   start = Start,
                                   count = Count} = State) ->
    Count1 = Count - 1,
    case Count1 of
        0 ->
            Sender ! done,
            Caller ! {complete,
                      erlang:monotonic_time() - Start};
        _ ->
            Sender ! pong,
            ok
    end,
    {noreply, State#state{count = Count1}};
handle_info({ping, Sender, Rem}, #state{caller = Caller,
                                        start = Start,
                                        count = Count} = State) ->
    Count1 = Count - 1,
    case Count1 of
        0 ->
            Sender ! done,
            Caller ! {complete,
                      erlang:monotonic_time() - Start};
        _ ->
            case Rem of
                1 ->
                    Sender ! pong;
                _ ->
                    ok
            end,
            ok
    end,
    {noreply, State#state{count = Count1}};
handle_info(timeout, #state{caller = Caller,
                            start = Start,
                            count = 0 } = State) ->
    Caller ! { complete,
               erlang:monotonic_time() - Start},
    {noreply, State};
handle_info(timeout, #state{count = Count} = State) ->
    {noreply, State#state{count = Count - 1}, 0};
handle_info(_Info, State) ->
    lager:warning("unexpected message ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
