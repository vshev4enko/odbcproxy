%%%-------------------------------------------------------------------
%%% @author irishman
%%% @copyright (C) 2019
%%% @doc
%%%
%%% @end
%%% Created : 08. May 2019 13:18
%%%-------------------------------------------------------------------
-module(odbcproxy_worker).
-author("irishman").

-behaviour(gen_server).
-behaviour(poolboy_worker).


%% API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/1]).

-record(state, {conn :: pid(),
                delay :: pos_integer(),
                timer :: timer:tref(),
                start_args :: proplists:proplist(),
                query_timeout :: integer()}).


-define(CONNECTION_ARGS, [dsn, server, port, sslmode, database, uid, pwd]).
-define(INITIAL_DELAY, 500).
-define(MAXIMUM_DELAY, 5 * 60 * 1000).
-define(DEFAULT_TIMEOUT, 5 * 1000).


start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    process_flag(trap_exit, true),
    QueryTimeout = case proplists:get_value(peer_query_timeout_seconds, Args) of
                       undefined -> ?DEFAULT_TIMEOUT;
                       Timeout   -> (Timeout*1000)
                   end,
    {ok, connect(#state{start_args = Args, delay = ?INITIAL_DELAY, query_timeout = QueryTimeout})}.

handle_call(_Query, _From, #state{conn = undefined} = State) ->
    {reply, {error, disconnected}, State};

handle_call({squery, Sql}, _From, #state{conn = Conn, query_timeout = Timeout} = State) ->
    Resp = with_exception(odbc, sql_query, [Conn, Sql, Timeout]),
    {reply, Resp, State};
handle_call({squery, Sql, Timeout}, _From, #state{conn = Conn} = State) ->
    Resp = with_exception(odbc, sql_query, [Conn, Sql, Timeout]),
    {reply, Resp, State};
handle_call({equery, Sql, Params, Timeout}, _From, #state{conn = Conn} = State) ->
    Resp = with_exception(odbc, param_query, [Conn, Sql, Params, Timeout]),
    {reply, Resp, State};
handle_call({equery, Sql, Params}, _From, #state{conn = Conn, query_timeout = Timeout} = State) ->
    Resp = with_exception(odbc, param_query, [Conn, Sql, Params, Timeout]),
    {reply, Resp, State}.

handle_cast(Request, State) ->
    error_logger:info_msg("Cast request ~p", [Request]),
    {noreply, State}.

handle_info({'EXIT', Pid, Reason}, State) ->
    {NewDelay, NewTref} =
        case State#state.timer of
            undefined ->
                Delay = calculate_delay(State#state.delay),
                {ok, Tref} = timer:send_after(Delay, self(), reconnect),
                {Delay, Tref};
            Timer ->
                {State#state.delay, Timer}
        end,
    error_logger:error_report("~p exit with reason ~p, attempt to reconnect after ~p", [Pid, Reason, NewDelay]),
    {noreply, State#state{conn = undefined, delay = NewDelay, timer = NewTref}};
handle_info(reconnect, State) ->
    {noreply, connect(State)};
handle_info(Msg, State) ->
    error_logger:info_msg("Info msg ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, #state{conn = undefined}) ->
    ok;
terminate(_Reason, #state{conn = Conn}) ->
    ok = odbc:disconnect(Conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec connect(#state{}) -> #state{}.
connect(State) ->
    Args = State#state.start_args,
    Server = proplists:get_value(server, Args),
    {ok, ConnStr} = make_conn_str(Args),

    case odbc:connect(ConnStr, [{binary_strings, on}, {tuple_row, off}]) of
        {ok, Conn} ->
            true = link(Conn),
            error_logger:info_msg("~n~p Connected to ~s", [self(), Server]),
            timer:cancel(State#state.timer),
            State#state{conn = Conn, delay = ?INITIAL_DELAY, timer = undefined};
        {error, Reason} ->
            NewDelay = calculate_delay(State#state.delay),
            error_logger:error_report("~n~p Unable to connect, reason ~p", [self(), Reason]),
            {ok, Tref} = timer:send_after(State#state.delay, self(), reconnect),
            State#state{conn=undefined, delay = NewDelay, timer = Tref}
    end.

-spec calculate_delay(integer()) -> integer().
calculate_delay(Delay) when (Delay * 2) >= ?MAXIMUM_DELAY ->
    ?MAXIMUM_DELAY;
calculate_delay(Delay) ->
    Delay * 2.

-spec make_conn_str(proplists:proplist()) -> {ok, string()}.
make_conn_str(Args) ->
    ConnStr = lists:foldl(fun(Key, Acc) ->
        case proplists:get_value(Key, Args) of
            Value when is_list(Value) ->
                Acc ++ [atom_to_list(Key) ++ "=" ++ Value];
            Value when is_integer(Value) ->
                Acc ++ [atom_to_list(Key) ++ "=" ++ integer_to_list(Value)];
            _ ->
                Acc
        end end, [], ?CONNECTION_ARGS),

    {ok, string:join(ConnStr, ";")}.

-spec with_exception(atom(), atom(), list()) -> term().
with_exception(Mod, Fun, Args) ->
    try
        erlang:apply(Mod, Fun, Args)
    catch
        E:R ->
            StackTrace = erlang:get_stacktrace(),
            error_logger:error_report("Error ~p due perform request, reason ~p, stacktrace ~p", [E, R, StackTrace]),
            {E, R}
    end.
