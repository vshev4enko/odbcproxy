%%%-------------------------------------------------------------------
%%% @author irishman
%%% @copyright (C) 2019
%%% @doc
%%%
%%% @end
%%% Created : 07. May 2019 18:35
%%%-------------------------------------------------------------------
-module(odbcproxy).
-author("irishman").

%% API
-export([connect/1,
         connect/2,
         squery/1,
         squery/2,
         squery/3,
         equery/2,
         equery/3,
         equery/4,
         select_to_proplists/1,
         make_conn_str/1,
         open_conn/1,
         open_conn/2,
         close_conn/1]).


-define(DEFAULT_POOL, odbc_pool).

-type settings() :: {list(), list()}.
-type pool_name() :: atom().
-type select() :: {atom(), list(), list()}.


-spec connect(settings()) -> supervisor:startchild_ret().
connect(Settings) ->
    connect(?DEFAULT_POOL, Settings).

-spec connect(pool_name(), settings()) -> supervisor:startchild_ret().
connect(PoolName, {SizeArgs, WorkerArgs}) ->
    odbcproxy_sup:add_pool(PoolName, SizeArgs, WorkerArgs).

-spec squery(list()) -> any().
squery(Sql) ->
    squery(?DEFAULT_POOL, Sql).

-spec squery(pool_name(), list()) -> any().
squery(PoolName, Sql) ->
    poolboy:transaction(PoolName, fun(Worker) -> gen_server:call(Worker, {squery, Sql}, infinity) end).

-spec squery(pool_name(), list(), integer()) -> any().
squery(PoolName, Sql, Timeout) ->
    poolboy:transaction(PoolName, fun(Worker) -> gen_server:call(Worker, {squery, Sql, Timeout}, infinity) end).

-spec equery(list(), list()) -> any().
equery(Stmp, Params) ->
    equery(?DEFAULT_POOL, Stmp, Params).

-spec equery(pool_name(), list(), list()) -> any().
equery(PoolName, Stmt, Params) ->
    poolboy:transaction(PoolName, fun(Worker) -> gen_server:call(Worker, {equery, Stmt, Params}, infinity) end).

-spec equery(pool_name(), list(), list(), integer()) -> any().
equery(PoolName, Stmt, Params, Timeout) ->
    poolboy:transaction(PoolName, fun(Worker) -> gen_server:call(Worker, {equery, Stmt, Params, Timeout}, infinity) end).

-spec open_conn(atom()) -> {ok, pid()} | {error, term()}.
open_conn(PoolName) ->
    open_conn(PoolName, []).

open_conn(PoolName, OverrideConfig) ->
    {ok, Pools} = application:get_env(odbcproxy, pools),
    Config = lists:foldl(fun({Name, _, Cfg}, _) when Name =:= PoolName -> Cfg; (_, Acc) -> Acc end, [], Pools),
    Overrided = lists:foldl(
        fun({K,V}, Acc) ->
            case proplists:get_value(K, OverrideConfig) of
                undefined -> [{K,V}|Acc];
                OverrideV -> [{K,OverrideV}|Acc]
            end
        end, [], Config),
    {ok, Str} = make_conn_str(Overrided),
    odbc:connect(Str, [{binary_strings, on}, {tuple_row, off}]).

-spec close_conn(pid()) -> ok | {error, term()}.
close_conn(Pid) ->
    odbc:disconnect(Pid).

-spec select_to_proplists(select()) -> proplists:proplist().
select_to_proplists({selected, Columns, Rows}) ->
    [lists:zip(Columns, tuple_to_list(X)) || X <- Rows];
select_to_proplists(_) ->
    [].

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
        end end, [], proplists:get_keys(Args)),

    {ok, string:join(ConnStr, ";")}.
