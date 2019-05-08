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
         select_to_proplists/1]).


-define(DEFAULT_POOL, odbc_pool).

-type settings() :: {list(), list()}.
-type pool_name() :: atom().
-type select() :: {atom(), list(), list()}.


-spec connect(settings()) -> supervisor:startchild_ret().
connect(Settings) ->
    connect(?DEFAULT_POOL, Settings).

-spec connect(pool_name(), settings()) -> supervisor:startchild_ret().
connect(PoolName, {SizeArgs, WorkerArgs}) ->
    PoolArgs = [{name, {local, PoolName}}, {worker_module, odbcproxy_worker}] ++ SizeArgs,
    odbcproxy_sup:add_pool(PoolName, PoolArgs, WorkerArgs).

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

-spec select_to_proplists(select()) -> proplists:proplist().
select_to_proplists({selected, Columns, Rows}) ->
    Columns1 = lists:map(fun(X) when is_list(X) -> iolist_to_binary(X); (X) -> X end, Columns),
    [lists:zip(Columns1, X) || X <- Rows];
select_to_proplists(_) ->
    [].
