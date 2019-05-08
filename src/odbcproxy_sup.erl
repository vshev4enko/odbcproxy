-module(odbcproxy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, add_pool/3]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
    {ok, Pools} = application:get_env(odbcproxy, pools),
    PoolSpec = lists:map(
        fun({PoolName, SizeArgs, WorkerArgs}) ->
            PoolArgs = [{name, {local, PoolName}}, {worker_module, odbcproxy_worker}] ++ SizeArgs,
            poolboy:child_spec(PoolName, PoolArgs, WorkerArgs)
        end, Pools),
    {ok, {{one_for_one, 10, 10}, PoolSpec}}.

add_pool(Name, PoolArgs, WorkerArgs) ->
    ChildSpec = poolboy:child_spec(Name, PoolArgs, WorkerArgs),
    supervisor:start_child(?MODULE, ChildSpec).
