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
    {ok, Checked} = check_config(Pools),
    PoolSpec = lists:map(
        fun({PoolName, SizeArgs, WorkerArgs}) ->
            PoolArgs = [{name, {local, PoolName}}, {worker_module, odbcproxy_worker}] ++ SizeArgs,
            poolboy:child_spec(PoolName, PoolArgs, WorkerArgs)
        end, Checked),
    {ok, {{one_for_one, 10, 10}, PoolSpec}}.

add_pool(Name, SizeArgs, WorkerArgs) ->
    case check_config([{Name, SizeArgs, WorkerArgs}]) of
        {ok, []} -> {error, bad_config};
        {ok, [{_, _, _}]} ->
            PoolArgs = [{name, {local, Name}}, {worker_module, odbcproxy_worker}] ++ SizeArgs,
            ChildSpec = poolboy:child_spec(Name, PoolArgs, WorkerArgs),
            supervisor:start_child(?MODULE, ChildSpec)
    end.

check_config(Params) ->
    Result = lists:foldr(
        fun({_, _, WorkerArgs} = Cfg, Acc) ->
            case proplists:get_value(dsn, WorkerArgs) of
                undefined ->
                    error_logger:error_msg("Bad config ~p", [Cfg]),
                    Acc;
                Dsn when is_list(Dsn) ->
                    [Cfg | Acc]
            end

        end, [], Params),

    {ok, Result}.
