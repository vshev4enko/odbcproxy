# odbcproxy

ODBC connection pool

### Build
```bash
rebar3 get-deps
rebar3 compile
```

## Config Example

Minimal .odbc.ini should be in PATH
```
[ClickHouse]
Driver = /path/to/libclickhouseodbc.so
```

App config
```
[
    {odbcproxy, [
        {pools, [
            {test, [
                {size, 10},
                {max_overflow, 20}
            ], [
                {dsn, "Test"},
                {server, "test_host"},
                {port, 1234},
                {sslmode, "require"},
                {database, "test_db"},
                {uid, "test_user"},
                {pwd, "test_pwd"},
                {peer_query_timeout_seconds, 5}
            ]}
        ]}
    ]}
].
```

## Usage Example
### Start App
```
1> application:ensure_all_started(odbcproxy).
=PROGRESS REPORT==== 8-May-2019::20:10:46 ===
         application: odbcproxy
          started_at: node@host
{ok,[odbc,poolboy,odbcproxy]}
````
### Simple query
```
2> odbcproxy:squery(clickhouse, "select * from table limit 1").
{selected,["column_names"],
          [{"data"}]}
```
### Make pool in runtime
```
odbcproxy:connect(test_pool, {[], [{dsn, "ClickHouse"}]}).
<0.86.0> Connected to undefined
=PROGRESS REPORT==== 8-May-2019::20:21:00 ===
          supervisor: {local,odbcproxy_sup}
             started: [{pid,<0.76.0>},
                       {id,test_pool},
                       {mfargs,
                           {poolboy,start_link,
                               [[{name,{local,test_pool}},
                                 {worker_module,odbcproxy_worker}],
                                [{dsn,"ClickHouse"}]]}},
                       {restart_type,permanent},
                       {shutdown,5000},
                       {child_type,worker}]
{ok,<0.76.0>}

```

