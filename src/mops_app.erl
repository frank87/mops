%%%-------------------------------------------------------------------
%% @doc mops public API
%% @end
%%%-------------------------------------------------------------------

-module(mops_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/png/[...]", mops_pict, []},
	       {"/[...]", cowboy_static, { priv_dir, mops, [ "www" ]} } ]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    mops_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
