-module(mops_store).

-behaviour( gen_server ).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2]).

-export([ get_db/0 ]).

get_db() -> gen_server:call( brndb, db ).

start_link() ->
	gen_server:start_link( { local, brndb }, 
			       mops_store, 
			       "priv/indata/NH3_17_nl.brn", 
			       [] ).

init( InFile ) ->
	{ ok, Fd } = brn_file:open( InFile ),
	Db = xydb:from_file(fun() -> brn_file:readxy(Fd) end ),
	brn_file:close(Fd),
	{ ok, Db }.

handle_call( db, _From, Db ) ->
	{reply, Db, Db }.

handle_cast( _, State ) -> { noreply, State }.
