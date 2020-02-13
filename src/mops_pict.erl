-module( mops_pict ).

-export( [ init/2, tst/0 ] ).

init(Req0, State) -> 
	Db = mops_store:get_db(),
	Map =  lists:foldr( fun ( R, A ) -> draw_point( R, A ) end,
	 				   nil, 
					   xydb:lookup_circle( Db,
							       { 191500, 554500 },
							       100000
							     )
	 		 ),
	Png = md_chart:out(Map),
	Req = cowboy_req:stream_reply( 200, 
				       #{ <<"content-type">> => <<"image/png">> }, 
				       Req0 ),
	cowboy_req:stream_body( Png, fin, Req ),
	{ok, Req, State}.

draw_point( { P, PL }, A ) ->
	md_chart:add_point( P,
			    brn_file:get( q, PL ),
			    A ).

tst() -> 
	Db = mops_store:get_db(),
	lists:foldr( fun ( R, A ) -> draw_point( R, A ) end,
	 				   nil, 
					   xydb:lookup_circle( Db,
							       { 191500, 554500 },
							       10000
							     )
	 		 ).
