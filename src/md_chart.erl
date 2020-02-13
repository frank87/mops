-module( md_chart ).


-export([ add_point/3, out/1 ]).

% raster: { gb_tree, bounding box }


findBB( X, {MinX, MaxX} ) when X < MinX -> { X, MaxX };
findBB( X, {MinX, MaxX} ) when X > MaxX -> { MinX, X };
findBB( _X, {MinX, MaxX} ) -> { MinX, MaxX };
findBB( X, _ ) -> { X, X }.

findBB( X, Y, V, { Xr, Yr, Vr } ) -> { findBB( X, Xr ), 
				       findBB( Y, Yr ),
				       findBB( V, Vr ) };
findBB( X, Y, V, _ ) -> { findBB( X, nil ), 
			  findBB( Y, nil ), 
			  findBB( V, nil ) }.


do_update( Key, Value, Tree ) ->
	case gb_trees:lookup( Key, Tree ) of
		none -> gb_trees:enter( Key, Value, Tree );
		{ value, Old } -> gb_trees:enter( Key, Value + Old, Tree )
	end.

add_point( Key = { X, Y }, Value, { T, BB } )->
	{ do_update( Key, Value, T ), findBB( X, Y, Value, BB  ) };
add_point( Key, Value, _ ) -> add_point( Key, Value, { gb_trees:empty(), nil } ).

scale( Xr, { MinX, MaxX }, M ) ->
	{ MinX - M, Xr / (M + MaxX - MinX) }.

dimensions( Xr, Yr, Vr, { Bx, By, {Vmin, Vmax} } ) ->
	{  scale( Xr, Bx, 2 ), 
	   scale( Yr, By, 2 ), 
	   scale( Vr, { math:log(Vmin), math:log(Vmax) }, 0 ) }.

rescale( X, { MinX, Xs } ) -> ( X - MinX ) * Xs.

out( none, _, _ ) -> ok;
out( { {X,Y}, Value, I }, D = { Xs, Ys, Vs }, Canvas ) ->
	io:fwrite("~p, ~p~n", [ rescale( X, Xs ), rescale( Y, Ys ) ] ),
	Color = rescale( Value, Vs ),
	egd:filledEllipse( Canvas, 
				{ rescale( X, Xs ),
				  rescale( Y, Ys ) },
				{ rescale( X, Xs ) + 1,
				  rescale( Y, Ys ) + 1},
				egd:color( {Color, Color, Color } ) ),
	out( gb_trees:next( I ), D, Canvas ).

out( { T, BB } ) ->
	Xr = 600, Yr=1000,
	Canvas = egd:create( Xr, Yr ),
	ok = out( gb_trees:next(gb_trees:iterator( T )), 
		  dimensions( Xr, Yr, 255, BB ), 
		  Canvas ),
	egd:render(Canvas).

