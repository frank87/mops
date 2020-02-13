-module( xydb_fun ).

-compile( export_all ).

-define( RANGE, { {_,_}, {_,_} } ).

%
%  general:
%  	point:	{ X, Y }
%  	range:	{ { Xmin, Xmax }, { Ymin, Ymax } }
%  		rectangle with all points {X,Y} with 
%  			Xmin =< X =< Xmax and
%  			Ymin =< Y =< Ymax
%
%
%  filters:
%  	filter( point ) ->
%  		true	:	pass
%  		false	:	block
%  	filter( range ) ->
%  		true	:	partly pass
%  		false	:	all block
%  		all	:	all pass
%
sqr( X ) -> X * X.

% Outside box, R around range -> false
filter_circle( {Xc, Yc}, R, { {Xmin, Xmax}, {Ymin, Ymax} } ) 
  	when Yc > Ymax + R,
	     Yc < Ymin - R,
	     Xc < Xmin - R,
	     Xc > Xmax + R	->  false; % Far away
% above, or below: in range
filter_circle( {Xc, _}, _, { {Xmin, Xmax}, {_, _} } ) 
	when Xc > Xmin,
	     Xc < Xmax	-> true;
% left or right: in range
filter_circle( {_ , Yc}, _, { {_ , _ }, { Ymin, Ymax } } ) 
	when Yc > Ymin,
	     Yc < Ymax	-> true;
% corners: test corner
filter_circle( {Xc, Yc}, R, { {Xmin, Xmax}, {Ymin, Ymax} } ) ->  
	min( sqr(Xc - Xmin), sqr(Xc - Xmax) ) +
	min( sqr(Yc - Ymin), sqr(Yc - Ymax) ) =< sqr(R);
filter_circle( {Xc, Yc}, R, { Xt, Yt } ) ->  
	sqr( Xc - Xt ) + sqr( Yc - Yt ) =< sqr(R).


%
%   sorters:
%   	sorter( A (point/range), B(point/range) ) ->
%   		A =< B	-> true
%   		other	-> false
%   	ordering function like the stdlib:
%   	 It is assumed that the following properties hold of F for all x, y and z:
%		- if x F y and y F x then x = y (F is antisymmetric);
%		- if x F y and and y F z then x F z (F is transitive);
%		- x F y or y F x (F is total).
%
%	ranges sort like the lowest point in the sort:
%		sorter( Range, Point ) = true  for all Points in Range
%


% find point in Range closest to Point
range_closest( {Xc, Yc}, { {Xmin, _Xmax}, {Ymin, _Ymax} } ) when Xc < Xmin, Yc < Ymin -> {Xmin, Ymin};
range_closest( {Xc, Yc}, { {Xmin, _Xmax}, {_Ymin, Ymax} } ) when Xc < Xmin, Yc > Ymax -> {Xmin, Ymax};
range_closest( {Xc, Yc}, { {_Xmin, Xmax}, {Ymin, _Ymax} } ) when Xc > Xmax, Yc < Ymin -> {Xmax, Ymin};
range_closest( {Xc, Yc}, { {_Xmin, Xmax}, {_Ymin, Ymax} } ) when Xc > Xmax, Yc > Ymax -> {Xmax, Ymax};
%  Xmin < Xc < Xmax and/or Ymin < Yc < Ymax
range_closest( {Xc, Yc}, { {_Xmin, _Xmax}, {_Ymin, Ymax} } ) when Yc > Ymax -> {Xc, Ymax};
range_closest( {Xc, Yc}, { {_Xmin, _Xmax}, {Ymin, _Ymax} } ) when Yc < Ymin -> {Xc, Ymin};
range_closest( {Xc, Yc}, { {_Xmin, Xmax}, {_Ymin, _Ymax} } ) when Xc > Xmax -> {Xmax, Yc};
range_closest( {Xc, Yc}, { {Xmin, _Xmax}, {_Ymin, _Ymax} } ) when Xc < Xmin -> {Xmin, Yc};
% Point in Range
range_closest( {Xc, Yc}, _ ) -> {Xc, Yc}.


sort_distance( Pc, R1 = ?RANGE, R2 ) -> 
		sort_distance( Pc, range_closest( Pc, R1 ), R2 );
sort_distance( Pc, R1, R2 = ?RANGE ) -> 
		sort_distance( Pc, R1, range_closest( Pc, R2 ) );
sort_distance( { Xc,Yc }, { X1, Y1 }, { X2, Y2 } ) -> 
		sqr( Xc - X1 ) + sqr( Yc - Y1 ) =< sqr( Xc - X2 ) + sqr( Xc - Y2 ).
