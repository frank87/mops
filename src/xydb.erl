-module( xydb ).

-export( [ new/0, delete/1, insert/3, from_file/1, from_file/2, bbox/1 ] ).
-export( [ tst/1, tst/2 ] ).
-export( [ lookup_circle/3, filter_fold/4, sort_fold/4, fold/3 ] ).


-record( db_pointer, { tid, node } ).
-record( branch, { range = empty, node } ).

-define( MAX_SIZE, 12 ).
%
%  Table - actions
%
new() ->
	Tid = ets:new(xydb,[]),
	ets:insert( Tid, { 0, 1, 1 } ), 		% db-info
	root_new( Tid,  #branch{ node = new_node( Tid, [] ) } ),
	Tid.

delete( Tid ) -> ets:delete( Tid ).

%
%  User functions
%

from_file( Tid, RR ) ->
	case RR() of
		{ P = { _,_}, PL } -> 
			insert( Tid, P, PL ),
			from_file( Tid, RR );
		eof	-> eof
	end.

from_file( RR ) ->
	Tid = new(),
	from_file( Tid, RR ),
	Tid.

insert( Tid, P, PL ) ->
	Rt = root_node(Tid),
	( Rb = #branch{} ) = get_node(Rt), 
	case branch_insert( Rt, Rb , P, PL ) of
		ok	-> ok;
		Result = #branch{} -> set_node( Rt, Result );
		Result when is_list( Result ) ->
			set_node(Rt,Result),
			root_new( Tid, branch_create( Rt ) )
	end.

bbox( Tid ) ->
	#branch{ range = R } = get_node( root_node(Tid) ),
	R.

filter_fold( Tid, Filter, Function, Acc ) ->
	Rt = root_node(Tid),
	Rb = get_node( Rt ),
	branch_ffold( Rt, Rb, Filter, Function, Acc ).

fold( Tid, Function, Acc ) ->
	Rt = root_node(Tid),
	Rb = get_node(Rt),
	branch_fold( Rt, Rb, Function, Acc ).

sort_fold( Tid, Sorter, Function, Acc ) ->
	Rt = root_node(Tid),
	Rb = get_node(Rt),
	{ Sorted, A } = branch_sfold( Rt, Rb, Sorter, Function, [], Acc ),
	lists:foldl( Function, A, Sorted ).

lookup_circle( Tid, Center, Radius )->
	filter_fold( Tid, 
		     fun ( X ) -> xydb_fun:filter_circle( Center, Radius, X ) end,
		     fun ( Rec, A ) -> [Rec| A] end,
		     [] ).

%
% lookup
% 
% Filter Fold: fold a subset defined by TestFun
branch_ffold( Node, #branch{ range = R, node = Sub }, TestFun, FoldFun, A )  ->
	case TestFun( R ) of
		true  ->
			sub_ffold( Node, get_node( Node#db_pointer{ node = Sub }), TestFun, FoldFun, A );
		all   ->
			sub_fold( Node, get_node( Node#db_pointer{ node = Sub }), FoldFun, A );
		false ->
			A
	end;
branch_ffold( _, Data = { P = {_,_}, _ }, TestFun, FoldFun, A ) ->
	case TestFun(P) of
		true	-> FoldFun( Data, A );
		false	-> A
	end.

sub_ffold( Node, InData, TestFun, FoldFun, A ) ->
	lists:foldl( fun ( Elem, Ain ) ->
			       branch_ffold( Node, Elem, TestFun, FoldFun, Ain )
	       end,
	       A,
	       InData ).

% Fold all
branch_fold( Node, #branch{ node = Sub }, FoldFun, A )  ->
	sub_fold( Node, get_node( Node#db_pointer{ node = Sub }), FoldFun, A );
branch_fold( _, Data, FoldFun, A ) ->
	FoldFun( Data, A ).

sub_fold( Node, InData, FoldFun, A ) ->
	lists:foldl( fun ( Elem, Ain ) ->
			       branch_fold( Node, Elem, FoldFun, Ain )
		     end, 
		     A, 
		     InData ).


sfold_catchup( _SortFun, _FoldFun, _LWM, [], A ) -> { [], A };
sfold_catchup( SortFun, FoldFun, LWM, InData = [ Hd = { P, PL } | Tail ], A ) ->
	case SortFun( P, LWM ) of
		true	-> sfold_catchup( SortFun, FoldFun, LWM, Tail, FoldFun( Hd, A ) );
		false	-> { InData, A }
	end.

branch_sfold( _Node, { P, PL }, SortFun, _FoldFun, Sorted, A )  ->
	{ lists:sort( fun ( { P1, _ }, { P2, _ } ) -> SortFun( P1, P2 ) end,
		      [{P,PL}|Sorted] ),
	  A };
branch_sfold( Node, #branch{ range = R, node = Sub }, SortFun, FoldFun, Sorted, A )  ->
	% Current Range R defines low water mark, could fail if higher level R-leafs overlap :(
	{ S2, A2 } = sfold_catchup( SortFun, FoldFun, R, Sorted, A ),
	sub_sfold( Node, get_node( Node#db_pointer{ node = Sub }), SortFun, FoldFun, S2, A2 ).

sub_sfold( Node, InData, SortFun, FoldFun, Sorted, A ) ->
	lists:foldl( fun ( Elem, { SortIn, Ain } ) ->
				     branch_sfold( Node, Elem, SortFun, FoldFun, SortIn, Ain )
		     end,
		     { Sorted, A },
		     lists:sort( fun ( P1, P2 ) -> subSort( SortFun, P1, P2 ) end, InData )
		   ).

subSort( SortFun, #branch{ range = R1 }, #branch{ range = R2 } ) -> SortFun( R1, R2 );
subSort( SortFun, { P1, _ }, { P2, _ } ) -> SortFun( P1, P2 ).



% ETS manipulation
% database info is stored in record 0
% { 0, root, next unique Id }
%
uniq_id( #db_pointer{ tid = Tid } ) -> ets:update_counter( Tid, 0, { 3, 1 } ).
root( Tid ) -> ets:lookup_element( Tid, 0, 2 ).

root( #db_pointer{ tid = Tid }, NR ) -> root( Tid, NR );
root( Tid, Nr ) -> ets:update_element( Tid, 0, { 2, Nr } ).

root_set( #db_pointer{ tid = Tid }, NR ) -> root( Tid, NR );
root_set( Tid, Nr ) -> ets:update_element( Tid, 0, { 2, Nr } ).

root_new( Tid, Data ) -> 
	NR = new_node( #db_pointer{tid=Tid}, Data ),
	root_set( Tid, NR ).

root_node( Tid ) -> #db_pointer{ tid = Tid, node = root(Tid) }.

get_node( #db_pointer{ tid = Tid, node = Node } ) -> 
	[ {Node, Data} |_] = ets:lookup( Tid, Node ),
	Data.

set_node( #db_pointer{ tid = Tid, node = Node }, Data ) -> 
	true = ets:insert( Tid, { Node, Data } ),
	ok.

new_node( D = #db_pointer{}, Data ) -> 
	Node = uniq_id(D),
	set_node( D#db_pointer{ node = Node }, Data ),
	Node;
new_node( Tid, Data ) -> new_node( #db_pointer{ tid = Tid }, Data ).


%
% data manipulatie
%
%	Possible content Node:
%	[ #branch{} | Tl ] -> Branch node
%	[ { P, PL } | Tl ] -> data node
%	return: new number of elements ( Parent splits )
%		content is updated
data_insert( Node = #db_pointer{}, P, PL ) ->
	case get_node( Node ) of
		[ First = #branch{} | Tl ] ->
			data_add2best( Node, First, P, PL, Tl, [] );
		Data	-> set_node( Node, [ {P, PL }| Data ] ), 
			length( Data ) + 1
	end.

%	Insert value into branch or subbranches
%	Node:	used for accessing subnodes
%	Branch: Branch to update
%	P, Pl:  point + payload
%
%	return:
%	ok		: No update neccesary
%	#branch{}	: updated branch (caller should save)
%	[ #branch{} ]	: split branches
branch_insert( Node = #db_pointer{}, 
	       Branch = #branch{ range = Range, node = Sub },
	       P,
	       PL ) -> 
	case { range_test( Range, P ), 
	       data_insert( Node#db_pointer{ node = Sub }, 
			    P, 
			    PL ) } of 
		{ 0, N } when N < ?MAX_SIZE -> ok;
		{ _, N } when N < ?MAX_SIZE -> Branch#branch{ range = range_add( Range, P ) };
		{ _, _ } -> branch_split( Node, Branch )
	end.

branch_split( Node = #db_pointer{}, #branch{ node = Sub, range = { {Xmin, _}, {Ymin, _} } } ) -> 
	{ NodeNr, NewNode } = data_split( Node#db_pointer{ node = Sub }, { Xmin, Ymin } ),
	[ branch_create( Node#db_pointer{ node = NodeNr } ),
	  branch_create( Node#db_pointer{ node = NewNode })].


branch_create( Node = #db_pointer{} )	->
	InData = get_node( Node ),
	#branch{ range = lists:foldl( fun ( P, A ) -> range_add( A, P ) end, 
				      empty,
				      InData ),
		 node = Node#db_pointer.node }.



data_add2best( Node = #db_pointer{}, Best = #branch{}, P, PL, [] , A ) ->
	case branch_insert( Node, Best, P, PL ) of
		ok		->	length( A ) + 1;
		NB = #branch{} 	->	RV =  [NB|A],
					set_node( Node, RV ),
					length(RV);
		Split when is_list( Split ) ->
				RV = Split ++ A,
				set_node( Node, RV ),
				length(RV)
	end;
data_add2best( Node = #db_pointer{}, 
	       Best = #branch{range = Rb }, 
	       P, PL, 
	       ToDo = [Next = #branch{range = Rn}|Tl] , A ) ->
	case { range_test( Rb, P ), range_test( Rn, P ) } of
	{ 0, _ }   		->  data_add2best( Node, Best, P, PL, [], A ++ ToDo );
	{ D1, D2 } when D1 < D2 ->  data_add2best( Node, Best, P, PL, Tl, [Next|A] );
	{ _D1, _D2 } 		->  data_add2best( Node, Next, P, PL, Tl, [Best|A] )
	end.
	

% split a node
data_split( Db = #db_pointer{}, CP = { _, _ } ) -> 
	InData = get_node( Db ),
       	PreSort = lists:map( fun ( R ) -> { calcNorm(CP, R), R } end, 
			     InData ),
       	Sort = lists:sort( fun ( { N1, _ }, { N2, _ } ) -> N1 =< N2 end, PreSort ),
	PostSort = lists:map( fun ( { _, R } ) -> R end, Sort ),
	{ Out1, Out2 } = lists:split( ?MAX_SIZE div 2, PostSort ),
	set_node( Db, Out1 ),
	{ Db#db_pointer.node , new_node( Db, Out2 ) }.

calcNorm( {Xm, Ym}, { {X, Y }, _PL } ) -> point2index( {X - Xm, Y - Ym} );
calcNorm( {Xm, Ym}, #branch{ range = R } ) -> 
	{ X, Y } = range_center( R ),
	point2index( { X- Xm, Y - Ym } );
calcNorm( A, B ) -> { A, B }.

% mix X and Y bit for bit ex: 1000 , 1010 -> 11000100

point2index( { X, Y }, A ) when is_float(X); is_float(Y) -> 
	point2index({trunc(X), trunc(Y)}, A );
point2index( { 0, 0 }, A ) -> lists:foldl( fun( X, Rv ) -> ( 4 * Rv ) + X end, 0, A );
point2index( { X, Y }, A ) ->
	point2index( { X div 2, Y div 2 },
		     [ ( X band 1 ) * 2 + ( Y band 1 )|A] ).

point2index( P ) -> point2index( P, [] ).
			

% distance outside the box ( > 0 , 0 is in the box )
range_test( { Rx, Ry }, { X, Y } ) -> range_test( Rx, X ) + range_test( Ry, Y );
range_test( { Xmin, _Xmax }, Xt ) when Xt < Xmin -> Xmin - Xt;
range_test( { _Xmin, Xmax }, Xt ) when Xt > Xmax -> Xt - Xmax;
range_test( { _, _ }, _ ) -> 0;
range_test( empty, _ ) -> nan.

% Empty + range
range_add( empty, R = { { _, _ }, { _, _ } } ) -> R;
% unpacking data
range_add( R1, { P2 = { _X, _Y }, _PL } ) -> range_add( R1, P2 );
% Empty + point
range_add( empty, { Xt, Yt } ) -> 
	{ { Xt, Xt }, { Yt, Yt } };
% unpacking branch
range_add( R1, #branch{ range = R2 } ) -> range_add( R1, R2 );
% range + range
range_add( { 
  		Rx = { _, _ }, 
		Ry = { _, _ } 
 	   }, 
	   { 
	    	{ Xmin, Xmax }, 
		{ Ymin, Ymax } 
	   } 
	 ) -> 
	{ range_add1d( range_add1d( Rx, Xmin ), Xmax ), 
	  range_add1d( range_add1d( Ry, Ymin ), Ymax ) };
% range + point
range_add( { 
  		Rx = { _, _ }, 
		Ry = { _, _ } 
	    }, 
	   { Xt, Yt } ) -> 
	{ range_add1d( Rx, Xt ), range_add1d( Ry, Yt ) }.


range_add1d( { Xmin, Xmax }, Xt ) when Xt < Xmin -> { Xt, Xmax };
range_add1d( { Xmin, Xmax }, Xt ) when Xt > Xmax -> { Xmin, Xt };
range_add1d( { Xmin, Xmax }, _ ) -> { Xmin, Xmax }.

range_center( { { Xmin, Xmax }, { Ymin, Ymax } } ) -> { (Xmin + Xmax)/2, (Ymin + Ymax)/2 };
range_center( { X, Y } ) -> { X, Y }.

tst(0, _Db) -> ok;
tst(N, Db) -> insert( Db, { rand:uniform( N * 5 ),  rand:uniform(N*5) }, N ),
	tst( N - 1, Db ).

tst(N) -> Db = new(),
	  RV1 = tst( N, Db ),
	  RV2 = ets:tab2list(Db),
	  delete(Db),
	  { RV1, RV2 }.
