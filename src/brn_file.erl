-module( brn_file ).

-export([ open/1, readln/1, close/1 ]).
-export([ readxy/1, get/2 ] ).

-record( brn_line, { snr,x, y, q, hc, h, r, s, dv, cat, area, ps, component }).

open( FileName ) -> file:open( FileName, [ read ] ).
close( FD ) -> file:close( FD ).

get( q, #brn_line{ q = Q } ) -> Q.

readln( Fd ) -> 
	readln( Fd, io:get_line( Fd, "" ) ).

readln( Fd, "!" ++ _  ) -> readln(Fd);
readln( _, L ) ->  
	case io_lib:fread("~s ~d ~d ~f ~s ~s ~s ~s ~s ~d ~s ~s",L) of
		{ ok,      [ Snr,X, Y, Q, Hc, H, R, S, Dv, Cat, Area, Ps] , Component } 
			-> #brn_line{ 
			      snr = Snr,
			      x = X, 
			      y = Y, 
			      q = Q, 
			      hc = Hc, 
			      h = H,
			      r = R, 
			      s = S, 
			      dv = Dv, 
			      cat = Cat, 
			      area = Area, 
			      ps = Ps, 
			      component = Component };
		eof	-> eof
	end.

readxy( Fd ) -> 
	case readln( Fd ) of
		L = #brn_line{ x=X, y=Y }
			-> { {X,Y}, L };
		eof	-> eof
	end.
