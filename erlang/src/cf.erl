 %% Author: Steven Tomcavage (stomcava@seas.upenn.edu)
 %% Created: October, 2010
 %% Description: Curve fitting in Erlang
 -module(cf).
 
 -export([start/0, adjust/4]).
 
 -import(lists, [dropwhile/2, foldl/3, foreach/2, map/2, member/2, 
				 nth/2, nthtail/2, seq/2, seq/3, sublist/3, zip/2]).
 
 %----------------------------------------------------------------------------
 % CONSTANTS
 %  (Note: constants are faked by using functions)
 %----------------------------------------------------------------------------
 
 % Tell the fit processes when to stop and return results.
 % Probably better to find a way to compare data between processes and 
 % dynamically decide when to stop, but this hack works well in testing.
 stopping() -> 300.
 
 % These are the polynomials we'll be generating a fit for
 polynomials() -> [linear, quadratic, cubic, quartic].
 
 % Each polynomial type is initialize with all its coefficients set to 1
 init_polynomial(linear) -> [1, 1];
 init_polynomial(quadratic) -> [1|init_polynomial(linear)];
 init_polynomial(cubic) -> [1|init_polynomial(quadratic)];
 init_polynomial(quartic) -> [1|init_polynomial(cubic)].
 
 %----------------------------------------------------------------------------
 % MAIN PROCESSING
 %
 % Here's how the processes communicate in this program:
 % 1. start spawns 4 processes by calling fit
 % 2. fit spawns between 2 and 5 processes by calling adjust
 % 3. adjust sends results back to fit process through fit_listener
 % 4. fit_listen sends results back to start process through start_listener
 % 5. start prints the results
 %----------------------------------------------------------------------------
 
 start() -> 
	 %% Read in the data file
	 {ok, Device} = file:open('curve_data', read),
	 {ok, CurveData} = io:read(Device, ''),
	 file:close(Device),
	 %% Start 4 processes running to find the best curve fit
	 register(start_pid, self()),
	 foreach(
	   fun(Type) -> 
			   FitPid = spawn(fun fit/0),
			   register(Type, FitPid),
			   io:format("~w (start) sending {~w, request, ~w, ~w} to ~w ~n",
						 [self(), self(), CurveData, Type, FitPid]),
			   FitPid ! {self(), request, CurveData, Type}
	   end, 
	   polynomials()),
	 start_listener([]).
 
 start_listener(Results) ->
	 % As each fit process returns, save the results
	 ActivePidList = dropwhile(fun(X) -> whereis(X) =:= undefined end, polynomials()),
	 if 
		 length(ActivePidList) > 0 -> 
			 receive
				 {FitPid, response, Type, Polynomial} ->
					 io:format("~w (start_listener) received {~w, response, ~w, ~w} ~n", 
							   [self(), FitPid, Type, Polynomial]),
					 start_listener([{Type, Polynomial}|Results]);
				 {'EXIT', Pid, Reason} ->
					 io:format("Process ~w closed: ~w ~n", [Pid, Reason])
				 after 100000 -> exit(timeout)
			 end;
		 length(ActivePidList) =:= 0 -> 
			 io:format("~n Results for best fit: ~p ~n", [Results])
	 end.
 
 fit() ->
	 % Spawn a server for each coefficient in this equation type
	 receive
		 {Pid, request, CurveData, Type} ->
			 io:format("~w (fit) received {~w, request, ~w, ~w} ~n", 
					   [self(), Pid, CurveData, Type]),
			 Polynomial = init_polynomial(Type),
			 AdjustPidList = 
				 map(
				   fun(Nth) -> 
						   process_flag(trap_exit, true),
						   AdjustPid = spawn_link(fun() -> adjust(CurveData, Nth) end),
						   io:format("~w (fit) sending {~w, request, ~w} to ~w ~n",
									 [self(), self(), Polynomial, AdjustPid]),
						   AdjustPid ! {self(), request, Polynomial},
						   AdjustPid
				   end, 
				   seq(1, length(Polynomial))),
			 InitErr = measure_error(CurveData, Polynomial),
			 fit_listener(AdjustPidList, CurveData, Type, Polynomial, InitErr, 1);
		 {'EXIT', Pid, Reason} ->
			 io:format("Process ~w closed: ~w ~n", [Pid, Reason])
		 after 100000 -> exit(timeout)
	 end.
 
 fit_listener(AdjustPidList, CurveData, Type, Polynomial, Err, I) ->
	 % Decide when to stop adjusting our polynomial and return a result
	 Stopping = stopping(),
	 if I >= Stopping -> 
			io:format("~w (fit_listener) sending {~w, response, ~w} back to ~w ~n",
					  [self(), self(), Polynomial, whereis(start_pid)]),
			start_pid ! {self(), response, Type, Polynomial},
			exit(normal);
		I < Stopping -> true end,
	 receive
		 % Handle the case where we found a better fit
		 {AdjustPid, response, OrigPoly, NewPoly, NewErr} when NewErr < Err ->
			 io:format("~w: ~w (fit_listener) received {~w, response, ~w, ~w, ~w} ~n", 
					   [I, self(), AdjustPid, OrigPoly, NewPoly, NewErr]),
			 foreach(
			   fun(X) -> 
					   io:format("~w (fit_listener) sending {~w, request, ~w} to ~w ~n", 
								 [self(), self(), NewPoly, X]),
					   X ! {self(), request, NewPoly}
			   end,
			   dropwhile(fun(X) -> X =:= AdjustPid end, AdjustPidList)),
			 fit_listener(AdjustPidList, CurveData, Type, NewPoly, NewErr, I + 1);
		 % Handle the case where we found a worse fit
		 {AdjustPid, response, OrigPoly, NewPoly, NewErr} when NewErr > Err ->
			 io:format("~w: ~w (fit_listener) received {~w, response, ~w, ~w, ~w} ~n", 
					   [I, self(), AdjustPid, OrigPoly, NewPoly, NewErr]),
			 io:format("~w (fit_listener) sending {~w, request, ~w} to ~w ~n", 
					   [self(), self(), Polynomial, AdjustPid]),
			 AdjustPid ! {self(), request, Polynomial},
			 fit_listener(AdjustPidList, CurveData, Type, Polynomial, Err, I + 1);
		 % Handle the case where the fit isn't changing
		 {AdjustPid, response, OrigPoly, NewPoly, NewErr} when NewErr == Err ->
			 io:format("~w: ~w (fit_listener) received {~w, response, ~w, ~w, ~w} ~n", 
					   [I, self(), AdjustPid, OrigPoly, NewPoly, NewErr]),
			 if OrigPoly =:= Polynomial ->
					io:format("~w (fit_listener) sending {~w, response, ~w} back to ~w ~n",
							  [self(), self(), NewPoly, whereis(start_pid)]),
					start_pid ! {self(), response, Type, NewPoly},
					exit(normal);
				OrigPoly =/= Polynomial ->
					io:format("~w (fit_listener) sending {~w, request, ~w} to ~w ~n", 
							  [self(), self(), Polynomial, AdjustPid]),
					AdjustPid ! {self(), request, Polynomial}
			 end,
			 fit_listener(AdjustPidList, CurveData, Type, Polynomial, Err, I + 1);
		 {'EXIT', Pid, Reason} ->
			 io:format("Process ~w closed: ~w ~n", [Pid, Reason])
		 after 100000 -> exit(timeout)
	 end.
 
 adjust(CurveData, Nth) ->
	 % Find the best fit for this data, by calling a recursive helper function
	 receive
		 {FitPid, request, Polynomial} ->
			 io:format("~w (adjust) received {~w, request, ~w} ~n", 
					   [self(), FitPid, Polynomial]),
			 Err = measure_error(CurveData, Polynomial),
			 {NewPolynomial, NewErr} = adjust(CurveData, Polynomial, Nth, Err),
			 io:format(
			   "~w (adjust) sending {~w, response, ~w, ~w} back to ~w ~n", 
			   [self(), self(), NewPolynomial, NewErr, FitPid]),
			 FitPid ! {self(), response, Polynomial, NewPolynomial, NewErr},
			 adjust(CurveData, Nth);
		 {'EXIT', Pid, Reason} ->
			 io:format("Process ~w closed: ~w ~n", [Pid, Reason])
		 after 100000 -> exit(timeout)
	 end.
 
 adjust(CurveData, Polynomial, Nth, Err) ->
	 % Recursively find best fit for this data, varying the Nth coefficient
	 NewErr = measure_error(CurveData, Polynomial),
	 if 
		 NewErr > Err -> 
			 {Polynomial, Err};
		 NewErr =< Err -> 
			 NewPolynomial = sublist(Polynomial, 1, Nth - 1) ++
								 [nth(Nth, Polynomial) + 1] ++
								 nthtail(Nth, Polynomial),
			 adjust(CurveData, NewPolynomial, Nth, NewErr)
	 
	 end.
 
 measure_error(CurveData, Polynomial) ->
	 % Find the sum of the squares of the standard deviation
	 foldl(
	   fun({X, Y}, Sum) -> Sum + math:pow(Y - evaluate(Polynomial, X), 2) end, 
	   0,
	   CurveData).
 
 evaluate(Polynomial, Point) ->
	 % Evalatue the polynomial at the given point (i.e., for y = x^2 + 1, find y)
	 foldl(
	   fun({X, Y}, Sum) -> Sum + (X * Y) end, 
	   0,
	   zip(
		 map(
		   fun(Exp) -> math:pow(Point, Exp) end, 
		   seq(length(Polynomial) - 1, 0, -1)),
		 Polynomial)).
