%%      Module: numpers.erl
%%      Author: Steven Tomcavage (stomcava@seas.upenn.edu)
%%     Created: September, 2010
%% Description: Describe a number's personality for a range of numbers
-module(numpers).

%% Exported Functions
-export([explore_numbers/2, is_antifirst/1, is_abundant/1, is_smug/1, 
		 is_square/1, is_triangular/1, is_happy/1, is_prime/1]).

% Prints a list of true attributes for each integer from Lo to Hi
explore_numbers(Lo, Hi) -> 
	lists:foreach(
	  fun(N) -> io:format("~w ~w~n", [N, num_attributes(N)]) end, 
	  lists:seq(Lo, Hi)).

% Gets a list of all the true attributes for N
num_attributes(N) ->
	Attr = [{prime, is_prime(N)}, 
			{happy, is_happy(N)},
			{triangular, is_triangular(N)},
			{square, is_square(N)},
			{smug, is_smug(N)},
			{abundant, is_abundant(N)},
			{antifirst, is_antifirst(N)}
		   ],
	[X || {X, Y} <- Attr, Y =:= true].

% Gets a list of all the integer divisors from 1 to N of any positive N 
get_divisors(N) when N >= 0 -> [X || X <- lists:seq(1, N), N rem X == 0];
get_divisors(_) -> [].

% Antifirst numbers have more positive divisors than any previous integer.
% Note: hard-code case where N = 1 because get_divisors(0) = [].
is_antifirst(1) -> true;
is_antifirst(N) when N > 1 ->
	length(get_divisors(N)) > 
		lists:max([length(get_divisors(A)) || A <- lists:seq(0, N - 1)]);
is_antifirst(_) -> false.

% Abundant numbers are numbers where the sum of its divisors is
% greater than twice the number
is_abundant(N) when N > 0 -> lists:sum(get_divisors(N)) > N * 2;
is_abundant(_) -> false.

% Smug numbers are numbers that are the sum of two squares
is_smug(N) when N > 0 ->
	Squares = [X * X || X <- lists:seq(1, N)],
	lists:member(N, [X + Y || X <- Squares, X < N, Y <- Squares, Y < N]);
is_smug(_) -> false.

% Square numbers are numbers that can be broken down into 
% a sum of consecutive odd positive integers
is_square(N) when N > 0 -> 
	Odd_Seqs = [lists:seq(1, X, 2) || X <- lists:seq(1, N), X rem 2 =:= 1],
	lists:member(N, [lists:sum(L) || L <- Odd_Seqs]);
is_square(_) -> false.

% Triangular numbers are positive numbers that can be broken down into
% a sum of consecutive integers
is_triangular(N) when N > 0 -> 
	Consecutive_Seqs = [lists:seq(1, X) || X <- lists:seq(1, N)],
	lists:member(N, [lists:sum(L) || L <- Consecutive_Seqs]);
is_triangular(_) -> false.

% Happy numbers can have their digits repeatedly squared and summed to get
% down to 1. If they square and sum to 4 at any point, they aren't happy.
% Note: convert N to string of ASCII characters and subtract 48 to get value.
is_happy(1) -> true;
is_happy(4) -> false;
is_happy(N) when N > 0 ->
	N_As_Digits = [Y - 48 || Y <- integer_to_list(N)],
	is_happy(
	  lists:foldl(fun(X, Sum) -> (X * X) + Sum end, 0, N_As_Digits));
is_happy(_) -> false.

% Prime number are positive numbers greater than 1 that only have
% 1 and themselves as divisors. 
is_prime(N) when N > 1 -> get_divisors(N) =:= [1, N|[]];
is_prime(_) -> false.
