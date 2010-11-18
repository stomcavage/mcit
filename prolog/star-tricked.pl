/*
    Program: star-tricked.pl
     Author: Steven Tomcavage (stomcava@seas.upenn.edu)
       Date: September, 2010
Description: Solution for "Star Tricked", Dell Logic Puzzles, October 1999
*/

% Each person saw a UFO on a different day.
ufo(balloon).
ufo(clothesline).
ufo(frisbee).
ufo(water_tower).

person(ms_barrada).
person(ms_gort).
person(mr_klatu).
person(mr_nikto).

day(tuesday).
day(wednesday).
day(thursday).
day(friday).

solve :-
    ufo(BarradaUFO), ufo(GortUFO), ufo(KlatuUFO), ufo(NiktoUFO),
    all_different([BarradaUFO, GortUFO, KlatuUFO, NiktoUFO]),
    
    person(MsBarrada), person(MsGort), person(MrKlatu), person(MrNikto),
    all_different([MsBarrada, MsGort, MrKlatu, MrNikto]),

    Triples = [ [MsBarrada, BarradaUFO, tuesday],
                [MsGort, GortUFO, wednesday],
                [MrKlatu, KlatuUFO, thursday],
                [MrNikto, NiktoUFO, friday] ],

	% 1. Mr. Klatu made his sighting at some point earlier in the week than the
	%    one who saw the balloon, but at some point later in the week than the
	%    one who spotted the frisbee (who isn't Ms. Gort)
	\+(member([mr_klatu, balloon, _], Triples)),
	\+(member([mr_klatu, frisbee, _], Triples)),
	\+(member([ms_gort, frisbee, _], Triples)),
	earlier([mr_klatu, _, _], [_, balloon, _], Triples),
	earlier([_, frisbee, _], [mr_klatu, _, _], Triples),

	% 2. Friday's sighting was made by either Ms. Barrada or the one who saw
	%    a clothesline, or both
	(member([ms_barrada, _, friday], Triples) ; 
	 member([_, clothesline, friday], Triples)),

	% 3. Mr. Nikto did not make his sighting on Tuesday
	\+(member([mr_nikto, _, tuesday], Triples)),
   
    % 4. Mr. Klatu isn't the one whose object turned out to be a water tower
    \+(member([mr_klatu, water_tower, _], Triples)),
   
    tell(MsBarrada, BarradaUFO, tuesday),
    tell(MsGort, GortUFO, wednesday),
    tell(MrKlatu, KlatuUFO, thursday),
    tell(MrNikto, NiktoUFO, friday).

% Succeeds if all elements of the argument list are bound and different.
% Fails if any elements are unbound or equal to some other element.
all_different([H | T]) :- member(H, T), !, fail.
all_different([_ | T]) :- all_different(T).
all_different([_]).

earlier(X, _, [X | _]).
earlier(_, Y, [Y | _]) :- !, fail.
earlier(X, Y, [_ | T]) :- earlier(X, Y, T). 

tell(X, Y, Z) :-
    write(X), write(' saw the '), write(Y), write(' on '), write(Z), 
    write('.'), nl.
