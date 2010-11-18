/*
    Program: adventure.pl
     Author: Steven Tomcavage (stomcava@seas.upenn.edu)
       Date: September, 2010
Description: A text-based adventure game written in Prolog
*/
:- dynamic i_am_at/1, at/2, holding/1, opened/1, used/1.

/* This fact defines your location */
i_am_at(middle).

/* These facts define how the locations are connected */
path(middle, n, n_door).
path(n_door, s, middle).
path(n_door, w, w_door).
path(n_door, e, e_door).

path(middle, s, s_door).
path(s_door, n, middle).
path(s_door, w, w_door).
path(s_door, e, e_door).

path(middle, e, e_door).
path(e_door, w, middle).
path(e_door, n, n_door).
path(e_door, s, s_door).

path(middle, w, w_door).
path(w_door, e, middle).
path(w_door, n, n_door).
path(w_door, s, s_door).

/* These facts define the answers to the riddles at each door */
the_answer('egg', n_door).
the_answer('fish', s_door).
the_answer('wind', w_door).
the_answer('teeth', e_door).

/* These rules define where things are in the game */
reveal(n_door) :- assert(at(key, n_door)).
reveal(w_door) :- assert(at(bottle, w_door)).
reveal(e_door) :- assert(at(passcode, e_door)).
reveal(s_door).

/* These rules describe how to pick up an object. */
take(X) :-
	holding(X),
	write('You''re already holding it!'),
	nl.

take(X) :-
	i_am_at(Place),
	at(X, Place),
	retract(at(X, Place)),
	assert(holding(X)),
	write('OK.'),
	nl.

take(_) :-
	write('I don''t see it here.'),
	nl.

/* These rules describe how to put an object down. */
drop(X) :-
	holding(X),
	i_am_at(Place),
	retract(holding(X)),
	assert(at(X, Place)),
	write('OK.'),
	nl.

drop(_) :-
	write('You aren''t holding it!'),
	nl.

/* These rules describe how to answer the guard's questions */
answer(_) :-
    i_am_at(Place),
    opened(Place),
	write('The door is already opened. No need to answer a riddle here.'),
	nl.
    	
answer(X) :-
    i_am_at(Place),
    the_answer(X, Place),
    assert(opened(Place)),
    reveal(Place),
    write('Your answer is correct! The guard opens the door. '),
    nl, nl,
    look.

answer(_) :-
    i_am_at(middle),
    write('Don''t speak unless spoken to. '),
    nl.

answer(_) :-
    write('Your answer is incorrect. The guard smiles an evil grin. '),
    nl.

/* These rules describe how to use items */
use(X) :-
    used(X),
    write('You''ve already used the '),
    write(X),
    nl.
    
use(X) :-
    i_am_at(s_door),
    opened(s_door),
    holding(X),
    assert(used(X)),
    look.

use(X) :-
    holding(X),
	write('The '),
	write(X),
	write(' won''t help you now. Save it for later. '),
	nl.    	

use(_) :-
    write('You don''t have one to use! '),
    nl.
    	
/* These rules define the direction letters as calls to go/1. */
n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).

/* This rule tells how to move in a given direction. */
go(Direction) :-
	i_am_at(Here),
	path(Here, Direction, There),
	retract(i_am_at(Here)),
	assert(i_am_at(There)),
	look.

go(_) :-
	write('You can''t go that way.').

/* This rule tells how to look about you. */
look :-
	i_am_at(Place),
	describe(Place),
	nl,
	notice_objects_at(Place),
	nl.

/* These rules set up a loop to mention all the objects in your vicinity. */
notice_objects_at(Place) :-
	at(X, Place),
	write('There is a '), write(X), write(' here.'), nl,
	fail.

notice_objects_at(_).

/* This rule tells how to die. */
die :-
	finish.

/* Under UNIX, the "halt." command quits Prolog but does not
   remove the output window. On a PC, however, the window
   disappears before the final output can be seen. Hence this
   routine requests the user to perform the final "halt." */
finish :-
	nl,
	write('The game is over. Please enter the "halt." command.'),
	nl.

/* This rule just writes out game instructions. */
instructions :-
	nl,
	write('Enter commands using standard Prolog syntax.'), nl,
	write('Available commands are:'), nl,
	write('start.             -- to start the game.'), nl,
	write('n.  s.  e.  w.     -- to go in that direction.'), nl,
	write('answer(Word).      -- to answer a riddle'), nl,
	write('take(Object).      -- to pick up an object.'), nl,
	write('drop(Object).      -- to put an object down.'), nl,
	write('use(Object).       -- to use an object.'), nl,
	write('look.              -- to look around you again.'), nl,
	write('instructions.      -- to see this message again.'), nl,
	write('halt.	          -- to end the game and quit.'), nl,
	nl.

/* This rule prints out instructions and tells where you are. */
start :-
	instructions,
    write('You awake to find yourself in the middle of a stone room. '),
    write('There is a closed wooden door in each of the four walls. '),
    write('Each door has a guard in front of it. Your job is to escape. '),
    nl, nl,
	look.

/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */
describe(middle) :- 
	write('You are in the middle of the room. ').

describe(s_door) :-
    opened(s_door),
    used(passcode),
    used(bottle),
    used(key),
    write('You are at the open door on the south side of the room. '),
    write('You open the door with the key, pass through the door, '),
    write('and find yourself outside of the room, back at your normal size. '),
	nl, nl,
    write('Congratulations! You''ve escaped! '),
	nl, nl,
    die.

describe(s_door) :-
    opened(s_door),
    used(passcode),
    used(bottle),
    write('You are at the open door on the south side of the room. '),
    write('You''ve drunk the contents of the bottle and shrank '),
    write('to the size of a mouse. Before you is a small door that you '),
    write('didn''t see before. The door can be opened with a key. ').

describe(s_door) :-
    opened(s_door),
    used(passcode),
    write('You are at the open door on the south side of the room. '),
    write('You have decyphered the sign on the door with your passcode. '),
    write('The sign says "Drink the bottle to escape." ').

describe(s_door) :-
    opened(s_door),
    write('You are at the open door on the south side of the room. '),
    write('Behind this door is a wall with a sign that looks to be '),
    write('encoded. A passcode might help to decode the message. ').

describe(s_door) :-
    write('You are at the door on the south side of the room. '),
    write('A guard blocks your way. He has a riddle for you: '),
    nl, nl,
	/* This riddle comes from The Hobbit by J.R.R. Tolkein */
    write('"Alive without breath, as cold as death; '),
    write('never thirsty, ever drinking, all in mail never clinking." '),
    nl, nl,
    write('If you can answer the riddle, the guard will open the door.').

describe(n_door) :-
    opened(n_door),
    write('You are at the open door on the north side of the room. '),
    write('Behind the door in this wall is a small closet. ').

describe(n_door) :-
    write('You are at the door on the north side of the room. '),
    write('A guard blocks your way. He has a riddle for you: '), 
    nl, nl,
	/* This riddle comes from The Hobbit by J.R.R. Tolkein */
    write('"A box without hinges, key or lid, '), 
    write('yet golden treasure inside is hid." '),
    nl, nl,
    write('If you can answer the riddle, the guard will open the door.').
    
describe(w_door) :-
    opened(w_door),
    write('You are at the open door on the west side of the room. '),
    write('Behind the door in this wall is a small closet. ').

describe(w_door) :-
    write('You are at the door on the west side of the room. '),
    write('A guard blocks your way. He has a riddle for you: '),
    nl, nl,
	/* This riddle comes from The Hobbit by J.R.R. Tolkein */
	write('"Voiceless it cries, wingless flutters, '),
	write('toothless bites, mouthless mutters" '),
    nl, nl,
    write('If you can answer the riddle, the guard will open the door.').

describe(e_door) :-
    opened(e_door),
    write('You are at the open door on the east side of the room. '),
    write('Behind the door in this wall is a small closet. ').

describe(e_door) :-
    write('You are at the door on the east side of the room. '),
    write('A guard blocks your way. He has a riddle for you: '),
    nl, nl,
	/* This riddle comes from The Hobbit by J.R.R. Tolkein */
	write('"Thirty white horses on a red hill, first they champ '),
	write('then they stamp, then they stand still." '),
    nl, nl,
    write('If you can answer the riddle, the guard will open the door.').
    