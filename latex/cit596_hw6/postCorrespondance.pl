/*
    Program: postCorrespondance.pl
Description: Finds a solution to the Post Correspondance Problem, if a solution 
             is available. Prints the solution as a list of item positions.
             Input the problem as a list of tuples where each tuple has the
             top value followed by the bottom value.
*/

% Try to find a one-item solution, if that doesn't work, recursively expand 
% possible solution size until a solution is found.
solve(Problem, Solution)    :- solve(Problem, Solution, 1).
solve(Problem, Solution, N) :- get_solution(Problem, ("",""), Solution, N).
solve(Problem, Solution, N) :- M is N + 1, solve(Problem, Solution, M).

% Using an item as the start point, recursively go through the problem space
% and try to find a combination of items that are a solution.
get_solution(_, ("",""), [], 0).
get_solution(Problem, Answer, [SolutionI|SolutionRest], N) :- 
    N > 0, 
    nth1(SolutionI, Problem, Item),
    append_top_bottom(Answer, Item, AnswerAppended), 
    check_answer(AnswerAppended, AnswerChecked), 
    M is N - 1, 
    get_solution(Problem, AnswerChecked, SolutionRest, M).

% Append the given item to the top and bottom of the current answer
append_top_bottom((Top1, Bottom1), (Top2, Bottom2), (Top3, Bottom3)) :- 
    append(Top1, Top2, Top3), 
    append(Bottom1, Bottom2, Bottom3).

% Check for match in top and bottom of all positions in the current answer 
check_answer(([], B), ([], B)) :- !.
check_answer((A, []), (A, [])).
check_answer(([X|A], [X|B]), (Ao, Bo)) :- check_answer((A, B), (Ao, Bo)).
