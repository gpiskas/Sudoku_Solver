%  Sudoku Solver
%  Copyright (C) 2013  George Piskas
%
%  This program is free software; you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation; either version 2 of the License, or
%  (at your option) any later version.
%
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License along
%  with this program; if not, write to the Free Software Foundation, Inc.,
%  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%
%  Contact: geopiskas@gmail.com

% Comment/Uncomment one of the following initial_state:

% Easy. - 8 missing (Iterations: 8)
initial_state([(1,1,1),(1,2,2),(1,3,0),(1,4,0),
               (2,1,0),(2,2,0),(2,3,2),(2,4,1),
               (3,1,2),(3,2,4),(3,3,0),(3,4,0),
               (4,1,0),(4,2,0),(4,3,4),(4,4,2)]).

% Medium. - 10 missing (Iterations: 10)
initial_state([(1,1,2),(1,2,0),(1,3,0),(1,4,0),
               (2,1,0),(2,2,1),(2,3,0),(2,4,0),
               (3,1,1),(3,2,0),(3,3,4),(3,4,3),
               (4,1,0),(4,2,4),(4,3,0),(4,4,0)]).

% Hard. - 13 missing (Iterations: 13)
initial_state([(1,1,4),(1,2,0),(1,3,0),(1,4,1),
               (2,1,0),(2,2,0),(2,3,0),(2,4,0),
               (3,1,0),(3,2,0),(3,3,0),(3,4,0),
               (4,1,0),(4,2,2),(4,3,0),(4,4,0)]).

% Empty board. - 16 missing (Iterations: 16)
initial_state([(1,1,0),(1,2,0),(1,3,0),(1,4,0),
               (2,1,0),(2,2,0),(2,3,0),(2,4,0),
               (3,1,0),(3,2,0),(3,3,0),(3,4,0),
               (4,1,0),(4,2,0),(4,3,0),(4,4,0)]).

% The goal is the same for each state. Description of final state follows.
goal(X):-
 X=[(1,1,X11),(1,2,X12),(1,3,X13),(1,4,X14),
    (2,1,X21),(2,2,X22),(2,3,X23),(2,4,X24),
    (3,1,X31),(3,2,X32),(3,3,X33),(3,4,X34),
    (4,1,X41),(4,2,X42),(4,3,X43),(4,4,X44)],
    
    % All possible numbers: [1,2,3,4].
    member(X11,[1,2,3,4]),member(X12,[1,2,3,4]),member(X13,[1,2,3,4]),member(X14,[1,2,3,4]),
    member(X21,[1,2,3,4]),member(X22,[1,2,3,4]),member(X23,[1,2,3,4]),member(X24,[1,2,3,4]),
    member(X31,[1,2,3,4]),member(X32,[1,2,3,4]),member(X33,[1,2,3,4]),member(X34,[1,2,3,4]),
    member(X41,[1,2,3,4]),member(X42,[1,2,3,4]),member(X43,[1,2,3,4]),member(X44,[1,2,3,4]),
    
    % Disallowed: Same number in same 2x2 grid more than once.
    difrent(X11,X12,X21,X22),difrent(X13,X14,X23,X24),difrent(X31,X32,X41,X42),difrent(X33,X34,X43,X44),
    % Disallowed: Same number in same column more than once.
    difrent(X11,X21,X31,X41),difrent(X12,X22,X32,X42),difrent(X13,X23,X33,X43),difrent(X14,X24,X34,X44),
    % Disallowed: Same number in same row more than once.
    difrent(X11,X12,X13,X14),difrent(X21,X22,X23,X24),difrent(X31,X32,X33,X34),difrent(X41,X42,X43,X44),!.

% Checks that all numbers are different.
difrent(A,B,C,D):-
  A\==B,
  A\==C,
  A\==D,
  B\==C,
  B\==D,
  C\==D.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Execution, solving sudoku.
sudoku(Solution):-
  initial_state(IS),
  V is 1000,
  % Taking Head, which includes the solution.
  bestFS([V-[IS]],[Solution|_],ITRS),
  % Find and print the number of iterations made.
  % Printing initial and final state as well.
  print('Iterations: '),print(ITRS),print('\n'),
  print('Initial  |  Solution\n'),printSudoku(IS,Solution).

% Prints the sudoku phases.
printSudoku([(_,_,A1),(_,_,B1),(_,_,C1),(_,_,D1),
               (_,_,A2),(_,_,B2),(_,_,C2),(_,_,D2),
               (_,_,A3),(_,_,B3),(_,_,C3),(_,_,D3),
               (_,_,A4),(_,_,B4),(_,_,C4),(_,_,D4)],
               [(_,_,A11),(_,_,B11),(_,_,C11),(_,_,D11),
               (_,_,A22),(_,_,B22),(_,_,C22),(_,_,D22),
               (_,_,A33),(_,_,B33),(_,_,C33),(_,_,D33),
               (_,_,A44),(_,_,B44),(_,_,C44),(_,_,D44)]):-
  print(A1),print(' '),print(B1),print(' '),print(C1),print(' '),print(D1),print('  |  '),print(A11),print(' '),print(B11),print(' '),print(C11),print(' '),print(D11),print('\n'),
  print(A2),print(' '),print(B2),print(' '),print(C2),print(' '),print(D2),print('  |  '),print(A22),print(' '),print(B22),print(' '),print(C22),print(' '),print(D22),print('\n'),
  print(A3),print(' '),print(B3),print(' '),print(C3),print(' '),print(D3),print('  |  '),print(A33),print(' '),print(B33),print(' '),print(C33),print(' '),print(D33),print('\n'),
  print(A4),print(' '),print(B4),print(' '),print(C4),print(' '),print(D4),print('  |  '),print(A44),print(' '),print(B44),print(' '),print(C44),print(' '),print(D44).

% Ending rule of Best First Search.
% State must fire with goal(X).
bestFS([_-[State|Path]|_],[State|Path],0):-
  goal(State),!.

% Recursive Best First Search.
bestFS([BestPath|RestPaths],Solution,ITRS):-
  % Finds next states, rates, sorts and executes recursively choosing the best one.
  next_states(BestPath,NewPaths),
  append(NewPaths,RestPaths,Frontier),
  keysort(Frontier,OrderedFrontier),
  bestFS(OrderedFrontier,Solution,TEMPITRS),
  % Iteration counter++.
  ITRS is TEMPITRS + 1.

% Finds all next states.
next_states(_-[State|Path],NewPaths):-
  findall(HV-[NewState,State|Path],
         (operator(State,NewState),heuristic(State,State,NewState,HV)),
         NewPaths).

% Operators: Filling a cell with 1, 2,3 or 4.
operator(P,NP):-
  % Find empty.
  member((X,Y,0),P),
  member(N,[1,2,3,4]),
  % Updates cell with chosen number.
  updateState((X,Y,0),(X,Y,N),P,NP).

% Recursive rule for new state production.
updateState(_,_,[],[]).
updateState(OLD,NEW,[OLD|P],[NEW|P]):-!.
updateState(OLD,NEW,[H|P],[H|NP]):-
  updateState(OLD,NEW,P,NP).

% Heuristic rating function. Needs current and next state, which we must rate.
% Rates the wellness of the chosen number in the specific cell.
heuristic([H|State],StateCopy,[H|NewState],HV):-
  heuristic(State,StateCopy,NewState,HV),!.
heuristic([(X,Y,0)|_],StateCopy,[(X,Y,NUM)|_],HV):-
  % Knowing NUM, find the rest 3.
  getRestNumbers(NUM,N1,N2,N3),
  % Find empty count.
  getEmptyCount(StateCopy,Empty),
  % Gets a list that contains the numbers in the same column and row
  % with the current cell and get a heuristic value ColRowHV.
  getColRowList(X,Y,StateCopy,L1),
  getHV(NUM,N1,N2,N3,L1,ColRowHV),
  % Gets a list that contains the numbers in the same 2x2 grid
  % with the current cell and get a heuristic value ColRowHV.
  getBoxList(X,Y,StateCopy,L2),
  getHV(NUM,N1,N2,N3,L2,BoxHV),
  % Heuristic wheighted calculation
  HV1 is ColRowHV+BoxHV*0.2+Empty,
  HV2 is BoxHV+ColRowHV*0.2+Empty,
  % Final value is the min of the two.
  (HV1=<HV2->HV is HV1 ; HV2<HV1->HV is HV2).

% Given N, returns the rest 3 numbers.
getRestNumbers(N,N1,N2,N3):-
  N==1->N1=2,N2=3,N3=4;
  N==2->N1=1,N2=3,N3=4;
  N==3->N1=1,N2=2,N3=4;
  N==4->N1=1,N2=2,N3=3.

% Recursive rule that returns the number of empty cells.
getEmptyCount([],0).
getEmptyCount([(_,_,X)|T],E):-
  member(X,[1,2,3,4]),
  getEmptyCount(T,E),!.
getEmptyCount([(_,_,0)|T],E):-
  getEmptyCount(T,NE),
  E is NE+1.

% Given X,Y coordinates, returns a list that contains the numbers in the same row and column.
getColRowList(X,Y,StateCopy,L1):-
  findall(K,(member((_,Y,K),StateCopy);member((X,_,K),StateCopy)),L1).

% Given X,Y coordinates, returns a list that contains the numbers in the same 2x2 grid.
getBoxList(X,Y,StateCopy,L1):-
  X>=3,Y>=3->findall(K,(member((X1,Y1,K),StateCopy),X1>=3,Y1>=3),L1);
  X=<2,Y=<2->findall(K,(member((X1,Y1,K),StateCopy),X1=<2,Y1=<2),L1);
  X>=3,Y=<2->findall(K,(member((X1,Y1,K),StateCopy),X1>=3,Y1=<2),L1);
  X=<2,Y>=3->findall(K,(member((X1,Y1,K),StateCopy),X1=<2,Y1>=3),L1).

% Rates N according to a number list L and the rest sudoku numbers except N.
getHV(N,N1,N2,N3,L,HV):-

  % If N exists, return a huge value to make this state impossible.
  member(N,L)->HV is 1000;
  
  % If N does not exist and the rest do exists, this is the best possible scenario. Rating = 1.
  not(member(N,L)), member(N1,L), member(N2,L), member(N3,L)->HV is 1;
  
  % The other possible scenario is to check all available numbers.
  % Thus, rating 2 means that 2 different numbers can be in that cell. Same for 3 and 4.
  not(member(N,L)), ( ( member(N1,L), member(N2,L), not(member(N3,L)) );( member(N1,L), member(N3,L), not(member(N2,L)) );( member(N2,L), member(N3,L), not(member(N1,L)) )) ->HV is 2;
  not(member(N,L)), ( ( not(member(N1,L)), not(member(N2,L)), member(N3,L) );( not(member(N1,L)), not(member(N3,L)), member(N2,L) );( not(member(N2,L)), not(member(N3,L)), member(N1,L) )) ->HV is 3;
  not(member(N,L)), not(member(N1,L)), not(member(N2,L)), not(member(N3,L)) ->HV is 4.
  