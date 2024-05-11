left([X, Y], [X, NewY]) :-
    NewY is Y - 1.

right([X, Y], [X, NewY]) :-
    NewY is Y + 1.

up([X, Y], [NewX, Y]) :-
    NewX is X - 1.

down([X, Y], [NewX, Y]) :-
    NewX is X + 1.

move(State, Next, 1):-
    left(State, Next);
    right(State, Next);
    up(State, Next);
    down(State, Next).

calculateHeuristic([X1, Y1], [X2, Y2], H) :-
    H is abs(X2 - X1) + abs(Y2 - Y1).

isValidNodeAndColor(Board, [X, Y], [NewX, NewY]) :-
    nth0(X, Board, Row), %gets the xth row
    nth0(Y, Row, Color),%gets the yth element of the xth row
    length(Row, RowLength),
    length(Board, BoardLength),
    NewX >= 0, NewX < BoardLength, NewY >= 0, NewY < RowLength,
    nth0(NewX, Board, NextRow),
    nth0(NewY, NextRow, NewColor),
    Color = NewColor.

getNextState(Board, [State,_,G,_,_],Open,Closed,Goal,[Next,State,NewG,NewH,NewF]):-
    move(State, Next, MoveCost),
    isValidNodeAndColor(Board,State,Next),
    calculateHeuristic(Next, Goal, NewH),
    NewG is G + MoveCost,
    NewF is NewG + NewH,
    ( not(member([Next,_,_,_,_], Open)) ; memberButBetter(Next,Open,NewF) ),
    ( not(member([Next,_,_,_,_],Closed));memberButBetter(Next,Closed,NewF)).

memberButBetter(Next, List, NewF):-
    findall(F, member([Next,_,_,_,F], List), Numbers),
    min_list(Numbers, MinOldF),
    MinOldF > NewF.

getAllValidChildren(Board, Node, Open, Closed, Goal, Children):-
    findall(Next, getNextState(Board, Node, Open, Closed, Goal, Next), Children).%find all possible Next states that can be reached from Node

addChildren(Children, Open, NewOpen):-
    append(Open, Children, NewOpen).%open+children

getBestState(Open, BestChild, Rest):-
    findMin(Open, BestChild),
    delete(Open, BestChild, Rest).

findMin([X], X):- !.
findMin([Head|T], Min):-
    findMin(T, TmpMin),
    Head = [_, _, _, _, HeadF],
    TmpMin = [_, _, _, _, TmpF],
    (TmpF < HeadF -> Min = TmpMin ; Min = Head).

printSolution([State, null, _, _, _], _):-
    write("Solution: "), nl,
     write(State).

printSolution([State, Parent, _, _, _], Closed):-
    member([Parent, GrandParent, PrevG, Ph, Pf], Closed),
    printSolution([Parent, GrandParent, PrevG, Ph, Pf], Closed),
    write(' -> '),write(State).

assignRow([], _, _).
assignRow([Color|Cols], RowNum, ColNum) :-
    write('Enter color for cell ['), write(RowNum), write(','), write(ColNum), write('] (r, y, b): '), nl,
    read(Color),
    NewColNum is ColNum + 1,
    assignRow(Cols, RowNum, NewColNum).

assignBoard([], _, _).
assignBoard([Row|Rows], RowNum, Cols) :-
    RowNum >= 0,
    length(Row, Cols),
    assignRow(Row, RowNum, 0), % Start column index from 0
    NewRowNum is RowNum + 1, % Increase row index by 1
    assignBoard(Rows, NewRowNum, Cols). % Recursively initialize the next row

search([], _, _,_) :-    
     write('No path exists'), nl, !.
     
search(Open, Closed, Goal, _):-
    getBestState(Open, [CurrentState, Parent, G, H, F], _),
    CurrentState = Goal,
    write("Search is complete!"), nl,
    printSolution([CurrentState, Parent, G, H, F], Closed), !.

search(Open, Closed, Goal, Board):-
    getBestState(Open, CurrentNode, TmpOpen),
    getAllValidChildren(Board, CurrentNode, TmpOpen, Closed, Goal, Children),
    addChildren(Children, TmpOpen, NewOpen), 
    append(Closed, [CurrentNode], NewClosed),
    search(NewOpen, NewClosed, Goal, Board).

main :-
    write('# of rows: '), nl,
    read(Rows),
    write('# of columns: '), nl,
    read(Cols),
    RowNum is 0, % Start row index from 0
    length(Board, Rows),
    assignBoard(Board, RowNum, Cols),
    write('Enter [x,y] for the start position : '), nl,
    read(Start),
    write('Enter [x,y] for the goal position : '), nl,
    read(Goal),
    search([[Start, null, 0, 0, 0]], [], Goal, Board).