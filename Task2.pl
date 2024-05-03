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
    nth0(X, Board, Row),
    nth0(Y, Row, Color),
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
    findall(Next, getNextState(Board, Node, Open, Closed, Goal, Next), Children).

addChildren(Children, Open, NewOpen):-
    append(Open, Children, NewOpen).

getBestState(Open, BestChild, Rest):-
    findMin(Open, BestChild),
    delete(Open, BestChild, Rest).

