% Example board
% board([
%     [(0,0,red), (0,1,red), (0,2,red)],
%     [(1,0,red), (1,1,blue), (1,2,red)],
%     [(2,0,red), (2,1,red), (2,2,red)]
% ]).
% Define the colors
color(r).
color(b).
color(y).

create_board(Board) :-
    write('Enter the number of rows (M): '),
    read(M),
    write('Enter the number of columns (N): '),
    read(N),
    createboard(M, N, Board),
    assign_indices(Board, IndexedBoard),
    getCycle(IndexedBoard).

%for each M rows
createboard(M, N, Board) :-
    length(Board, M), 
    maplist(create_row(N), Board).

%create row of length N
create_row(N, Row) :-
    length(Row, N),
    maplist(read_color, Row).

% input color
read_color(CellColor) :-
    write('Enter cell color: '),
    read(CellColor).



assign_indices(Board, IndexedBoard) :-
    assign_indices(Board, 0, 0, IndexedBoard).

% Base case:
assign_indices([], _, _, []).

assign_indices([Row|Rest], RowIndex, _, [IndexedRow|IndexedRest]) :-
    NextRowIndex is RowIndex + 1,
    assign_indices(Rest, NextRowIndex, 0, IndexedRest),
    assign_indices_row(Row, RowIndex, 0, IndexedRow).

assign_indices_row([], _, _, []).

assign_indices_row([Cell|Rest], RowIndex, ColIndex, [(RowIndex,ColIndex,Cell)|IndexedRest]) :-
    NextColIndex is ColIndex + 1,
    assign_indices_row(Rest, RowIndex, NextColIndex,IndexedRest).


% Moves (Left, Right, Up, Down)
valid_moves(_, _, _, Moves) :-
    Moves = [(0,-1), (0,1), (-1,0), (1,0)]. 

get_children(Node, Children, Board) :-
    Node = (Row, Col, _),
    valid_moves(_, _, _, Moves),
    findall((NewRow, NewCol, Color), (
        member((DeltaRow, DeltaCol), Moves),
        NewRow is Row + DeltaRow,
        NewCol is Col + DeltaCol,
        nth0(NewRow, Board, NewRowList),
        nth0(NewCol, NewRowList, (_, _, Color)),
        (NewRow, NewCol) \== (Row, Col)
    ), Children).

node_in_path(Node, Path) :-
    member(Node, Path).

find_cycle(StartNode, Cycle, Board) :-
    find_cycle(StartNode, [], [], Cycle, Board).

find_cycle(CurrentNode, Path, Visited, Cycle, Board) :-
    \+ member(CurrentNode, Path), 
    get_children(CurrentNode, Children, Board), 
    member(Child, Children), 
    find_cycle(Child, [CurrentNode|Path], [CurrentNode|Visited], Cycle, Board).
    
find_cycle(CurrentNode, Path, _, Cycle, _) :-
    member(CurrentNode, Path),
    length(Path, N), N >= 4,
    isSameColor(Path), 
    adjacent_cells(Path), 
    reverse([CurrentNode|Path], TempCycle),
    removeDuplicatesNodes(TempCycle, Cycle),
    isAdjancent(Cycle). 

isSameColor([(_, _, Color)|Path]) :-
    isSameColor(Path, Color).

isSameColor([], _).
isSameColor([(_, _, Color)|Path], Color) :-
    isSameColor(Path, Color).

adjacent_cells([(Row1, Col1, _), (Row2, Col2, _)|Path]) :-
    abs(Row1 - Row2) + abs(Col1 - Col2) =:= 1, 
    adjacent_cells([(Row2, Col2, _)|Path]).

adjacent_cells([(_,_,_)|[]]). % Base case
isAdjancent([]).
isAdjancent([_]).
isAdjancent([(Row1, Col1, _), (Row2, Col2, _)|T]) :-
    abs(Row1 - Row2) + abs(Col1 - Col2) =:= 1,
    isAdjancent([(Row2, Col2, _)|T]).

removeDuplicatesNodes([], []).
removeDuplicatesNodes([Node|Nodes], [Node|UniqueNodes]) :-
    \+ member(Node, Nodes),
    removeDuplicatesNodes(Nodes, UniqueNodes).
removeDuplicatesNodes([Node|Nodes], UniqueNodes) :-
    member(Node, Nodes),
    removeDuplicatesNodes(Nodes, UniqueNodes).

bfs_find_cycle(Board, Cycle) :-
    color(Color),
    member(RowList, Board),
    member(Node, RowList),
    find_cycle(Node, Cycle, Board),
    member((_,_,Color), Cycle).


getCycle(Board) :-
    bfs_find_cycle(Board, Cycle),! ->write("Cycle ="),writeln(Cycle);
    writeln("No Cycle found").
