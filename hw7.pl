
%If the original dictionary is empty, return the dict with the pair
%If the key matches an entry replace it with the new value
%If it doesnt match the current head key recursively call put with the tail
%and append the current K,V pair to the result
put(K,V,[],[[K,V]]).
put(K,V,[[K|_]|T],.([K,V],T)).
put(K,V,[[H|T]|L],.(.(H,T),R)) :- K =\= H, put(K,V,L,R).

%If the key matches a key in the list, return the associated value
%If not recursively call get with the tail
get(K,[[K|[V]]|_],V).
get(K,[_|T],R) :- get(K,T,R).

%If they are both empty, return
%If the first list is empty, it is a subsequence of the second
%If the heads of both match, recursively call subseq on the tails of both
%If the heads do not match, recursively call subseq on the original first list
%and the tail of the second
subseq([],[]).
subseq([],[_|_]).
subseq([H|T],[H|L]) :- subseq(T,L).
subseq([H|T],[_|R]) :- subseq(.(H,T),R).

%Get all of the values for each position in the board
%Create a list of possible values for each position
%Find all permutations for the rows
%Narrow down the permutations by checking the columns
%Narrow further by checking the quadrants
%Create R by placing the variables in the correct places
sudoku([H1,H2,H3,H4],R) :- 
H1 = [X11,X12,X13,X14],
H2 = [X21,X22,X23,X24],
H3 = [X31,X32,X33,X34],
H4 = [X41,X42,X43,X44],
D = [1,2,3,4],
%Rows
permutation(H1,D), permutation(H2,D), permutation(H3,D), permutation(H4,D),
%Columns
permutation([X11,X21,X31,X41],D), permutation([X12,X22,X32,X42],D), 
permutation([X13,X23,X33,X43],D), permutation([X14,X24,X34,X44],D), 
%Quadrant
permutation([X11,X12,X21,X22],D), permutation([X13,X14,X23,X24],D), 
permutation([X31,X32,X41,X42],D), permutation([X33,X34,X43,X44],D), 
R = [[X11,X12,X13,X14],
	[X21,X22,X23,X24],
	[X31,X32,X33,X34],
	[X41,X42,X43,X44]].
	
%assignVals takes the list of all letters, first and second words, and a 
%list of possible digits (0-9)
%It uses member to assign a variable a random digit, and deletes the digit
%from the list of possible digits
%This creates all the possible variable assignments for (0-9)
assignVals([],_,_,_).
assignVals([H|T],[H1|T1],[H2|T2],D) :-
member(H,D), delete(D,H,X), assignVals(T,[H1|T1],[H2|T2],X).
%isValid checks that the correct variables of the first and second
%words sum to the correct variable in the resultant word
%It also takes a carry, which is initially 0, but is set to 1 if 
%the variables add to >= 10
isValid([],[],[],_).
isValid([],[],[H|_],C) :- H =:= C.
isValid([X],[Y],[H|T],C) :- X=\=0, Y=\=0,
X+Y+C < 10, H =:= X+Y+C, isValid([],[],T,0).
isValid([X],[Y],[H|T],C) :- X=\=0, Y=\=0,
X+Y+C >= 10, H =:= X+Y+C-10, isValid([],[],T,1).
isValid([H1|T1],[H2|T2],[H|T],C) :- T1\==[], T2\==[],
H1+H2+C < 10, H =:= H1+H2+C, isValid(T1,T2,T,0).
isValid([H1|T1],[H2|T2],[H|T],C) :- T1\==[], T2\==[],
H1+H2+C >= 10, H =:= H1+H2+C-10, isValid(T1,T2,T,1).
%First the lists of the first,second, and resultant words are reversed 
%so that they can be summed properly
%A list of possible digits (0-9) is created
%assignVals is called with the list of all variables, first and second words
%(in original order), and D
%isValid is then called to narrow the results for the correct variables
verbalArithmetic(L,[H1|T1],[H2|T2],S) :-
reverse([H1|T1],R1), reverse([H2|T2],R2), reverse(S,RS), D = [0,1,2,3,4,5,6,7,8,9],
assignVals(L,[H1|T1],[H2|T2],D), isValid(R1,R2,RS,0).

%listEqual checks if 2 lists are equivalent
listEqual([],[]).
listEqual([H1|T1],[H2|T2]) :- H1=:=H2, listEqual(T1,T2).
%First a check is made to see if the towers are equal
%If so, the empty list is returned
%If not, the limitations on the size of the list will choose the correct 
%moves to make
towerOfHanoi([P1,P2,P3],[G1,G2,G3],[]) :- listEqual(P1,G1), listEqual(P2,G2),
listEqual(P3,G3).
%If any peg is empty, a disk will be moved to that peg
towerOfHanoi([[],[H2|T2],P3],G,.(to(peg2,peg1),R)) :- 
towerOfHanoi([[H2],T2,P3],G,R).
towerOfHanoi([[],P2,[H3|T3]],G,.(to(peg3,peg1),R)) :- 
towerOfHanoi([[H3],P2,T3],G,R).
towerOfHanoi([[H1|T1],[],P3],G,.(to(peg1,peg2),R)) :- 
towerOfHanoi([T1,[H1],P3],G,R).
towerOfHanoi([P1,[],[H3|T3]],G,.(to(peg3,peg2),R)) :-
towerOfHanoi([P1,[H3],T3],G,R).
towerOfHanoi([[H1|T1],P2,[]],G,.(to(peg1,peg3),R)) :-
towerOfHanoi([T1,P2,[H1]],G,R).
towerOfHanoi([P1,[H2|T2],[]],G,.(to(peg2,peg3),R)) :-
towerOfHanoi([P1,T2,[H2]],G,R).
%If a peg can be moved from peg1 to another peg, make that move
towerOfHanoi([[H1|T1],P2,[H3|T3]],G,.(to(peg1,peg3),R)) :- H1 < H3,
towerOfHanoi([T1,P2,[H1,H3|T3]],G,R).
towerOfHanoi([[H1|T1],[H2|T2],P3],G,.(to(peg1,peg2),R)) :- H1 < H2,
towerOfHanoi([T1,[H1,H2|T2],P3],G,R).
%If a peg can be moved from peg2 to another peg, make that move
towerOfHanoi([[H1|T1],[H2|T2],P3],G,.(to(peg2,peg1),R)) :- H2 < H1,
towerOfHanoi([[H2,H1|T1],T2,P3],G,R).
towerOfHanoi([P1,[H2|T2],[H3|T3]],G,.(to(peg2,peg3),R)) :- H2 < H3,
towerOfHanoi([P1,T2,[H2,H3|T3]],G,R).
%If a peg can be moved from peg3 to another peg, make that move
towerOfHanoi([[H1|T1],P2,[H3|T3]],G,.(to(peg3,peg1),R)) :- H3 < H1,
towerOfHanoi([[H3,H1|T1],P2,T3],G,R).
towerOfHanoi([P1,[H2|T2],[H3|T3]],G,.(to(peg3,peg2),R)) :- H3 < H2,
towerOfHanoi([P1,[H3,H2|T2],T3],G,R).