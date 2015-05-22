/* !!!MAKE SURE THE SCRIPT COMPILES FIRST!!! */
consult(hw7).
/* Tests for Problem 1 */
put(1,hello,[],D),D=[[1,hello]],!. /* a put to an empty dictionary */
put(1,hello,[[2,two]],D),permutation(D,[[2,two],[1,hello]]),!. /* a simple put without replacement */
put(1,hello,[[2,two],[1,one]],D),permutation(D,[[2,two],[1,hello]]),!. /* a put with replacement */
findall(D,put(1,hello,D,[[2,two],[1,hello]]),L),permutation(L,[[[2,two]],[[2,two],[1,_]]]),!. /* opposite of put */
findall(D,put(1,bye,D,[[1,bye]]),L),permutation(L,[[[1,_]],[]]),!. /* another opposite of put */
findall(D,put(3,three,D,[[1,one],[2,two],[3,three]]),L),permutation(L,[[[1,one],[2,two],[3,_]],[[1,one],[2,two]]]),!. /* a more complex opposite of put */
\+(put(1,bye,D,[[3,three],[2,hello]])),!. /* impossible opposite of put */

\+(get(3,[[2,two],[1,hello]],V)),!. /* try to get something not in the dictionary */
get(1,[[2,hello],[1,bye],[0,zero]],V),V=bye,!. /* get a value given a key */
get(1,[[1,bye]],V),V=bye,!. /* get a value given a key again */
findall(K,get(K,[[2,hello],[1,hello]],hello),L),permutation(L,[1,2]),!. /* find all keys given a value */
\+(get(K,[[3,three],[2,two],[1,one]],zero)),!. /* find keys given a non-existence value */

/* Tests for Problem 2 */
subseq([1,3],[1,2,3]),!. /* direct comparsion */
\+(subseq([3,1],[1,2,3])). /* according to the spec, the subsequence has to be in the same order */
subseq([5,1,9],[4,5,0,1,9]),!. /* unsorted sequence */
\+(subseq([1,5,9],[4,5,0,1,9])). /* unsorted sequence again, but this one should fail */
findall(X,subseq(X,[1,2]),L),permutation(L,[[1,2],[1],[2],[]]),!. /* a simple subsequence test */
findall(X,subseq(X,[3,1,2]),L),permutation(L,[[3,1,2],[3,1],[3,2],[1,2],[3],[1],[2],[]]),!. /* a more complex test */

/* Tests for Problem 3 */
sudoku([[2,1,_,_],
        [4,_,_,_],
        [_,_,_,4],
        [_,_,1,_]], Solution),Solution=[[2,1,4,3],[4,3,2,1],[1,2,3,4],[3,4,1,2]],!. /* a sudoku with solution */
sudoku([[4,_,1,_],
        [1,_,2,_],
        [_,4,_,1],
        [_,1,_,2]], Solution),Solution=[[4,2,1,3],[1,3,2,4],[2,4,3,1],[3,1,4,2]],!. /* another sudoku with solution */
\+(sudoku([[1,2,3,4],
        [_,_,_,_],
        [4,3,2,1],
        [3,1,4,2]],Solution)). /* a sudoku with no solution */

/* Tests for Problem 4 */
verbalArithmetic([S,E,N,D,M,O,R,Y],[S,E,N,D],[M,O,R,E],[M,O,N,E,Y]),D=7,E=5,M=1,N=6,O=0,R=8,S=9,Y=2,!. /* this one may take some time */
verbalArithmetic([C,O,A,L,S,I],[C,O,C,A],[C,O,L,A],[O,A,S,I,S]),A=6,C=8,I=9,L=0,O=1,S=2,!. /* another test case with solution */
\+(verbalArithmetic([A,B,C,D],[A,B,D],[A,B,C],[A,B,C,C])). /* this one has no solution */
\+(verbalArithmetic([S,A],[S,A],[A],[S,A])). /* this one has no solution either */
\+(verbalArithmetic([D,E,L,A,Y,N,O,M,R],[D,E,L,A,Y],[N,O],[M,O,R,E])). /* just for fun, if you get what it means */

/* Tests for Problem 5 */
length(Moves,L),L<8,towerOfHanoi([[1,2,3],[],[]],[[],[1,2,3],[]],Moves),L=7,
        Moves=[to(peg1,peg2),to(peg1,peg3),to(peg2,peg3),to(peg1,peg2),to(peg3,peg1),to(peg3,peg2),to(peg1,peg2)],!.
length(Moves,L),L<2,towerOfHanoi([[1,2,3],[],[]],[[2,3],[1],[]],Moves),L=1,
        Moves=[to(peg1,peg2)],!.
length(Moves,L),L<8,towerOfHanoi([[1,2,3],[4,5,6,7],[]],[[],[1,2,3,4,5,6,7],[]],Moves),L=7,
	Moves=[to(peg1,peg2),to(peg1,peg3),to(peg2,peg3),to(peg1,peg2),to(peg3,peg1),to(peg3,peg2),to(peg1,peg2)],!. /* more than 3 discs */