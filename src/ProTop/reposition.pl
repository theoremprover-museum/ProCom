%%*****************************************************************************
%% $Id: reposition.pl,v 1.1 1994/06/07 16:42:01 gerd Exp $
%%*****************************************************************************
%% Author: Gerd Neugebauer
%%=============================================================================

:- true. /*%--- Just to make pcode happy. -------------------------------------


\Predicate reposition_nodes/2().

\PL*/
reposition_nodes(Max,DX) :-
	functor(NewXarray,x,Max),
	setval(proof_tree_counter,1),
	repeat,
	getval(proof_tree_counter,CNT),
	getval(x_array,Xarray),
	reposition_x(Max,Xarray,NewXarray,DX,_),
	( CNT < 30 ->
	    CNT1 is CNT + 1,
	    setval(proof_tree_counter,CNT1),
	    setval(x_array,NewXarray),
	    fail
	;
	    store_pos(Max,NewXarray),
	    setval(x_array,0)
	).

store_pos(N,XA) :-
	( N > 0 ->
	    retract(node_position(N,_,Y)),
	    arg(N,XA,Xfloat),
	    fix(Xfloat,X),
	    assert(node_position(N,X,Y)),
	    N1 is N -1,
	    store_pos(N1,XA)
	;   true
	).

reposition_x(No,X1,X2,DX,OM) :-
	( No < 1 ->
	    true
	;
	    node_lr(No,L,R),
	    node_son(Father,No),
	    ( Father = 0 ->
		Xup = 0
	    ;   arg(Father,X1,Xup)
	    ),
	    findall(S,(node_son(No,Son),arg(Son,X1,S)),Sons),
	    sum_list(Sons,Sum,Len),
	    arg(No,X1,X),

	    move0(X,Xup,100,0.8,M0),

	    ( L \== [],
	      arg(L,X1,XL),
	      X < XL+DX ->
	        M1 is (XL+DX-X)*0.5 
	    ;   M1 = 0
	    ),
	    ( R \== [],
	      arg(R,X1,XR),
	      X > XR-DX ->
		M2 is (XR-DX-X)*0.5 
	    ;   M2 = 0
	    ),
	    ( Len == 0 ->
		M3 = 0
	    ;   M3 is (Sum/Len - X) * 0.1
	    ),

	    NewX is X + M0+M1+M2+M3,
	    arg(No,X2,NewX),
	    No1 is No-1,
	    reposition_x(No1,X1,X2,DX,OM)
	).

move0(X,T,A,B,Move) :-
	Dir is T-X,
	( Dir < -A -> Move is -A * B
        ; Dir >  A -> Move is  A * B
        ;	      Move is Dir* B
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate sum_list/3(+List, ?Sum, ?Length).

\PL*/
sum_list([],0,0).
sum_list([A],A,1) :- !.
sum_list([A,B],Sum,2) :- !, Sum is A+B.
sum_list([H|T],Sum,Len) :-
	sum_list(T,TailSum,TailLen),
	Sum is TailSum+H,
	Len is TailLen+1.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\EndProlog */
