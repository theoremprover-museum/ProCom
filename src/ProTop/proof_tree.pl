%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: proof_tree.pl,v 1.4 1995/01/11 09:15:45 gerd Exp $
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%% This file is part of ProCom.
%%% It is distributed under the GNU General Public License.
%%% See the file COPYING for details.
%%% 
%%% (c) Copyright 1995 Gerd Neugebauer
%%% 
%%% Net: gerd@imn.th-leipzig.de
%%% 
%%%****************************************************************************

:- module_interface(proof_tree). /*%-------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.4 $}

\PL*/
:- export proof_tree/2,
	  proof_tree/3.
:- begin_module(proof_tree).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- dynamic node_info/2,
	   node_position/3,
	   node_lr/3,
	   node_son/2.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The library |literal| is needed to define the ops |++| and |--|.

\PL*/
:-	lib(literal).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The predicate |member/2| is taken from the library |lists|.

\PL*/
:-	lib(lists).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate proof_tree/2(Proof, Stream).

\PL*/
proof_tree(proof(GoalNo,P),Stream) :-
	proof_tree(proof(GoalNo,P),
		   [dx(100),dy(100),tree],
		   Stream).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate proof_tree/3(Proof, Options, Stream).

\PL*/
proof_tree(proof(GoalNo,P),Options,Stream) :-
	retract_all(node_info(_,_)),
	retract_all(node_position(_,_,_)),
	retract_all(node_son(_,_)),
	retract_all(node_lr(_,_,_)),
	Top  = 0,
	Ytop = 0,
	Xtop = 0,
	( once(member(dx(DX),Options)) ->
	    true
	;   DX = 100
	),
	( once(member(dy(DY),Options)) ->
	    true
	;   DY = DX
	),
	setval(proof_no,Top),
	assert(node_info(Top,goal(GoalNo))),
	assert(node_position(Top,Xtop,Ytop)),
	proof_tree_phase_1(P,Top,Sons),

	( once(member(tree,Options)) ->
	    getval(proof_no,Max),
	    functor(Xarray,x,Max),
		
	    position_level(Sons,0,Ytop,Width,Height,DX,DY,Xarray),
	    ( once(member(reposition,Options)) ->
		setval(x_array,Xarray),
		reposition_nodes(Max,DX)
	    ;	true
	    ),

	    fix(Width,W),
	    printf(Stream,"\n\\begin{ProofTree}{%w}{%w}\n",[W,Height]),
	    (	node_position(No,X,Y),
		printf(Stream,"	 \\Node{%w}{%w}{%w}\n",[No,X,Y]),
		fail
	    ;	nl(Stream)
	    ),
	    (	node_son(Node,Son),
		node_position(Node,X1,Y1),
		node_position(Son,X2,Y2),
		printf(Stream,"	 \\Next{%w}{%w}{%w}{%w}\n",[X1,Y1,X2,Y2]),
		fail
	    ;	printf(Stream,"\\end{ProofTree}\n",[])
	    )
	;   true
	),

	( once(member(info,Options)) ->
	    printf(Stream,"\\begin{ProofInfo}\n",[]),

	    setof(Node,A^B^node_position(Node,A,B),Nodes),
	    (	member(Node,Nodes),
		(   printf(Stream,"  \\begin{Info}{%w}\n",[Node]),
		    node_info(Node,Info),
		    tree_node(Info,Stream,'  '),
		    fail
		;   printf(Stream,"  \\end{Info}\n",[])
		),
		fail
	    ;	printf(Stream,"\\end{ProofInfo}\n",[])
	    )
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate tree_node/3(Literal,Stream,Prefix).

\PL*/
tree_node(--(--P),S,In) :- !,printf(S,"%Dw{\\it %Dw}\\NL\n"	,[In,P]).
tree_node( -(--P),S,In) :- !,printf(S,"%Dw{\\it %Dw}\\NL\n"	,[In,P]).
tree_node(--(++P),S,In) :- !,printf(S,"%Dw\\NEG{\\it %Dw}\\NL\n",[In,P]).
tree_node( -(++P),S,In) :- !,printf(S,"%Dw\\NEG{\\it %Dw}\\NL\n",[In,P]).
tree_node(   --P ,S,In) :- !,printf(S,"%Dw\\NEG{\\it %Dw}\\NL\n",[In,P]).
tree_node(   ++P ,S,In) :- !,printf(S,"%Dw{\\it %Dw}\\NL\n"	,[In,P]).
tree_node(++(--P),S,In) :- !,printf(S,"%Dw\\NEG{\\it %Dw}\\NL\n",[In,P]).
tree_node(++(++P),S,In) :- !,printf(S,"%Dw{\\it %Dw}\\NL\n"	,[In,P]).
tree_node(    P	 ,S,In) :- !,printf(S,"%Dw{\\it %Dw}\\NL\n"	,[In,P]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate proof_tree_phase_1/3().

\PL*/
proof_tree_phase_1([],_,[]).
proof_tree_phase_1([P|T],Father,[node(No,PL)|TP]) :-
	P =.. [F|Args],
	incval(proof_no),
	getval(proof_no,No),
	assert(node_info(No,F)),
	assert(node_son(Father,No)),
	proof_tree_phase_1_args(Args,No,[],PL),
	proof_tree_phase_1(T,Father,TP).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate proof_tree_phase_1_args/4(List,Number,In,Out).

\PL*/
proof_tree_phase_1_args([],_,X,X).
proof_tree_phase_1_args([H|T],No,In,Out) :-
	( functor(H,'.',2) ->
	    proof_tree_phase_1(H,No,HL),
	    append(In,HL,Next),
	    proof_tree_phase_1_args(T,No,Next,Out)
	; H == [] ->
	    proof_tree_phase_1_args(T,No,In,Out)
	;
	    assert(node_info(No,H)),
	    proof_tree_phase_1_args(T,No,In,Out)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate position_level/8().

\PL*/
position_level(Nodes,W,Y,Width,Height,DX,DY,Xarray) :-
	( Nodes == [] ->
	    Height = Y,
	    Width  = W
	;
	    store_neighbors(Nodes,[]),
	    Y1 is Y - DY,
	    X1 is (1-length(Nodes)) * DX/2,
	    position_level_nodes(Nodes,X1,Y1,[],NextLevel,DX,Xarray),
	    L  is length(Nodes) * DX/2,
	    W1 is max(L,W),
	    position_level(NextLevel,W1,Y1,Width,Height,DX,DY,Xarray)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate position_level_nodes/7().

\PL*/
position_level_nodes([],_,_,X,X,_,_).
position_level_nodes([node(No,Next)|T],X,Y,In,Out,DX,Xarray) :-
	append(In,Next,Mid),
	assert(node_position(No,X,Y)),
	arg(No,Xarray,X),
	X1 is X + DX,
	position_level_nodes(T,X1,Y,Mid,Out,DX,Xarray).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate store_neighbors/2().

\PL*/
store_neighbors([],_).
store_neighbors([node(No,_)],L) :-
	!,
	assert(node_lr(No,L,[])).
store_neighbors([node(No,_)|T],L) :-
	T = [node(R,_)|_],
	!,
	assert(node_lr(No,L,R)),
	store_neighbors(T,No).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
