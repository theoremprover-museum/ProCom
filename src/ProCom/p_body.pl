%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: p_body.pl,v 1.11 1995/03/13 19:58:01 gerd Exp $
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

:- module_interface(p_body). /*
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.11 $}

\PL*/
:- export compile_body/7,
	  reset_proof_steps/0,
	  put_proof_steps/3,
	  proof_steps/3. 
:- begin_module(p_body).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:-	lib(options),
	lib(literal),
	lib(p_info),
	lib(p_predicate).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- dynamic proof_steps/3.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate reset_proof_steps/0().

This predicate removes all information stored about proof steps.

\PL*/
reset_proof_steps :-
	retract_all(proof_steps(_,_,_)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate put_proof_steps/3(A,B,C).

\PL*/
put_proof_steps(A,B,C) :-
	assert(proof_steps(A,B,C)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate compile_body/7(+LiteralList, -Body, +Depth, +Path, ?NewPath
			  -Proof, Unifier).

This predicate compiles a list of literal terms |literal(Literal.Index)| into
a conjunction |Body| of Prolog predicates. If the body contains a pure literal
then this literal and the remaining subgoals are replaced by |fail|.

If |Depth| is has the functor |InOut/2| then the compilation passes through
the depth argument. Otherwise the depth argument is given to each literal
unchanged. These two variants are implemented as recursive loops in the
predicates |compile_body_1/6| and |compile_body_2/7|.

\PL*/
compile_body(LiteralList,Body,Depth,Path,PathOut,Proof,Unifier) :-
	( ( nonvar(Depth),
	    Depth = 'InOut'(Din,Dout) ) ->
	    compile_body_2(LiteralList,Body,
			   Din,Dout,
			   Path,PathOut,
			   Proof,Unifier)
	;
	    compile_body_1(LiteralList,Body,
			   Depth,
			   Path,PathOut,
			   Proof,Unifier)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate compile_body_1/7(+Literals, -Body, +Depth, +PathIn, +PathOut,
			    -Proof, Unifier).

\PL*/
compile_body_1([],true,_,P,P,[],_).
compile_body_1([literal(Literal,Index)|Rest],
	     Body,
	     Depth,
	     Path,
	     PathOut,
	     [Proof|SubProof],
	     Unifier) :-
	make_functor_from_template(Literal,Functor,_,_),
	proof_steps(Functor,Index,Candidates),
	( Candidates = [] ->
	    Body     = fail,
	    PathOut  = Path,
	    SubProof = fail
	;
	    negate_literal(Literal,Lit),
	    need_proc(Lit),
	    make_literal(Lit,Candidates,Depth,Path,Proof,Unifier,Head,[]),
	    term_variables(Lit,Vars),
	    ( is_option('ProCom:lemma') ->
		make_internal_rep(Lit,IR),
		Head2 = ( literal_wrapper(Head,Lit,Depth,Path,Proof,Vars),
			  lemma(IR,Path,NewPath) )
	    ;	Head2 = literal_wrapper(Head,Lit,Depth,Path,Proof,Vars),
		Path  = NewPath
	    ),
	    ( Rest = [] -> 
		Body	 = Head2,
		PathOut	 = Path,
		SubProof = []
	    ;	
		Body = ( Head2,RestBody ),
		compile_body_1( Rest,RestBody,
				Depth,
				NewPath,PathOut,
				SubProof,Unifier)
	    )
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate compile_body_2/7(+Literals, -Body, +DepthIn, +DepthOut, +Path,
			    -Proof, Unifier).

\PL*/
compile_body_2([],true,D,D,P,P,[],_).
compile_body_2([literal(Literal,Index)|Rest],
	     Body,
	     Din, Dout,
	     Path,PathOut,
	     [Proof|SubProof],
	     Unifier) :-
	make_functor_from_template(Literal,Functor,_,_),
	proof_steps(Functor,Index,Candidates),
	( Candidates = [] ->
	    Body     = fail,
	    SubProof = fail,
	    PathOut  = Path,
	    Dout     = Din
	;
	    negate_literal(Literal,Lit),
	    need_proc(Lit),
	    make_literal(Lit,Candidates,'InOut'(Din,D),Path,Proof,Unifier,
			 Head,[]),
	    term_variables(Lit,Vars),
	    ( is_option('ProCom:lemma') ->
		make_internal_rep(Lit,IR),
		Head2 = ( literal_wrapper(Head,Lit,Depth,Path,Proof,Vars),
			  lemma(IR,Path,NewPath) )
	    ;	Head2 = literal_wrapper(Head,Lit,Depth,Path,Proof,Vars),
		Path  = NewPath
	    ),
	    ( Rest = [] -> 
		Body	 = Head2,
		PathOut	 = Path,
		SubProof = [],
		Dout	 = D
	    ;	
		Body = ( Head2,RestBody ),
		compile_body_2( Rest,RestBody,
				D,Dout,
				NewPath,PathOut,
				SubProof,Unifier)
	    )
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
