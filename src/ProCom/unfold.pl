%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: unfold.pl,v 1.8 1995/01/27 13:45:38 gerd Exp $
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

:- module_interface(unfold). /*%-----------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.8 $}

This module applies unfolding to a set of clauses.

\PL*/
:- export apply_definitions/3,
	  apply_unsorted_definitions/3.
:- begin_module(unfold).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:-	lib(unify).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


\Predicate apply_unsorted_definitions/3(Clauses,Rules,Result).





\begin{verbatim}
  Clause = (h(X,Y,Z) :- p(X),q(17,Y),r(Z),q(Z,Z)),
  apply_unsorted_definitions([Clause],
			     [ (q(A,a) :- s(A,a(A))),
			       (q(A,o) :- t(A,o(A))),
			       (rr(_)  :- false)
			     ],
			     NewClause)
\end{verbatim}


\PL*/
apply_unsorted_definitions(Clauses,RuleClauses,Result) :-
	( setof(FA,get_pattern(RuleClauses,FA),Functors) ->
	    findall(def(Pattern,Rules),
		    ( member(F/A,Functors),
		      functor(Pattern,F,A),
		      findall((Pattern:-Body),
			      member((Pattern:-Body),RuleClauses),
			      Rules)		  
		    ),
		    Defs),
	    apply_definitions(Clauses,Defs,Result)
	;   Result = Clauses
	).

get_pattern(Rules,F/A) :-
	member((Head:-_),Rules),
	functor(Head,F,A).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate apply_definitions/3().


\begin{verbatim}
  Clause = (h(X,Y,Z) :- p(X),q(17,Y),r(Z),q(Z,Z)),
  apply_definitions([Clause],
		    [def(q(_,_),
			 [ (q(A,a) :- s(A,a(A))),
			   (q(A,o) :- t(A,o(A)))
			 ]),
		     def(rr(_),[])
		    ],
		    NewClause)
\end{verbatim}

\PL*/
apply_definitions(Clauses,Defs,Result) :-
	setval('unfold:changed',false),
	apply_definitions_once(Clauses,Defs,RESULT),
	( getval('unfold:changed',false) ->
	    Result = RESULT
	;
	    apply_definitions(RESULT,Defs,Result),
	    setval('unfold:changed',true)	
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate apply_definitions_once/3().

\PL*/
apply_definitions_once(Clauses,[],Clauses).
apply_definitions_once(Clauses,[def(Pattern,Rules)|Defs],Result) :-
	findall(R,
		( member(Clause,Clauses),
		  apply_one_definition(Clause,Pattern,Rule,R,F),
		  ( var(F) ->
		      true
		  ;   member(Rule,Rules),
		      setval('unfold:changed',true)
		  )
		),
		Res),
	apply_definitions_once(Res,Defs,Result).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate apply_def/5(+Clause, +Pattern, +Rule, ?NewClause, -Flag).

\begin{verbatim}
  apply_one_definition((h(X,Y,Z):-p(X),q(17,Y),r(Z)),
	    q(_,_),
	    (q(A,aha):-s(A,A)),
	    NewClause)
\end{verbatim}

\PL*/
apply_one_definition((Head:-Body),Pattern,(From:-To),(Head:-NewB),true) :-
	get_literal(Body,Pattern,To,NewB),
	!,
	From = Pattern.
apply_one_definition((Head:-Body),Pattern,(From:-To),(Head:-NewB),true) :-
	get_literal(Body,(\+ Pattern),To,NewB),
	!,
	From = Pattern.
apply_one_definition((Head:-Body),Pattern,(From:-To),(Head:-NewB),true) :-
	get_literal(Body,not(Pattern),To,NewB),
	!,
	From = Pattern.
apply_one_definition(Clause,_,_,Clause,_).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate get_literal/4(+Body, +Literal, ?NewLiteral, ?NewBody).

\begin{verbatim}
  get_literal((r(X),p(X,X),p(Z,Y)),
	      p(X1,f(X1)),
	      a(X1),
	      M)
\end{verbatim}

\PL*/
get_literal((A,B),Literal,NewLit,NewAB) :-
	!,
	(   get_literal(A,Literal,NewLit,NewA),
	    NewAB = (NewA,B)
	;   get_literal(B,Literal,NewLit,NewB),
	    NewAB = (A,NewB)
	).
get_literal(L,Literal,V,V) :-
	unify(L,Literal).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
