%%*****************************************************************************
%% $Id: connection_graph.pl,v 1.8 1995/03/13 19:58:01 gerd Exp $
%%*****************************************************************************
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

:- module_interface(connection_graph). /*%-------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.8 $}

\subsection{Building the Connection Graph}

The connection graph is stored in the Prolog data base as facts for the
predicate |Connection/2|. The fact |Connection(A,B)| denotes a directed
connection from |A| to |B|.

Given is a database containing facts for |Clause/3|, |HashClause/3| and
|GoalClause/2|. From this data base the corresponding entries for the facts
|Connection/2| are created.

\PL*/
:- export connection_graph/0,
	  'LiteralDegree'/2.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- begin_module(connection_graph).
info(red,"$Revision: 1.8 $","Builting the reachability graph.").
info(connection_graph,"$Revision: 1.8 $","Builting the reachability graph.").
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:-	lib(lists).
:-	lib(matrix),
	lib(literal),
	lib(message),
	lib(options).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- define_option find_connections	  = on.
:- define_option remove_unreached_clauses = on.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- define_option find_all_connections	  = off.
:- define_option connect_weak_unifiable	  = on.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- dynamic('LiteralDegree'/2).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate connection_graph/0().

This predicate creates the reachability graph as described in
\cite{neugebauer:reachability}. This means that the extension procedure is
hard coded into this module. For other calculi it might not be appropriate to
turn on this feature.

\PL*/
connection_graph :-
	retract_all('LiteralDegree'(_,_)),
	is_option(find_connections,Type),
	(  empty_option(Type) -> 
	    true
	;   connection_graph(Type),
	    ( is_option(remove_unreached_clauses) ->
		remove_unreached_clauses
	    ;	true
	    )
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate connection_graph/1(+Type).

The reachability graph can be constructed in several variants. |Type| denotes
the type to be used.

\PL*/
connection_graph(off) :- !.
connection_graph([])  :- !.
connection_graph(no)  :- !.
connection_graph(goals_weak) :-
	!,
	findall(LI,('GoalClause'(C),'ClauseLiteral'(C,LI)),Literals),
	connect_weak(Literals).
connection_graph(all_weak) :-
	!,
	findall(LI,'ClauseLiteral'(_,LI),Literals),
	connect_weak(Literals).
connection_graph(_) :-
	is_option(find_all_connections),
	!,
	findall(LI,'ClauseLiteral'(_,LI),Literals),
	( is_option(connect_weak_unifiable) ->
	    connect_literals(Literals)
	;   connect_literals_all(Literals)
	).
connection_graph(_) :-
	findall(Index,'GoalClause'(Index),GoalClauses),
	( is_option(connect_weak_unifiable) ->
	    connect_clauses(GoalClauses)
	;   connect_clauses_all(GoalClauses)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate connect_weak/1(+List).

\PL*/
:- mode connect_weak(+).
connect_weak(List) :-
	( setof(E,connect_extend_weak(List,E),Front) ->
	    connect_weak(Front)
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate connect_extend_weak/2(+List, -Index).

\PL*/
:- mode connect_extend_weak(+,-).
connect_extend_weak([CL|_],Index) :-
	'Contrapositive'(Lit,_,CL),
	( Lit = (--LIT) ->
	    'Contrapositive'((++LIT),List,Hit)
	; Lit = (++LIT) ->
	    'Contrapositive'((--LIT),List,Hit)
	),
	add_connection(CL,Hit),
	member(literal(_,Index),List),
	\+ 'LiteralDegree'(Index,_),
	assert('LiteralDegree'(Index,_)).
connect_extend_weak([_|T],E) :-
	connect_extend_weak(T,E).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate connect_clauses/1(+Clauses).

The argument |Clauses| is a list of indices of clauses. All connections
starting from literals in the list |Clauses| are taken into account. Thus the
algorithm is to collect all literal indices and connect those literals using
|connect_literals/2|.

\PL*/
:- mode connect_clauses(++).
connect_clauses([]).
connect_clauses([ClauseIndex|Rest]) :-
	findall(Index, 'ClauseLiteral'(ClauseIndex,Index), Literals),
	connect_literals(Literals),
	connect_clauses(Rest).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate connect_literals/1(+Literals).

Given a list |Literals| of literals find all outgoing links and follow them
recursively.

\PL*/
:- mode connect_literals(++).
connect_literals([]) .
connect_literals([LitIndex|Literals]) :-
	'LiteralDegree'(LitIndex,_),
	!,
	connect_literals(Literals).
connect_literals([CL|Literals]) :-
	'Contrapositive'(Lit,_,CL),
	negate_literal(Lit,Entry),
	findall(Index,
		'Contrapositive'(Entry,_,Index),
		Targets),
	length(Targets,Degree),
	assert('LiteralDegree'(CL,Degree)),
	connect_one_literal(Targets,CL),
	connect_literals(Literals).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate connect_one_literal/2(+Literals, +LiteralIndex).

Given a literal index |LiteralIndex| and a list of indices of
complementary literals. If an arc already exists we are ready.
Otherwise we have to consider all literals in the new clause.

\PL*/
:- mode connect_one_literal(++,++).
connect_one_literal([],_).
connect_one_literal([Hit|Targets],CI) :-
	( 'Connection'(CI,Hit) ->
	    true
	;   add_connection(CI,Hit),
	    'ClauseLiteral'(HitClause,Hit),
	    findall(Lit,
		    ( 'ClauseLiteral'(HitClause,Lit), Lit \== Hit ),
		    Literals),
	    connect_literals(Literals)
	),
	connect_one_literal(Targets,CI).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate connect_clauses_all/1(+Clauses).

The argument |Clauses| is a list of indices of clauses. All connections
starting from literals in the list |Clauses| are taken into account. Thus the
algorithm is to collect all literal indices and connect those literals using
|connect_literals/2|.

\PL*/
:- mode connect_clauses_all(++).
connect_clauses_all([]).
connect_clauses_all([ClauseIndex|Rest]) :-
	findall(Index,
		'ClauseLiteral'(ClauseIndex,Index),
		Literals),
	connect_literals_all(Literals),
	connect_clauses_all(Rest).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate connect_literals_all/1(+Literals).

Given a list |Literals| of literals find all outgoing links and follow them
recursively.

\PL*/
:- mode connect_literals_all(++).
connect_literals_all([]) .
connect_literals_all([LitIndex|Literals]) :-
	'LiteralDegree'(LitIndex,_),
	!,
	connect_literals_all(Literals).
connect_literals_all([CL|Literals]) :-
	'Contrapositive'(Lit,_,CL),
	literal_functor(Lit,F,A),
	literal_neg_sign(Lit,NegSign),
	make_literal(F,A,NegSign,Entry),
	findall(Index,
		'Contrapositive'(Entry,_,Index),
		Targets),
	length(Targets,Degree),
	assert('LiteralDegree'(CL,Degree)),
	connect_one_literal_all(Targets,CL),
	connect_literals_all(Literals).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate connect_one_literal_all/2(+Literals, +LiteralIndex).

Given a literal index |LiteralIndex| and a list of indices of
complementary literals. If an arc already exists we are ready.
Otherwise we have to consider all literals in the new clause.

\PL*/
:- mode connect_one_literal_all(++,++).
connect_one_literal_all([],_).
connect_one_literal_all([Hit|Targets],CI) :-
	( 'Connection'(CI,Hit) ->
	    true
	;   add_connection(CI,Hit),
	    'ClauseLiteral'(HitClause,Hit),
	    findall(Lit,
		    ( 'ClauseLiteral'(HitClause,Lit), Lit \== Hit ),
		    Literals),
	    connect_literals_all(Literals)
	),
	connect_one_literal_all(Targets,CI).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\subsection{Working on the Connection Graph}


\Predicate remove_unreached_clauses/0().


\PL*/
remove_unreached_clauses :-
	setval('CG removed clauses',0),
	(   'ClauseLiteral'(_,Idx),
	    \+ 'Connection'(_,Idx),
	    delete_contrapositive(Idx),
	    incval('CG removed clauses'),
	    fail
	; is_option(verbose) ->
	    getval('CG removed clauses',No),
	    msg("--- ",No," unreached contrapositives removed.\n")
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- skipped connection_graph/0,
	   'LiteralDegree'/2.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
