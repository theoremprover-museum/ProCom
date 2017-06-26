%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% $Id: p_goal.pl,v 1.21 1995/05/15 19:58:27 gerd Exp $
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

:- module_interface(p_goal). /*%-----------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.21 $}

\PL*/
:- export compile_goal/0. 
:- begin_module(p_goal).

:-	lib(literal),
	lib(add_arg),
	lib(matrix),
	lib(options),
	lib(varlist),
	lib(p_put),
	lib(p_body),
	lib(p_reorder),
	lib(linker),
	lib(optimize),
	lib(numbervars).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate compile_goal/0().

This predicate performs all actions necessary to compile all clauses
marked as goals and write the resulting clause to the program. The
main task is performs by the pipe |goals/3|.

Two predicates are provided in this step.
\begin{description}
  \item [goal/1]\ \\
	This predicate initiates the proof process. It takes one argument.
	This argument is interpreted as file name and given to the predicate
	|show_proof/2| to store the proof term in this file.
  \item [goal/0]\ \\
	This predicate acts like |goal/1| where the file argument is the empty
	list. The empty list is used to indicate that no proof log is required.
\end{description}

\PL*/
compile_goal :-
	( getval('ProCom:depth_pass_through',on) ->
	    Depth = 'InOut'(D,_)
	;   Depth = D
	),
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
	findall(Index,'GoalClause'(Index),Goals),
	( Goals = [] ->
	    require_predicate([no_goal/0]),
	    puts("*** No goal left."),
	    GOAL_CLAUSES = no_goal
	; Goals = [Goal] ->
	    puts("Goal clause: ",Goal),
	    compile_goals(Goals,Depth,Path,ProofFile,GOAL_CLAUSES)
	;
	    reorder(Goals,
		    'ProCom:reorder_goal_clauses',
		    compare_goal_clauses,
		    SortedGoals),
	    puts("Goal clauses: ",SortedGoals),
	    compile_goals(SortedGoals,Depth,Path,ProofFile,GOAL_CLAUSES)
	),
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
	is_option('ProCom:init_goal_list',IGList),
	prepare_goal_list(IGList,INIT_GOAL_LIST),
	is_option('ProCom:init_level_list',ILList),
	prepare_goal_list(ILList,INIT_LEVEL_LIST,D),
	( is_option('ProCom:trace') ->
	    require_predicate([show_depth/1]),
	    SHOW_DEPTH = show_depth(D)
	;   SHOW_DEPTH = true
	),
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
	is_option('ProCom:proof_limit',ProofLimit),
	( integer(ProofLimit) ->
	    require_predicate([init_proof_limit/0,check_proof_limit/1]),
	    PROOF_LIMIT	     = check_proof_limit(ProofLimit),
	    INIT_PROOF_LIMIT = init_proof_limit
	; ProofLimit == interactive ->
	    require_predicate([init_proof_limit/0,more/1]),
	    PROOF_LIMIT	     = more(Depth),
	    INIT_PROOF_LIMIT = init_proof_limit
	; ProofLimit == all ->
	    require_predicate([init_proof_limit/0]),
	    PROOF_LIMIT	     = fail ,
	    INIT_PROOF_LIMIT = init_proof_limit
	;   PROOF_LIMIT	     = true,
	    INIT_PROOF_LIMIT = true
	),
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
	require_predicate([set_time/0,
			   set_depth_bound/1,
			   reset_print_time/1,
			   no_more_solutions/0]),
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
	put_clause((:- dynamic(log_file/1))),
	require_predicate([set_global/2]),
	put_optimized_clause([
	    ( goal(ProofFile) :- 
		  set_global(log_file,ProofFile),
		  empty_path(Path),
		  INIT_PROOF_LIMIT,
		  (   set_time,
		      INIT_GOAL_LIST,
		      set_depth_bound(D),
		      SHOW_DEPTH,
		      INIT_LEVEL_LIST,
		      GOAL_CLAUSES,
		      PROOF_LIMIT
		  ;   reset_print_time([]),
		      no_more_solutions
		  )
	    ),
	    ( goal :- goal([]) )]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate compile_goals/3(+GoalList, +Depth, Path, ProofFile, -GoalTerm).

To compile a list |GoalList| of goals we have to separate three cases.
If the list is empty it simply means to fail. If there is only one
goal left we compile it. Otherwise we take the first goal, compile it
and recursively treat the remaining goals.

\PL*/
:- mode compile_goals(+,?,?,?,-).

compile_goals([],_,_,_,fail).
compile_goals([Goal|GoalList],Depth,Path,ProofFile,GoalTerm) :-
	compile_one_goal(Goal,Depth,Path,_,ProofFile,OneGoalTerm),
	( GoalList = [] ->
	    GoalTerm = OneGoalTerm
	;
	    GoalTerm = (OneGoalTerm;GoalListTerm),
	    compile_goals(GoalList,Depth,Path,ProofFile,GoalListTerm)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate compile_one_goal/6(+GoalIndex, +Depth, Path, Proof, ProofFile,
	-ProofGoalTerm).

To compile a single goal we return the conjunction of its literals.
Optionally a library predicate is appended which is intended to show
the variable bindings.

\PL*/
compile_one_goal(GoalIndex,Depth,Path,Proof,ProofFile,Goal) :-
	once('Clause'(GoalIndex,Body1)),
	reorder(Body1,
		'ProCom:reorder_goal',
		compare_goal_literals,
		Body),
	varlist(Body,NumberedBody,Vars),
	compile_body(Body,Goal_Literals,Depth,Path,_,ProofList,_),
	Proof = proof(GoalIndex,ProofList),

	is_option('ProCom:post_goal_list',PostGoalList),
	prepare_goal_list(PostGoalList,POST_GOAL_LIST,Vars,Proof),

	( ( is_option('ProCom:show_result'),
	    functor(Vars,'.',2) ) ->
	    require_predicate(list_bindings/3),
	    strip_body(NumberedBody,NoBody),
	    LIST_BINDINGS = list_bindings(GoalIndex,NoBody,Vars)
	;   LIST_BINDINGS = true,
	    NoBody = []
	),
	is_option('ProCom:time_limit',TimeLimit),
	require_predicate([set_timeout/1,
			   reset_print_time/1,
			   save_bindings/4,
			   save_proof/2]),

	( is_option('ProCom:proof'),
	  is_option('ProCom:show_proof')  ->
	    require_predicate(show_proof/1),
	    SHOW_PROOF = show_proof(Proof)
	;   SHOW_PROOF = true
	),
	Goal = (set_timeout(TimeLimit),
		Goal_Literals,
		POST_GOAL_LIST,
		reset_timeout,
		reset_print_time(ProofFile),
		save_bindings(GoalIndex,NoBody,Vars,ProofFile),
		save_proof(Proof,ProofFile),
		LIST_BINDINGS,
		SHOW_PROOF).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate strip_body/2(LiteralList, Conjunction).

This predicate translates a list of literal into a conjuntion of goals. Just
the predicate part of the literal is preserved.

\PL*/
strip_body([],true).
strip_body([literal(P,_)|Rest],Result) :-
	( Rest == [] ->
	    Result = P
	;
	    Result = (P,StrippedRest),
	    strip_body(Rest,StrippedRest)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate prepare_goal_list/2().

\PL*/
prepare_goal_list([],true).
prepare_goal_list([Literal|Rest],Result) :-
	( Rest == [] ->
	    Result = Literal
	;
	    Result = (Literal,StrippedRest),
	    prepare_goal_list(Rest,StrippedRest)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate prepare_goal_list/3().

\PL*/
prepare_goal_list([],true,_).
prepare_goal_list([P|Rest],Result,A1) :-
	add_args(P,Literal,A1),
	( Rest == [] ->
	    Result = Literal
	;
	    Result = (Literal,StrippedRest),
	    prepare_goal_list(Rest,StrippedRest,A1)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate prepare_goal_list/4().

\PL*/
prepare_goal_list([],true,_,_).
prepare_goal_list([P|Rest],Result,A1,A2) :-
	add_args(P,Literal,A1,A2),
	( Rest == [] ->
	    Result = Literal
	;
	    Result = (Literal,StrippedRest),
	    prepare_goal_list(Rest,StrippedRest,A1,A2)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
