%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: capricore.pl,v 1.41 1995/07/03 11:35:12 gerd Exp gerd $
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

:- module_interface(capricore). /*%--------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.41 $}

This module provides the core routines for the \CaPrI{} system. This includes
the interpreter for the \CaPrI{} descriptor files.

This module is usually used automatically when the file {\sf capri.pl} is
loaded into a \CaPrI{} module.


\PL*/
:- tool('CaPrI Define Prover'/0,'CaPrI Define Prover'/1).
:- export 'CaPrI Description Interpreter'/1,
	  'CaPrI Define Prover'/1,
	  'CaPrI Check'/1.
:- op(1180,fx,descriptor).
:- begin_module(capricore).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Use the library sorts from the \eclipse{} libraries.
\PL*/
:-	lib(sorts).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Load some libraries from the \ProTop{} area.
\PL*/
:-	lib(unify),
	lib(add_arg),
	lib(message),
	lib(options),
	lib(literal),
	lib(matrix),
	lib(linker),
	lib(optimize),
	lib(unfold).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Load all required modules from the \ProCom{} area.
\PL*/
:-	lib(p_options),
	lib(p_predicate),
	lib(p_reorder),
	lib(p_driver),
	lib(p_body),
	lib(p_goal),
	lib(p_info),
	lib(p_put).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate CaPrI Define Prover/1(+Name).

This predicate provides an interface to the description interpreter.  It is
called in the wrapper file ({\sf capri.pl}). A new predicate is asserted which
is identical to the current module name. This new predicate calls the
description interpreter with its name as argument. Thus the description
interpreter knows where it has been called from.

This predicate is not used itself. Instead it is defined as a {\em tool}. This
is done in the interface section of this module.  With this technique
\eclipse{} takes care of the argument and inserts the appropriate module name.

\PL*/
'CaPrI Define Prover'(Name) :-
	concat_string(["The CaPrI module ",Name],Info),
	call(assert((info(capri,Version,Info) :- info(Version,Info),!)),Name),
	call(assert(info(capri,"???",Info)),Name),
	call(assert((Name :- 'CaPrI Description Interpreter'(Name))),Name).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate CaPrI Check/1(+Module).

This predicate tries to apply some consistency checks which are omitted during
the compilation for efficiency reasons.	 The consistency checks are defined as
inconsistencies in the predicate |check/1|.

\PL*/
'CaPrI Check'(Module) :-
	msg("Checking ..."),
	(   call((descriptor Desc),Module),
	    check(Desc),
	    fail
	;   true
	),
	msg("Checking ... done").
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate check/1(+Desc).

This predicate performs consistency checks. Each clause tests one possible
inconsistency and prints an error message if one is found. No clause needs to
fail since this predicate is called in a failure driven loop in |CaPrI
Check/1|.
\PL*/
check(Desc) :-
	var(Desc),
	!,
	err("*** ProCom Error: Unspecified descriptor encountered (var).").
check(Desc) :-
	desc_get(Desc,name(N1)),
	desc_get(Desc,name(N2)),
	\+ (N1 = N2),
	err("*** ProCom Error: Incompatible names in a descriptor: ",
	    N1,", ",N2).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate desc_get/2(+Desc, ?Item).

Unify |Item| with any matching item in the descriptor body |Desc|.

\PL*/
desc_get(X,X).
desc_get((X,_),X).
desc_get((_,Rest),X) :-
	desc_get(Rest,X).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate CaPrI Description Interpreter/1(+Module).

This predicate performs all actions necessary to interpret a \CaPrI{}
description module. 

Since I unfolded some definitions this predicate seems to be a little
bit lengthy. Nevertheless I hope things are not too weird.

\PL*/
'CaPrI Description Interpreter'(Module) :-
	setval('CaPrI aux counter',1),
	setval('CaPrI module name',Module),
	put_boxed('ProCom:verbose',
		  "ProCom: PROLOG COMPILER FOR CLAUSE LOGIC"),
	apply_options(FunctorList,Module),
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The linker is initialized. This is done mainly from several
options. The value of those options --- containing two colons --- are
made known to the linker.

\PL*/
	initialize_linker,
	( is_option('ProCom:occurs_check') ->
	    add_link_files_from_options(['ProCom::unify'])
	;   add_link_files_from_options(['ProCom::unify_simple'])
	),
	add_link_files_from_options([   'ProCom::member',
				        'ProCom::path',
				        'ProCom::literal',
					'ProCom::lemma',
					'ProCom::show_result',
					'ProCom::proof',
					'ProCom::more',
					'ProCom::proof_limit',
					'ProCom::timing',
					'ProCom::trace'
				    ]),
	add_link_files('setq.pl'),
	require_predicate(literal_wrapper/6),
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Next the linker instructions from the \CaPrI{} module are evaluated.
\begin{description}
  \item [library\_file({\em File})]\ \\
	Each such instruction implies that {\em File}\/ is made known to the
	linker. This is done by the predicate |add_link_files/1|.
  \item [library\_path({\em Path})]\ \\
	Each such instruction implies that {\em Path}\/ is made known to the
	linker's search path. This is done by the predicate |add_link_path/1|.
  \item [require\_predicate({\em Spec})]\ \\
	Each such instruction implies that {\em Spec}\/ is tried to be linked.
	This is done by the predicate |require_predicate/1|.
  \item [provide\_definition({\em Spec})]\ \\
	Each such instruction implies that {\em Spec}\/ is a definition to be
	used by the linker. This is done by the predicate
	|provide_definition/2|.
\end{description}

Since the linker predicates work with side effects, failure driven
loops can be used to get all instructions and evaluate them in turn.
\PL*/
	(   call(library_file(File),Module),
	    add_link_files(File),
	    fail
	;   call(library_path(Path),Module),
	    add_link_path(Path),
	    fail
	;   call(require_predicate(Spec),Module),
	    require_predicate(Spec),
	    fail
	;   call(provide_definition(Spec),Module),
	    concat_atom(["@",Module,"@"],Id),
	    provide_definition(Spec,Id),
	    fail
	;
	    true
	),
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
	( is_option('ProCom:ancestor_pruning') ->
	    use_reductions(all)
	;   true
	),
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If a module is requested them we take the code from the library
|ProCom::module| and link it immediately.

\PL*/
	( is_option('ProCom:module') ->
	    immediate_link_from_option('ProCom::module')
	;   true
	),
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Now we have to initialize the search strategy. The search strategies are
defined and loaded in the module {\sf procom}. The implicit assumption is that
search strategies satisfy a naming convention. Suppose that the search
strategy {\em Search} is selected by the option |search|. Then the associated
initialization predicate is taken from the module {\sf p\_\_}{\em Search}. The
initialization predicate is named |init_search/0|.

If no appropriate search module is loaded then the strategy
|iterative_deepening(1,1,1)| is used instead. In fact this assumes that this
strategy is loaded.

\PL*/
	is_option(search,Search),
	functor(Search,S,_),
	concat_atom(['p__',S],SearchFile),
	( current_module(SearchFile) ->
	    ( call(init_search,SearchFile) ->
		true
	    ;	err("*** ProCom. Initialization failed for search strategy ",
	            Search,
		    "\n--- Failure ignored. Trying to continue anyway.")
	    )
	;   err("*** ProCom. Invalid search strategy selected: ",Search,
		"\n--- Using iterative_deepening(1,1,1) instead."),
	    set_option(search=iterative_deepening(1,1,1)),
	    call(init_search,'p__iterative_deepening')
	),
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Now the options should have their final values. Thus we can print them for the
user and debugging purposes.

\PL*/
	( is_option('ProCom:verbose') ->
	    put_boxed('ProCom:verbose',"Options:"),
	    puts("Options:"), 
	    (	is_option(Name,Value),
		puts(Name," = ",Value),
		fail
	    ;	true
	    )
	;   true
	),
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Add any initialization code immediatly. This is done by the linker by copying
in the libraries specified in |ProCom::immediate_link|.

\PL*/
	( is_option('ProCom:link',off) ->
	    true
	;   immediate_link_from_option('ProCom::immediate_link')
	),
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
	put_boxed('ProCom:verbose',"Program Segment"),
	
	initialize_proof_steps(FunctorList,Module),
	is_option('ProCom:extra_procedures',ProcList),
	init_proc(ProcList),
	repeat,
	( unprocessed_proc(F,A) ->
	    once(compile_procedure(F,A,Module)),
	    fail
	;   true
	),
	!,
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Someone may need the contrapositives at run time. For this purpose we add them
upon request. A contrapositive is stored as a fact
\[
	\mbox{\tt contrapositive}(L,[B_1,\ldots,B_n]).
\]
where $L$\/ and $B_i$\/ are literals of the form $literal(Lit,Index)$.
\PL*/
	( is_option('ProCom:add_contrapositives') ->
	    put_boxed('ProCom:verbose',
		      "Contrapositives (just in case we need them)"),
	    (	'Contrapositive'(A_,B_,C_),
		put_clause(contrapositive(literal(A_,C_),B_)),
		fail
	    ;	true
	    )
	;   true
	),
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The goal has to be compiled special.

\PL*/
	put_boxed('ProCom:verbose',"Goal Segment"),
	compile_goal,
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Finally we urge the linker to do its work.

\PL*/
	is_option('ProCom:link',LinkType),
	( LinkType \== off ->
	    put_boxed('ProCom:verbose',"Runtime System"),
	    link_runtime_system
	;   true
	),
	is_option('ProCom:trace',Trace),
	(   member(T,Trace),
	    ( T = trace ->
		put_clause((:-'Trace'))
	    ; T = notrace ->
		put_clause((:-'Notrace'))
	    ; T = spy(FA) ->
		put_clause((:-'Spy'(FA)))
	    ),
	    fail
	;   true
	),
	immediate_link_from_option('ProCom::post_link').
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate compile_procedure/3(+Functor, +Arity, +Module).

\PL*/
compile_procedure(F,A,Module) :-
	make_template(F,A,Template),
	put_report('ProCom:report_actions',"%% Compiling procedure ",F,"/",A),
	( is_option('ProCom:dynamic_reordering') ->
	    make_driver(F,A,Driver),
	    put_report('ProCom:report_actions',
		       " Driver for dynamic reordering generated."),
	    MainProc = [Driver|Proc]
	;   make_driver_clauses(F,A,Driver-Proc),
	    put_report('ProCom:report_actions'," Static driver generated."),
	    MainProc = Driver
	),

	findall(Clause-Aux,
		( call((descriptor Desc),Module),
		  ( once(descriptor_macro(Desc,Desc2)) ->
		      compile_desc(Template,Desc2,Module,Clause,Aux)
		  ;   compile_desc(Template,Desc,Module,Clause,Aux)
		  )
		),
		Proc_Aux),
	collect_aux(Proc_Aux,Proc,AuxProc),
	( number_clauses(Proc,1) ->true;true),
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
	reorder(MainProc,
		'ProCom:reorder_prolog_clauses',
		compare_prolog_clauses,
		MainProcSorted),
	put_optimized_clause(MainProcSorted),
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
	reorder(AuxProc,
		'ProCom:reorder_aux_clauses',
		compare_aux_clauses,
		AuxProcSorted),
	put_optimized_clause(AuxProcSorted).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate number_clauses/2(+Clause, +Number).

\PL*/
number_clauses([],_).
number_clauses([(_:-B)|T],No) :-
	number_clauses_1(B,No),
	No1 is No+1,
	number_clauses(T,No1).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate number_clauses_1/2(+Term, +Number).

\PL*/
number_clauses_1((A,B),No) :-
	!,
	number_clauses_1(A,No),
	number_clauses_1(B,No).
number_clauses_1(check_depth_bound(_,_,No),No)	 :- !.
number_clauses_1(check_depth_bound(_,_,No,_),No) :- !.
number_clauses_1(_,_).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate collec_aux/3(+Proc_Aus_list,ProcList,AuxList).

Take a list with elements of the form {\em Proc |-| Aux} and produce two lists
|ProcList| which contains each |Proc| and |AuxList| which contains each |Aux|.

\PL*/
:- mode collect_aux(+,?,?).
collect_aux([],[],[]).
collect_aux([P-A|Rest],Proc,Aux) :-
	append(P,RestProc,Proc),
	append(A,RestAux,Aux),
	collect_aux(Rest,RestProc,RestAux).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate compile_desc/5(+Template, +Desc, +Module, Clauses, AuxDefs).

\PL*/
:- mode compile_desc(+,+,++,?,?).
compile_desc(Goal,Desc,Module,Clauses,AuxDefs) :-
	( desc_get(Desc,proof(ProofInfo)) -> true ; true);
	( desc_get(Desc,name(Name)) ->
	    ID = Name
	; nonvar(ProofInfo) ->
	    ID = ProofInfo
	;   ID = Desc
	),
	put_report('ProCom:report_actions',
		   "\n%--- Considering the descriptor: ",ID),
	( getval('ProCom:depth_pass_through',on) ->
	    Depth  = 'InOut'(Di,Do),
	    CHECK_DEPTH = check_depth_bound(Di,D2,CTR,Weight)
	;   CHECK_DEPTH = check_depth_bound(Depth,Di,CTR,Weight),
	    D2 = Do,
	    D2 = Di
	),
	extract_diagonal(Desc,Diag,[],PSteps,[]),
	put_report('ProCom:report_actions',""),
	negate_literal(Goal,NegGoal),
	( get_diagonal_goal(Diag,NegGoal) ->
	    true
	;   put_report('ProCom:report_actions',
		       "$$ Goal does not match. Diagonal rejected."),
	    fail
	),
	make_literal(Goal,__Name,Depth,Path,ProofVar,_,Head,[]),
	compile_spec(Diag,
		     Module,
		     D2,Do,
		     Path2,
		     _,
		     _,
		     SubProofs,
		     Subgoals,
		     Aux,
		     Weight),
	( Subgoals = fail ->
	    Clause  = (Head:-fail),
	    AuxDefs = []
	;
	    ( is_option('ProCom:dynamic_reordering') ->
		Check = true,
		Depth = D2,
		Path  = Path2
	    ;	
		make_internal_rep(--Goal,IR),
		( is_option('ProCom:automatic_put_on_path') ->
		    Check = ( CHECK_DEPTH, put_on_path(IR,Path,Path2) )
		;   Check = CHECK_DEPTH,
		    Path  = Path2
		)
	    ),
	    ( nonvar(Name) ->
		ProofSteps =.. [by,name(Name)|PSteps]
	    ;	ProofSteps =.. [by|PSteps]
	    ),
	    ( var(ProofInfo) ->
		ProofTerm =.. [prove,NegGoal,ProofSteps|SubProofs]
	    ;   ProofTerm =.. [prove,NegGoal,ProofSteps,ProofInfo|SubProofs]
	    ),
	    
	    Clause = (Head :- Check, Subgoals, ProofVar=ProofTerm),
	    ( (Aux \== [],is_option('ProCom:expand_aux' )) ->
		apply_unsorted_definitions([Clause],Aux,Clauses),
		AuxDefs = []
	    ;	Clauses = [Clause],
		AuxDefs = Aux
	    )
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate compile_spec/11(+Spec,
			  +Module,
			  ?DepthIn, ?DepthOut,
			  ?PathIn, ?PathOut,
			  ?Unifier,
			  ?Proof,
			  -Code,
			  -AuxPreds).

This predicate compiles a single specification as well as a list of
specifications |Spec|.

The first clause catches the case of an empty list of specifications. Nothing
special is done. The path |Path| and the depth |Depth| are passed through. No
code is generated; no auxiliary definitions generated; no proof term build.

\PL*/
:- mode compile_spec(+,++,?,?,?,?,?,?,?,?).
compile_spec([],_,Depth,Depth,Path,Path,_,[],true,[],0) :- !.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The next case recursively goes through a list of specifications. Special care
is taken to treat the case of a failing subgoal. In this case the following
subgoals are not considered for producing code but only generated to see if
the compilation is successful.

In fact it is also checked if the rest list is empty. Thus we can avoid to
call the base case of the recursion and produce an unnecessary |true|.

\PL*/
compile_spec([First|Rest],Module,DepthIn,DepthOut,Path,PathOut,Unifier,Proof,
	     Code,Aux,Weight) :-
	!,
	put_report('ProCom:report_actions',
		   " Trying description directive: ",First),
	compile_spec(First,Module,
		     DepthIn,DepthMid,
		     Path,PathMid,
		     Unifier,Proof1,
		     Code1,Aux1,Weight1),
	put_report('ProCom:report_actions',
		   " Code produced: ",Code1),
	( Code1 = fail ->
	    Proof   = [],
	    Code    = fail,
	    Aux	    = Aux1,
	    Weight  = 0,
	    compile_spec(Rest,Module,
			 DepthMid,DepthOut,
			 PathMid,PathOut,
			 Unifier,_,_,_,_)
	;
	    compile_spec(Rest,Module,
			 DepthMid,DepthOut,
			 PathMid,PathOut,
			 Unifier,Proof2,
			 Code2,Aux2,Weight2),
	    ( Code2 = fail ->
		Proof  = [],
		Code   = fail,
		Aux    = [],
		Weight = Weight1
	    ;	( Proof1 == [] ->
		    Proof = Proof2
		;   Proof = [Proof1|Proof2]
		),
		( Code1 = true ->
		    Code = Code2
		; Code2 = true ->
		    Code = Code1
		;   Code  = (Code1,Code2)
		),
		append(Aux1,Aux2,Aux),
		add(Weight1,Weight2,Weight)
	    )
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Get instructions are executed at once. This is done in the procedure
|evaluate_get/4|.

\PL*/
compile_spec(get(Type,Arg),_,Depth,Depth,Path,Path,_,[],true,[],Weight) :-
	!,
	evaluate_get(Type,Arg,Depth,Path),
	weight(get,Weight).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
compile_spec(is_on_path(Literal),_,Depth,Depth,Path,Path,_,[],
		is_on_path(Literal,Path),[],Weight) :-
	!,
	weight(is_on_path,Weight).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The next instruction puts a literal on the path. Just make an internal
representation and leave the rest to the library predicate |put_on_path/3|.

\PL*/
compile_spec(put_on_path(Pred),_,
	     Depth,Depth,
	     Path,PathOut,
	     _,
	     [],
	     put_on_path(IR,Path,PathOut),
	     [],
	     Weight) :-
	!,
	require_predicate(put_on_path/3),
	make_internal_rep(Pred,IR),
	merge_literal_signs(Pred,Literal),
	need_proc(Literal),
	weight(put_on_path,Weight).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The next instruction puts a literal on the path. Just make an internal
representation and leave the rest to the library predicate |put_on_path/4|.

\PL*/
compile_spec(put_on_path(Pred,Info),_,
	     Depth,Depth,
	     Path,PathOut,
	     _,
	     [],
	     put_on_path(IR,Info,Path,PathOut),
	     [],
	     Weight) :-
	!,
	require_predicate(put_on_path/4),
	make_internal_rep(Pred,IR),
	merge_literal_signs(Pred,Literal),
	need_proc(Literal),
	weight(put_on_path,Weight).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A new path will be used from this time on. Thus we have to insert the path
given in the |PathOut| argument.

\PL*/
compile_spec(use_path(Path),_,Depth,Depth,_,Path,_,[],true,[],Weight) :-
	!,
	weight(use_path,Weight).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A call instruction is evaluated immediately. We have to take care to use the
current module where the descriptors are defined.

\PL*/
compile_spec(call(H),Module,Depth,Depth,Path,Path,_,[],true,[],Weight) :-
	!,
	call(H,Module),
	weight(call,Weight).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A constructor code is simply returned. No subproof term is generated.  This is
indicated by the empty list as proof term.

\PL*/
compile_spec(constructor(Code),_,Depth,Depth,Path,Path,_,[],Code,[],Weight) :-
	!,
	require_predicates_for(Code),
	weight(constructor,Weight).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This case is assumed to compile a template into an individual predicate. This
is done mainly in the predicate |compile_template/8| to take advantage of
Prolog hashing.

\PL*/
compile_spec(template(A,B),_,
	     Di,Do,
	     Path,PathOut,
	     Unifier,Proof,
	     Code,Aux,Weight) :-
	!,
	( nonvar(A) ->
	    compile_template(B,A,
			     Di,Do,
			     Path,PathOut,
			     Unifier,Proof,
			     Code,Aux,Weight)
	;   err("*** ProCom ignoring template with variable type: ",
		template(A,B))
	).
compile_spec(_,_,Depth,Depth,Path,Path,_,[],true,[],0).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate compile_template/10(+Type, +Pred,
			      DepthIn, DepthOut,
			      Path, PathOut,
			      Unifier,
			      -Proof, -Code, -Aux,
			      -Weight).

This predicate performs the compilation for templates. To take advantage from
the Prolog hashing the first arg is the instanciated type of the template.

A template on the goal is ignored.

\PL*/
:- mode compile_template(+,+,?,?,?,?,?,?,?,?,?).
compile_template(goal,_,Depth,Depth,P,P,_,[],true,[],Weight) :-
	weight(goal,Weight).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A residue is directly translated into a call to the associated procedure.

\PL*/
compile_template(residue,HeadSpec,
		 DepthIn,DepthOut,
		 Path,Path,
		 Unifier,
		 [Proof],Code,[],Weight) :-
	( getval('ProCom:depth_pass_through',on) ->
	    Depth = 'InOut'(DepthIn,DepthOut)
	;   Depth = DepthIn,
	    Depth = DepthOut
	),
	merge_literal_signs(--HeadSpec,Head),
	make_literal(Head,[driver],Depth,Path,Proof,Unifier,Code0,[]),
	term_variables(Head,Vars),
	( is_option('ProCom:lemma') ->
	    make_internal_rep(Head,IR),
	    Code = (literal_wrapper(Code0,Head,Depth,Path,Proof,Vars),
		    lemma(IR,Path,PathOut))
	;   Code = literal_wrapper(Code0,Head,Depth,Path,Proof,Vars),
	    Path = PathOut
	),
	need_proc(Head),
	weight(residue,Weight).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A residue is directly translated into a call to the associated procedure.

\PL*/
compile_template(neg_residue,HeadSpec,
		 DepthIn,DepthOut,
		 Path,PathOut,
		 Unifier,
		 [Proof],Code,[],Weight) :-
	( getval('ProCom:depth_pass_through',on) ->
	    Depth = 'InOut'(DepthIn,DepthOut)
	;   Depth = DepthIn,
	    Depth = DepthOut
	),
	merge_literal_signs(++HeadSpec,Head),
	make_literal(Head,[driver],Depth,Path,Proof,Unifier,Code0,[]),
	term_variables(Head,Vars),
	( is_option('ProCom:lemma') ->
	    make_internal_rep(Head,IR),
	    Code = (literal_wrapper(Code0,Head,Depth,Path,Proof,Vars),
		    lemma(IR,Path,PathOut))
	;   Code = literal_wrapper(Code0,Head,Depth,Path,Proof,Vars),
	    Path = PathOut
	),
	need_proc(Head),
	weight(neg_residue,Weight).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A template on the path is translated into a call to |is_on_path/2|. No
subproof term is needed.

\PL*/
compile_template(path,P,Depth,Depth,Path,Path,_,[],Code,[],Weight):- 
	require_predicate(is_on_path/2),
	merge_literal_signs(++P,PP),
	make_internal_rep(PP,IR),
	Code = is_on_path(IR,Path),
	weight(path,Weight).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
compile_template(path(Info),P,Depth,Depth,Path,Path,_,[],Code,[],Weight):- 
	require_predicate(is_on_path/3),
	merge_literal_signs(++P,PP),
	make_internal_rep(PP,IR),
	Code = is_on_path(IR,Info,Path),
	weight(path,Weight).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If no argument is requested for an extension then the argument is simply
ignored.

\PL*/
compile_template(extension,
		 Pred,
		 Di,Do,
		 Path,PathOut,
		 Unifier,Proof,
		 Code,Aux,
		 Weight) :-
	compile_template(extension(_),
			 Pred,
			 Di,Do,
			 Path,PathOut,
			 Unifier,
			 Proof,
			 Code,Aux,Weight).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A request for extension is translated into a call to an auxiliary predicate.
This auxiliary predicate may be unfolded and thus not given explicitely.

\PL*/
compile_template(extension(Index),LitSpec,
		 Di,Do,
		 Path,PathOut,
		 _,Proof,
		 Code,Aux,Weight) :-
	merge_literal_signs(LitSpec,Lit),
	getval('CaPrI aux counter',AUX),
	incval('CaPrI aux counter'),
	( getval('ProCom:need_weight',on) ->
	    Depth = 'InOut'(Di,Do)
	;   Depth = Di,
	    Depth = Do
	),
	Lit =.. [Sign,L],
	( getval('ProCom:need_weight',on) ->
	    add_args(L,NewL,Index,Weight),
	    weight(extension,Wext),
	    COMPUTE_WEIGHT = (Weight is length(Body)*Wext)
	;   add_args(L,NewL,Index),
	    COMPUTE_WEIGHT = true
	),
	NewLit =.. [Sign,NewL],
	make_literal(NewLit,
		     no_candidates,
		     Depth,
		     Path/PathOut,
		     Proof,
		     Unifier,
		     AuxHead,
		     [' AUX ',AUX]), 
	findall(Index+Clause,
		(   'Contrapositive'(Lit,Body,Index),
		    COMPUTE_WEIGHT,
		    compile_body(Body,Goals,Depth,Path,PathOut,Proof,Unifier),
		    Clause = (AuxHead :- Goals)
		),
		A_Clauses),
	( A_Clauses = [] ->
	    Code     = fail,
	    Aux	     = []
	; A_Clauses = [Index+(C:-B)] ->
	    unify(AuxHead,C),
	    Code     = B,
	    Aux	     = []
	;
	    reorder(A_Clauses,'ProCom:reorder_ext',compare_ext,A__Clauses),
	    skip_index(A__Clauses,Aux),
	    Code     = AuxHead
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate skip_index/2(+IndexedList, ?SimpleList).

Given a list of pairs $I-C$\/ as |IndexedList| return the list |SimpleList|
consisting of the elements $C$\/ only.

\PL*/
skip_index([],[]).
skip_index([_+H|T],[H|TT]) :-
	skip_index(T,TT).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate evaluate_get/4(+Type, ?Arg, Depth, Path).

\PL*/
evaluate_get(depth,Depth,Depth,_).
evaluate_get(path,Path,_,Path).
evaluate_get(contrapositive(Index),CP,_,_) :-
	'Contrapositive'(Head,Body,Index),
	CP = [literal(Head,Index)|Body].
evaluate_get(index(Literal),Index,_,_) :-
	'Contrapositive'(Literal,_,Index).
evaluate_get(neg(Literal),NegLit,_,_) :-
	( Literal = (++NegLit) ->
	    true
	; Literal = (--NegLit) ->
	    true
	; Literal = (-NegLit) ->
	    true
	; NegLit = (--Literal)
	).
evaluate_get(functor(Lit),F,_,_) :-
	merge_literal_signs(++Lit,Literal),
	( Literal = (-- Predicate) ->
	    functor(Predicate,Functor,Arity),
	    F = (-Functor)/Arity
	; Literal = (++ Predicate) ->
	    functor(Predicate,Functor,Arity),
	    F = Functor/Arity
	;   functor(Predicate,Functor,Arity),
	    F = Functor/Arity
	).
evaluate_get(functors,[],_,_) :- 
	true.
evaluate_get(literals,Literals,_,_) :-
	( setof(L, 'HashClause'(_,L,_), Literals) ->
	    true
	;   Literals = []
	).
evaluate_get(predicates,Predicates,_,_) :-
	( setof(F, evaluate_get_predicates(F), Predicates) ->
	    true
	;   Predicates = []
	).
evaluate_get(solved_goals,[],_,_).
evaluate_get(open_goals,[],_,_).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate evaluate_get_predicates/1(?Functors).

\PL*/
evaluate_get_predicates(FCT) :- 
	'Clause'(_,Body),
	member(L,Body),
	literal_functor(L,F,A),
	( literal_is_negative(L) ->
	    FCT = (-F)/A
	;   FCT = F/A
    ).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate extract_diagonal/3(+Desc, List, Rest).

A descriptor may contain templates with lists or maps of specifiers. Those
lists and maps are interpreted as disjunctions, i.e. an elements has to be
selected from lists and assignments have to be made for maps.

Other element of a descriptor may be relevant in which case they are returned.
Otherwise they are ignored.

The input argument |Desc| is a conjunction of descriptor elements. This
conjunction is decomposed and analyzed. The result is returned in the
difference list |DiffList|.

\PL*/
extract_diagonal((A,B),D,L,P1,P2) :-
	!,
	extract_diagonal(A,D,X,P1,P),
	extract_diagonal(B,X,L,P,P2).
extract_diagonal(template(Pred,Spec),D,L,P1,P2) :-
	!,
	( var(Pred) ->
	    extract_diagonal_from_template(Pred,Spec,D,L,P1,P2)
	; functor(Pred,'.',2) ->
	    err("*** ProCom list/map templates are not supported any more: ",
		template(Pred,Spec)),
	    fail,
	    extract_diagonal_from_template_list(Pred,Spec,D,L,P1,P2)
	; Pred = [] ->
	    true
	;   extract_diagonal_from_template(Pred,Spec,D,L,P1,P2)
	).
extract_diagonal(template(Pred) ,[template(Pred,goal)|L],L,P,P) :- !.
extract_diagonal(proof(_)	,L,L,P,P) :- !.
extract_diagonal(name(_)	,L,L,P,P) :- !.
extract_diagonal(constructor(C)	,[constructor(C)|L],L,Result,P) :-
	!,
	extract_diagonal_strip(C,CS),
	( CS = true ->
	    Result = P
	;   Result = [constructor(CS)|P]
	).
extract_diagonal(Instruction	,[Instruction|L],L,P,P).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate extract_diagonal_strip/1(Ignored).

This predicate provides those constructs which are ignored in th e
constructor. This predicate is called from |extract_diagonal_strip/2| to
perform its task.

\PL*/
extract_diagonal_strip(fail).
extract_diagonal_strip(!).
extract_diagonal_strip(write(_)).
extract_diagonal_strip(writeln(_)).
extract_diagonal_strip(display(_)).
extract_diagonal_strip(nl).
extract_diagonal_strip(printf(_,_)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate extract_diagonal_strip/2().

\PL*/
extract_diagonal_strip((A,B),Result) :-
	!,
	extract_diagonal_strip(A,AS),
	extract_diagonal_strip(B,BS),
	( AS == true ->
	    Result = BS
	; BS == true ->
	    Result = AS
	;   Result = (AS,BS)
	).
extract_diagonal_strip(Pred,Result) :-
	( extract_diagonal_strip(Pred) ->
	    Result = true
	;   Result = Pred
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate extract_diagonal_from_template/4(+Pred, +Spec, D,L).

A template |Spec| can either be a list or a simple predicate specification. A
simple predicate specification is just returned in the difference list
|D-L|. In the case of a list one element of the list is returned. This is only
possible if the list is not empty, in which case the predicate will fail.

\PL*/
extract_diagonal_from_template(Pred,Spec,[template(Pred,S)|L],L,P1,P2) :-
	( functor(Spec,'.',2) ->
	    member(S0,Spec),
	    proof_node(S0,Pred,P2,S,P1)
	; proof_node(Spec,Pred,P2,S,P1)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
proof_node(extension,Pred,R,extension(I),[extension(I,Pred)|R]).
proof_node(extension(I),Pred,R,extension(I),[extension(I,Pred)|R]).
proof_node(path(I),Pred,R,path(I),[path(Pred)|R]).
proof_node(path,Pred,R,path,[path(Pred)|R]).
proof_node(residue,_Pred,R,residue,[residue|R]).
proof_node(neg_residue,_Pred,R,neg_residue,[neg_residue|R]).
proof_node(goal,_Pred,R,goal,R).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate extract_diagonal_from_template_list/4(+PredList, +Spec, ?D, ?L).

\PL*/
extract_diagonal_from_template_list([],Spec,L,L) :- 
	Spec \== [].
extract_diagonal_from_template_list([Pred|List],Spec,D,L) :-
	( functor(Spec,'.',2) ->
	    D = [template(Pred,S)|DD],
	    member(S,Spec),
	    extract_diagonal_from_template_list(List,Spec,DD,L)
	; Spec =.. [map|Map] ->
	    extract_diagonal_from_template_map([Pred|List],Map,D,L)
	; Spec \== [] ->
	    D = [template(Pred,Spec)|DD],
	    extract_diagonal_from_template_list(Spec,List,DD,L)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate extract_diagonal_from_template_map/4(+PredicateList,
						+SpecList,
						-D,
                                                ?L).

\PL*/
extract_diagonal_from_template_map([],Spec,L,L) :- 
	( Spec == [] ->
	    true
	;   err("*** ProCom Error: Template map has inconsistent length."),
	    fail
	).
extract_diagonal_from_template_map([Pred|List],
				   Map,
				   [template(Pred,S)|D],
				   L) :-
	( Map == [] ->
	    err("*** ProCom Error: Map list has inconsistent length."),
	    fail
	;
	    delete(Spec,Map,Rest),
	    ( functor(Spec,'.',2) ->
		member(S,Spec)
	    ; Spec = [] ->
		fail
	    ;	S = Spec
	    ),
	    extract_diagonal_from_template_map(List,Rest,D,L)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate get_diagonal_goal/2(+Diag, ?Goal).

Given a diagonal |Diag| of a descriptor, this predicate succeeds iff there is
a goal template in |Diag| and all goal templates in |Diag| unify
simultaneously with |Goal|. This unification is performed.

This task is performed in two phases. The first phase searches for the first
goal template and tries to unify it with the given goal |Goal|. Then the
second phase |get_diagonal_goal_succ/2| is entered.

\PL*/
get_diagonal_goal([Spec|Diag],Goal) :-
	( Spec = template(G,goal) ->
	    unify(G,Goal),
	    get_diagonal_goal_succ(Diag,Goal)
	; Spec = template(G,goal(_)) ->
	    unify(G,Goal),
	    get_diagonal_goal_succ(Diag,Goal)
	;
	    get_diagonal_goal(Diag,Goal)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate get_diagonal_goal_succ/2(+Diag, ?Goal).

This predicate just searches for goal templates in a diagonal |Diag| and
unifies them with |Goal|. The difference to the first phase in the predicate
|get_diagonal_goal/2| is that it is not necessary that any goal template is
present at all. Thus this predicate succeeds in the case that no element is
left in the diagonal.

\PL*/
get_diagonal_goal_succ([],_).
get_diagonal_goal_succ([Spec|Diag],Goal) :-
	( Spec = template(G,goal) ->
	    unify(G,Goal),
	    get_diagonal_goal_succ(Diag,Goal)
	; Spec = template(G,goal(_)) ->
	    unify(G,Goal),
	    get_diagonal_goal_succ(Diag,Goal)
	;
	    get_diagonal_goal_succ(Diag,Goal)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate initialize_proof_steps/2(+Functors, +Module).

\PL*/
initialize_proof_steps(FunctorList,Module) :-
	reset_proof_steps,
	( is_option('ProCom:dynamic_reordering') ->
	    init_proof_steps(FunctorList,Module)
	;   put_proof_steps(_,_,no_candidates)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate init_proof_steps/2(+Functors, +Module).

\PL*/
init_proof_steps([],_).
init_proof_steps([F/A|Rest],Module) :-
	make_functor(F,A,FA),
	( (F = -F1) ->
	    functor(G,F1,A),
	    Goal = - G
	;   functor(Goal,F,A)
	),
	( findall(Step,
		  get_step(Module,Goal,Step),
		  Steps ) ->
	    true
	;   Steps = []
	),
	put_proof_steps(FA,_,Steps),
	init_proof_steps(Rest,Module).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate get_step/3(+Module, +Goal, ?Step).

\PL*/
get_step(Module,Goal,Step) :-
	call((descriptor D),Module),
	( once(descriptor_macro(D,Desc)) ->
	    true
	;
	    D = Desc
	),
	( desc_get(Desc,name(Step)) ->
	    true
	;
	    ( desc_get(Desc,proof(Proof)) ->
		functor(Proof,Step,_)
	    ;
		Step = '*proof*'
	    )
	),
	extract_diagonal(Desc,Diag,[],_,_),
	get_diagonal_goal(Diag,Goal).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate add/3(A, B, Sum).

Build the arithmetic expression $A+B$. In certain cases this expression can be
simplified. If one of $A$\/ or $B$\/ are $0$\/ the result is the other one. If
both are integers then the sum can be computed.

\PL*/
add(A,B,Sum) :-
	( A == 0 ->
	    Sum = B
	; B == 0 ->
	    Sum = A
	; (integer(A), integer(B)) ->
	    Sum is A + B
	;   Sum = (A + B)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
descriptor_macro(reduction,
		 ( template(Pred,goal),
		   template(-Pred,path)
		 )).
descriptor_macro(extension,Value) :-
	( is_option('ProCom:automatic_put_on_path') ->
	    Value = ( template(Pred,goal),
		      template(--Pred,extension(Index)))
	;   Value = ( template(Pred,goal),
		      put_on_path(Pred),
		      template(--Pred,extension(Index)))
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
weight(get,0).
weight(put_on_path,0).
weight(is_on_path,1).
weight(use_path,0).
weight(call,0).
weight(constructor,1).
weight(goal,0).
weight(residue,1).
weight(neg_residue,1).
weight(path,1).
weight(extension,1).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
