%%*****************************************************************************
%% $Id: xref.pl,v 1.4 1995/04/03 21:47:12 gerd Exp $
%%*****************************************************************************
%% Author: Gerd Neugebauer
%%=============================================================================

/*%----------------------------------------------------------------------------

\PL*/
:- [main].

path('').
path('System/').
path('ProTop/').
path('Filter/').
path('Prepare/').
path('Normalform/').
path('Reductions/').
path('Pool/').
path('Otter/').
path('Setheo/').
path('ProCom/').
path('Capri/').
path(Path) :-
	get_flag(library_path,OldPath),
	member(P,OldPath),
	concat_atom([P,'/'],Path).

find_file(Base,File) :-
	path(Path),
	concat_atom([Path,Base],File),
	exists(File),
	!.
find_file(Base,File) :-
	path(Path),
	concat_atom([Path,Base,'.pl'],File),
	exists(File),
	!.


:- set_error_handler(60,fail/0).

:- dynamic (defined)/3, (used)/3, (declared)/3, (exported)/3.


xref(List) :-
	retract_all(defined(_,_,_)),
	retract_all(used(_,_,_)),
	retract_all(declared(_,_,_)),
	retract_all(exported(_,_,_)),
	( List == [] ->
	    true
	; string(List) ->
	    xref_read_file(List)
	; atom(List) ->
	    xref_read_file(List)
	; functor(List,'.',2) ->
	    (	member(File,List),
		xref_read_file(File),
		fail
	    ;	true
	    )
	),
	format_xref.

xref_read_file(FileName) :-
	find_file(FileName,File),
	printf("%% %w\n%b",[File]),
	open(File,read,Stream),
	repeat,
	read(Stream,Term),
	( Term == end_of_file ->
	    close(Stream)
	;
	    xref_do(Term,File),
	    fail
	),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate xref_do/2().

\PL*/
xref_do((Head:-Body),File) :-
	!,
	xref_define(Head,File),
	xref_use(Body,omit,File).
xref_do((:-T),File) :-
	!,
	xref_use(T,do,File).
xref_do((?-T),File) :-
	!,
	xref_use(T,do,File).
xref_do(Fact,File) :-
	xref_define(Fact,File).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
xref_store((A,B),Type,File) :-
	!,
	xref_store(A,Type,File),
	xref_store(B,Type,File).
xref_store(F/A,Type,File) :-
	!,
	Fact =.. [Type,F,A,File],
	( ( once(Fact)
	  ; is_built_in(F/A)
	  ) ->
	    true
	;   assert(Fact)
	).
xref_store(_,_).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
xref_define(P,File) :-
	functor(P,F,A),
	xref_store(F/A,defined,File).

xref_declare(Spec,File) :-
	xref_store(Spec,defined,File).

xref_require(Spec,File) :-
	xref_store(Spec,used,File).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
xref_use(X,_,_) :-
	var(X),
	!.
xref_use((A,B),Flag,File) :-
	!,
	xref_use(A,Flag,File),
	xref_use(B,Flag,File).
xref_use((A;B),Flag,File) :-
	!,
	xref_use(A,Flag,File),
	xref_use(B,Flag,File).
xref_use((A->B),Flag,File) :-
	!,
	xref_use(A,Flag,File),
	xref_use(B,Flag,File).
xref_use(findall(_,A,_),Flag,File) :-
	!,
	xref_use(A,Flag,File).
xref_use(bagof(_,A,_),Flag,File) :-
	!,
	xref_use(A,Flag,File).
xref_use(setof(_,A,_),Flag,File) :-
	!,
	xref_use(A,Flag,File).
xref_use(once(A),Flag,File) :-
	!,
	xref_use(A,Flag,File).
xref_use(call(A),Flag,File) :-
	!,
	xref_use(A,Flag,File).
xref_use(module(_),_,_) :-
	!.
xref_use(module_interface(_),_,_) :-
	!.
xref_use(begin_module(_),_,_) :-
	!.
xref_use(op(A,B,C),_,_) :-
	!,
	op(A,B,C).
xref_use('-?->'(A),Flag,File) :-
	!,
	xref_use(A,Flag,File).
xref_use(tool(Spec,Body),_,File) :-
	!,
	xref_store(Spec,defined,File),
	xref_store(Body,used,File).
xref_use(define_macro(Spec,Macro,_),_,File) :-
	!,
	xref_store(Spec,defined,File),
	xref_store(Spec,exported,File),
	xref_store(Macro,used,File).
xref_use(dynamic(Spec),_,File) :-
	!,
	xref_declare(Spec,File).
xref_use(lib(Spec),Flag,_) :-
	!,
	(nonvar(Spec),Flag==go -> lib(Spec); true).
xref_use(export(Spec),_,File) :-
	!,
	xref_store(Spec,exported,File).
xref_use(global(Spec),_,File) :-
	!,
	xref_store(Spec,exported,File).
xref_use(Term,_,File) :-
	functor(Term,F,A),
	xref_store(F/A,used,File).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
format_xref :-
	open("xref.ltx",write,Stream),
	printf(Stream,"\n\\begin{XREF}\n",[]),
%	(   used(F,A,File),
%	    defined(F,A,File),
%	    retract(used(F,A,File)),
%	    fail
%	;   true
%	),
	setof(exported(F,A,File),exported(F,A,File),List),
	(   member(exported(F,A,File),List),
	    printf(Stream,"\\Spec{%w/%w}{%w}\n",[F,A,File]),
	    used(F,A,Used),
	    printf(Stream,"  \\Used{%w}\n",[Used]),
	    fail
	;   true
	),
	printf(Stream,"\\end{XREF}\n\n",[]),
	close(Stream).

:- dynamic node/2.

level :-
	setof(M,A^B^(used(A,B,M)),Nodes),
	level(Nodes,0),
	show_levels.

level([],_) :- !.
level(Open,N) :-
	findall(Mod,
		(member(Mod,Open),
		findall(x,
			( used(A,B,Mod),
			  exported(A,B,M),
			  M \== Mod,
			  member(M,Open)
			),
		        [])),
		Nodes),
	( Nodes == [] ->
	    printf("%% Still open: %w\n",[Open])
	;
	    save_level(Nodes,N,1),
	    subtract(Open,Nodes,StillOpen),
	    N1 is N+1,
	    level(StillOpen,N1)
	).

save_level([],_,_).
save_level([H|T],N,X) :-
	assert(node(H,X,N)),
	X1 is X+1,
	save_level(T,N,X1).
	
show_levels :-
	(   node(M,X,Y),
	    printf(" \Node(%w,%w){%w}\n",[X,Y,M]),
	    fail
        ;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
go :-
	xref([main,
	      add_arg,eq_member,find_file,find_module,hook,literal,log,maplist,
	      matrix,message,op_def,options,os,parse,proof,qsort,run_filter,
	      run_prover,time,unify,varlist,
	      protop,proof_report,proof_tree,report_summary,
	      prove,complete_goals,connection_graph,
	      reductions,red_pure,
	      procom,capri,capricore,linker,optimize,'p--simple',
	      p__depth_first,p__iterative_broadening,p__iterative_deepening,
	      p__iterative_inferences,p__iterative_widening,
	      p_body,p_check_desc,p_driver,p_goal,p_info,p_options,p_predicate,
	      p_put,p_reorder,unfold,
	      otter,
	      setheo,
	      pool,
	      'protop.cfg','prepare.cfg','procom.cfg','otter.cfg','setheo.cfg',
	      util,lists,numbervars,strings,sorts
	  ]),
	(   defined(P,A,File),
	    \+ used(P,A,_),
	    printf("--- %w/%w defined in %w but not used.\n",[P,A,File]),
	    fail
	;   nl,
	    fail
	;   used(P,A,File),
	    \+ defined(P,A,_),
	    printf("*** %w/%w used in %w but not defined.\n",[P,A,File]),
	    fail
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
exported(unify_args, 3, 'System/add_arg.pl').
exported(unify_args, 4, 'System/add_arg.pl').
exported(add_args, 3, 'System/add_arg.pl').
exported(add_args, 4, 'System/add_arg.pl').
exported(add_args, 5, 'System/add_arg.pl').
exported(add_args, 6, 'System/add_arg.pl').
exported(add_args, 7, 'System/add_arg.pl').
exported(add_args, 8, 'System/add_arg.pl').
exported(eq_member, 2, 'System/eq_member.pl').
exported(eq_subtract, 3, 'System/eq_member.pl').
exported(find_file, 4, 'System/find_file.pl').
exported(find_module, 4, 'System/find_module.pl').
exported(call_in_module, 4, 'System/find_module.pl').
exported(run_hooks, 1, 'System/hook.pl').
exported(run_hooks, 2, 'System/hook.pl').
exported(define_hook, 2, 'System/hook.pl').
exported(unhook, 3, 'System/hook.pl').
exported(hook, 3, 'System/hook.pl').
exported(negate_literal, 2, 'System/literal.pl').
exported(make_literal, 4, 'System/literal.pl').
exported(merge_literal_signs, 2, 'System/literal.pl').
exported('Macro literal_is_positive', 2, 'System/literal.pl').
exported('Macro literal_is_negative', 2, 'System/literal.pl').
exported('Macro literal_functor', 2, 'System/literal.pl').
exported('Macro literal_sign', 2, 'System/literal.pl').
exported('Macro literal_neg_sign', 2, 'System/literal.pl').
exported(literal_is_positive, 1, 'System/literal.pl').
exported(literal_is_negative, 1, 'System/literal.pl').
exported(literal_functor, 3, 'System/literal.pl').
exported(literal_sign, 2, 'System/literal.pl').
exported(literal_neg_sign, 2, 'System/literal.pl').
exported(log_reset_time_and_print, 2, 'System/log.pl').
exported(log_reset_time_and_print, 3, 'System/log.pl').
exported('Maplist', 3, 'System/maplist.pl').
exported('CheckPath', 1, 'System/matrix.pl').
exported('Clause', 3, 'System/matrix.pl').
exported('ClauseLength', 2, 'System/matrix.pl').
exported('Connection', 2, 'System/matrix.pl').
exported('Connection', 4, 'System/matrix.pl').
exported('Contrapositive', 4, 'System/matrix.pl').
exported('CP_by_index', 4, 'System/matrix.pl').
exported('GoalClause', 1, 'System/matrix.pl').
exported('HashClause', 3, 'System/matrix.pl').
exported('Label', 2, 'System/matrix.pl').
exported('MaxClause', 1, 'System/matrix.pl').
exported('NegativeClause', 1, 'System/matrix.pl').
exported('PositiveClause', 1, 'System/matrix.pl').
exported(add_goal_mark, 1, 'System/matrix.pl').
exported(add_connection, 2, 'System/matrix.pl').
exported(add_connection, 4, 'System/matrix.pl').
exported(add_contrapositive, 3, 'System/matrix.pl').
exported(delete_clause, 1, 'System/matrix.pl').
exported(delete_clauses, 1, 'System/matrix.pl').
exported(delete_contrapositive, 2, 'System/matrix.pl').
exported(delete_literal, 1, 'System/matrix.pl').
exported(delete_literals, 1, 'System/matrix.pl').
exported(make_index, 3, 'System/matrix.pl').
exported(remove_Clause, 1, 'System/matrix.pl').
exported(reset_matrix, 0, 'System/matrix.pl').
exported(show_matrix, 1, 'System/matrix.pl').
exported(show_matrix, 2, 'System/matrix.pl').
exported(store_clause, 1, 'System/matrix.pl').
exported(store_clause_vars, 3, 'System/matrix.pl').
exported(store_label, 1, 'System/matrix.pl').
exported(msg, 0, 'System/message.pl').
exported(msg, 1, 'System/message.pl').
exported(msg, 2, 'System/message.pl').
exported(msg, 3, 'System/message.pl').
exported(msg, 4, 'System/message.pl').
exported(msg, 5, 'System/message.pl').
exported(msg_list, 1, 'System/message.pl').
exported(err, 0, 'System/message.pl').
exported(err, 1, 'System/message.pl').
exported(err, 2, 'System/message.pl').
exported(err, 3, 'System/message.pl').
exported(err, 4, 'System/message.pl').
exported(err, 5, 'System/message.pl').
exported(err_list, 1, 'System/message.pl').
exported(err_list, 2, 'System/message.pl').
exported(is_option, 2, 'System/options.pl').
exported(is_option, 1, 'System/options.pl').
exported(set_option, 1, 'System/options.pl').
exported(reset_all_options, 0, 'System/options.pl').
exported(load_options, 1, 'System/options.pl').
exported(define_option, 1, 'System/options.pl').
exported(empty_option, 1, 'System/options.pl').
exported(delete_file, 1, 'System/os.pl').
exported(execute, 1, 'System/os.pl').
exported(read_matrix, 1, 'System/parse.pl').
exported(read_matrix_from_stream, 1, 'System/parse.pl').
exported(show_proof, 2, 'System/proof.pl').
exported(save_bindings, 4, 'System/proof.pl').
exported(save_proof, 2, 'System/proof.pl').
exported(qsort, 4, 'System/qsort.pl').
exported(read_matrix_with_filter, 2, 'System/run_filter.pl').
exported(declare_filter, 1, 'System/run_filter.pl').
exported(run_prover, 0, 'System/run_prover.pl').
exported(run_prover, 1, 'System/run_prover.pl').
exported(reset_time, 0, 'System/time.pl').
exported(reset_time, 1, 'System/time.pl').
exported(time_to_string, 2, 'System/time.pl').
exported(reset_time_and_print, 2, 'System/time.pl').
exported(reset_time_and_print, 3, 'System/time.pl').
exported(unify, 2, 'System/unify.pl').
exported(varlist, 2, 'System/varlist.pl').
exported(varlist, 3, 'System/varlist.pl').
exported(protop, 0, 'ProTop/protop.pl').
exported(protop, 1, 'ProTop/protop.pl').
exported(proof_report, 1, 'ProTop/proof_report.pl').
exported(proof_report, 2, 'ProTop/proof_report.pl').
exported(proof_report, 3, 'ProTop/proof_report.pl').
exported(proof_report_latex, 0, 'ProTop/proof_report.pl').
exported(proof_report_latex, 1, 'ProTop/proof_report.pl').
exported(proof_report_define_table, 2, 'ProTop/proof_report.pl').
exported(proof_report_remove_table, 1, 'ProTop/proof_report.pl').
exported(proof_tree, 2, 'ProTop/proof_tree.pl').
exported(proof_tree, 3, 'ProTop/proof_tree.pl').
exported(summary_clear, 0, 'ProTop/report_summary.pl').
exported(summary_store, 3, 'ProTop/report_summary.pl').
exported(make_summary_table, 2, 'ProTop/report_summary.pl').
exported(prove, 2, 'Prepare/prove.pl').
exported(complete_goals, 0, 'Prepare/complete_goals.pl').
exported(connection_graph, 0, 'Prepare/connection_graph.pl').
exported(init_connection_graph, 0, 'Prepare/connection_graph.pl').
exported('LiteralDegree', 2, 'Prepare/connection_graph.pl').
exported(apply_reductions, 0, 'Reductions/reductions.pl').
exported(red_pure, 0, 'Reductions/red_pure.pl').
exported(red_pure, 1, 'Reductions/red_pure.pl').
exported(red_pure, 2, 'Reductions/red_pure.pl').
exported(procom, 1, 'ProCom/procom.pl').
exported('CaPrI Description Interpreter', 1, 'ProCom/capricore.pl').
exported('CaPrI Define Prover', 1, 'ProCom/capricore.pl').
exported('CaPrI Check', 1, 'ProCom/capricore.pl').
exported(initialize_linker, 0, 'ProCom/linker.pl').
exported(initialize_linker, 1, 'ProCom/linker.pl').
exported(link_runtime_system, 0, 'ProCom/linker.pl').
exported(add_link_path, 1, 'ProCom/linker.pl').
exported(add_link_files, 1, 'ProCom/linker.pl').
exported(add_link_files_from_options, 1, 'ProCom/linker.pl').
exported(immediate_link_from_option, 1, 'ProCom/linker.pl').
exported(require_predicate, 1, 'ProCom/linker.pl').
exported(expand_predicate, 1, 'ProCom/linker.pl').
exported(provide_predicate, 1, 'ProCom/linker.pl').
exported(provide_predicate, 2, 'ProCom/linker.pl').
exported(provide_definition, 2, 'ProCom/linker.pl').
exported(put_optimized_clause, 1, 'ProCom/optimize.pl').
exported(clear_definitions, 0, 'ProCom/optimize.pl').
exported(store_definition, 3, 'ProCom/optimize.pl').
exported(compare_extensions, 4, 'ProCom/p--simple.pl').
exported(compile_body, 6, 'ProCom/p_body.pl').
exported(compile_body, 7, 'ProCom/p_body.pl').
exported(reset_proof_steps, 0, 'ProCom/p_body.pl').
exported(put_proof_steps, 3, 'ProCom/p_body.pl').
exported(proof_steps, 3, 'ProCom/p_body.pl').
exported('CaPrI Check', 1, 'ProCom/p_check_desc.pl').
exported(make_driver, 3, 'ProCom/p_driver.pl').
exported(make_driver_clauses, 3, 'ProCom/p_driver.pl').
exported(compile_goal, 0, 'ProCom/p_goal.pl').
exported('Need Path', 1, 'ProCom/p_info.pl').
exported('Need Reduction', 1, 'ProCom/p_info.pl').
exported(use_path, 1, 'ProCom/p_info.pl').
exported(use_reductions, 1, 'ProCom/p_info.pl').
exported(force_options, 1, 'ProCom/p_options.pl').
exported(apply_options, 2, 'ProCom/p_options.pl').
exported(require_options, 1, 'ProCom/p_options.pl').
exported(check_requirement, 1, 'ProCom/p_options.pl').
exported(negate_functor, 2, 'ProCom/p_predicate.pl').
exported(is_negative, 1, 'ProCom/p_predicate.pl').
exported(build_functor, 3, 'ProCom/p_predicate.pl').
exported(make_functor, 3, 'ProCom/p_predicate.pl').
exported(make_functor_from_template, 4, 'ProCom/p_predicate.pl').
exported(make_template, 3, 'ProCom/p_predicate.pl').
exported(make_internal_rep, 2, 'ProCom/p_predicate.pl').
exported(make_literal, 8, 'ProCom/p_predicate.pl').
exported(translate_head, 3, 'ProCom/p_predicate.pl').
exported(put_open, 1, 'ProCom/p_put.pl').
exported(put_close, 0, 'ProCom/p_put.pl').
exported(put_nl, 0, 'ProCom/p_put.pl').
exported(puts, 1, 'ProCom/p_put.pl').
exported(puts, 2, 'ProCom/p_put.pl').
exported(puts, 3, 'ProCom/p_put.pl').
exported(put_report, 2, 'ProCom/p_put.pl').
exported(put_report, 3, 'ProCom/p_put.pl').
exported(put_report, 4, 'ProCom/p_put.pl').
exported(put_report, 5, 'ProCom/p_put.pl').
exported(put_boxed, 2, 'ProCom/p_put.pl').
exported(put_matrix, 0, 'ProCom/p_put.pl').
exported(put_clause, 1, 'ProCom/p_put.pl').
exported(put_clause, 2, 'ProCom/p_put.pl').
exported(reorder, 4, 'ProCom/p_reorder.pl').
exported(apply_definitions, 3, 'ProCom/unfold.pl').
exported(apply_unsorted_definitions, 3, 'ProCom/unfold.pl').
exported(otter, 0, 'Otter/otter.pl').
exported(otter, 1, 'Otter/otter.pl').
exported(setheo, 0, 'Setheo/setheo.pl').
exported(pool, 0, 'Pool/pool.pl').
exported(add_path, 1, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
exported(add_suffix, 1, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
exported(between, 3, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
exported(compiled, 0, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
exported(compile_selection, 0, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
exported(list_error, 3, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
exported(stream, 1, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
exported(streams, 0, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
exported(read_line, 1, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
exported(read_line, 2, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
exported(stat, 1, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
exported(ptags_all, 0, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
exported(time, 1, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
exported(write_history, 0, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
exported(append, 3, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
exported(checklist, 2, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
exported(delete, 3, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
exported(flatten, 2, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
exported(intersection, 3, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
exported(length, 2, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
exported(maplist, 3, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
exported(member, 2, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
exported(memberchk, 2, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
exported(nonmember, 2, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
exported(print_list, 1, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
exported(reverse, 2, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
exported(subset, 2, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
exported(subtract, 3, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
exported(union, 3, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
exported(numbervars, 3, '/usr/local/lib/eclipse/3.4.5/lib/numbervars.pl').
exported(append_strings, 3, '/usr/local/lib/eclipse/3.4.5/lib/strings.pl').
exported(substring, 4, '/usr/local/lib/eclipse/3.4.5/lib/strings.pl').
exported(keysort, 2, '/usr/local/lib/eclipse/3.4.5/lib/sorts.pl').
exported(sort, 2, '/usr/local/lib/eclipse/3.4.5/lib/sorts.pl').
exported(msort, 2, '/usr/local/lib/eclipse/3.4.5/lib/sorts.pl').
exported(merge, 3, '/usr/local/lib/eclipse/3.4.5/lib/sorts.pl').
exported(merge, 5, '/usr/local/lib/eclipse/3.4.5/lib/sorts.pl').
exported(prune_instances, 2, '/usr/local/lib/eclipse/3.4.5/lib/sorts.pl').


used(append, 3, 'System/add_arg.pl').
used(unify_args, 3, 'System/add_arg.pl').
used(unify_args, 4, 'System/add_arg.pl').
used(eq_member, 2, 'System/eq_member.pl').
used(eq_subtract, 3, 'System/eq_member.pl').
used(member, 2, 'System/find_file.pl').
used(find_file, 4, 'System/find_file.pl').
used(msg, 1, 'System/find_file.pl').
used(err, 4, 'System/find_file.pl').
used(err, 3, 'System/find_file.pl').
used(err, 5, 'System/find_file.pl').
used(check_requirement, 2, 'System/find_module.pl').
used(find_file, 4, 'System/find_module.pl').
used(find_module, 4, 'System/find_module.pl').
used(define_hook, 2, 'System/hook.pl').
used(hook, 3, 'System/hook.pl').
used(unhook, 3, 'System/hook.pl').
used('Hook', 3, 'System/hook.pl').
used('Macro literal_is_positive', 2, 'System/literal.pl').
used('Macro literal_functor', 2, 'System/literal.pl').
used('Macro literal_sign', 2, 'System/literal.pl').
used('Macro literal_neg_sign', 2, 'System/literal.pl').
used(merge_literal_signs, 2, 'System/literal.pl').
used(log_reset_time_and_print, 3, 'System/log.pl').
used(reset_time, 1, 'System/log.pl').
used(time_to_string, 2, 'System/log.pl').
used(is_option, 2, 'System/log.pl').
used(empty_option, 1, 'System/log.pl').
used('Maplist', 3, 'System/maplist.pl').
used(append, 3, 'System/maplist.pl').
used(define_hook, 1, 'System/matrix.pl').
used(run_hooks, 1, 'System/matrix.pl').
used('Contrapositive', 4, 'System/matrix.pl').
used('CP_by_index', 4, 'System/matrix.pl').
used('Connection', 4, 'System/matrix.pl').
used(append, 3, 'System/matrix.pl').
used(err, 1, 'System/matrix.pl').
used(store_clause, 1, 'System/matrix.pl').
used(add_goal_mark, 1, 'System/matrix.pl').
used(prepare_clause, 5, 'System/matrix.pl').
used(store_contrapositives, 2, 'System/matrix.pl').
used('GoalClause', 1, 'System/matrix.pl').
used(delete_clause, 1, 'System/matrix.pl').
used(delete_clauses, 1, 'System/matrix.pl').
used(delete, 3, 'System/matrix.pl').
used('PositiveClause', 1, 'System/matrix.pl').
used('NegativeClause', 1, 'System/matrix.pl').
used(delete_literal, 1, 'System/matrix.pl').
used(delete_literals, 1, 'System/matrix.pl').
used(show_matrix, 2, 'System/matrix.pl').
used('Label', 2, 'System/matrix.pl').
used(numbervars, 3, 'System/matrix.pl').
used(show_clause, 3, 'System/matrix.pl').
used(msg_list, 1, 'System/message.pl').
used(err_list, 1, 'System/message.pl').
used(member, 2, 'System/options.pl').
used(is_option, 2, 'System/options.pl').
used(err, 3, 'System/options.pl').
used(err, 2, 'System/options.pl').
used(set_option, 1, 'System/options.pl').
used(delete_file, 1, 'System/os.pl').
used(define_option, 1, 'System/parse.pl').
used(is_option, 2, 'System/parse.pl').
used(find_file, 4, 'System/parse.pl').
used(msg, 2, 'System/parse.pl').
used(err, 3, 'System/parse.pl').
used(read_matrix_from_stream, 1, 'System/parse.pl').
used(reset_time, 0, 'System/parse.pl').
used(reset_matrix, 0, 'System/parse.pl').
used(parse_clause, 2, 'System/parse.pl').
used('MaxClause', 1, 'System/parse.pl').
used(log_reset_time_and_print, 2, 'System/parse.pl').
used(parse_clause, 1, 'System/parse.pl').
used(store_label, 1, 'System/parse.pl').
used(store_clause, 1, 'System/parse.pl').
used(add_to_clause, 4, 'System/parse.pl').
used(merge_literal_signs, 2, 'System/parse.pl').
used(show_proof_list, 3, 'System/proof.pl').
used(show_proof_term, 3, 'System/proof.pl').
used(qsort, 4, 'System/qsort.pl').
used(sort3, 8, 'System/qsort.pl').
used(qpartition, 6, 'System/qsort.pl').
used(append, 3, 'System/qsort.pl').
used(define_option, 1, 'System/run_filter.pl').
used(is_option, 2, 'System/run_filter.pl').
used(filter_module, 1, 'System/run_filter.pl').
used(find_file, 4, 'System/run_filter.pl').
used(err, 3, 'System/run_filter.pl').
used(empty_option, 1, 'System/run_filter.pl').
used(msg, 1, 'System/run_filter.pl').
used(read_matrix, 1, 'System/run_filter.pl').
used(err, 2, 'System/run_filter.pl').
used(run_filter, 2, 'System/run_filter.pl').
used(read_matrix_with_filter, 2, 'System/run_filter.pl').
used(read_matrix_from_stream, 1, 'System/run_filter.pl').
used(declare_filter, 1, 'System/run_filter.pl').
used(reset_time, 0, 'System/run_filter.pl').
used(reset_time_and_print, 2, 'System/run_filter.pl').
used(define_option, 1, 'System/run_prover.pl').
used(is_option, 2, 'System/run_prover.pl').
used(run_prover, 1, 'System/run_prover.pl').
used(empty_option, 1, 'System/run_prover.pl').
used(err, 1, 'System/run_prover.pl').
used(err, 2, 'System/run_prover.pl').
used(is_option, 1, 'System/run_prover.pl').
used(reset_time_and_print, 3, 'System/time.pl').
used(reset_time, 1, 'System/time.pl').
used(time_to_string, 2, 'System/time.pl').
used(varlist, 6, 'System/varlist.pl').
used(varlist, 7, 'System/varlist.pl').
used(prover_system_path, 1, 'ProTop/protop.pl').
used(union, 3, 'ProTop/protop.pl').
used(define_option, 1, 'ProTop/protop.pl').
used(pt, 1, 'ProTop/protop.pl').
used(get_argv, 1, 'ProTop/protop.pl').
used(is_option, 2, 'ProTop/protop.pl').
used(member, 2, 'ProTop/protop.pl').
used(append, 3, 'ProTop/protop.pl').
used(empty_option, 1, 'ProTop/protop.pl').
used(find_file, 4, 'ProTop/protop.pl').
used(is_option, 1, 'ProTop/protop.pl').
used(protop_version, 1, 'ProTop/protop.pl').
used(protop_set_error_handlers, 0, 'ProTop/protop.pl').
used(get_argv, 3, 'ProTop/protop.pl').
used(protop, 3, 'ProTop/protop.pl').
used(protop_end, 1, 'ProTop/protop.pl').
used(protop_trace, 2, 'ProTop/protop.pl').
used(err, 3, 'ProTop/protop.pl').
used(pt_command, 1, 'ProTop/protop.pl').
used('ProTop macro', 3, 'ProTop/protop.pl').
used(err, 1, 'ProTop/protop.pl').
used(protop_status, 1, 'ProTop/protop.pl').
used(add_dir, 3, 'ProTop/protop.pl').
used(err, 2, 'ProTop/protop.pl').
used(msg, 1, 'ProTop/protop.pl').
used(set_option, 1, 'ProTop/protop.pl').
used(reset_all_options, 0, 'ProTop/protop.pl').
used(default_option, 1, 'ProTop/protop.pl').
used(prove, 2, 'ProTop/protop.pl').
used(protop_open_log_stream, 1, 'ProTop/protop.pl').
used(show_matrix, 2, 'ProTop/protop.pl').
used(protop_do_report, 2, 'ProTop/protop.pl').
used(msg, 2, 'ProTop/protop.pl').
used(proof_report_define_table, 2, 'ProTop/protop.pl').
used(proof_report_remove_table, 1, 'ProTop/protop.pl').
used(proof_report, 1, 'ProTop/protop.pl').
used(proof_report_latex, 0, 'ProTop/protop.pl').
used(length, 2, 'ProTop/protop.pl').
used(prover_module, 1, 'ProTop/protop.pl').
used(filter_module, 1, 'ProTop/protop.pl').
used('Contrapositive', 4, 'ProTop/protop.pl').
used('GoalClause', 1, 'ProTop/protop.pl').
used(define_option, 1, 'ProTop/proof_report.pl').
used(is_option, 2, 'ProTop/proof_report.pl').
used(proof_report, 3, 'ProTop/proof_report.pl').
used(summary_clear, 0, 'ProTop/proof_report.pl').
used(log_clear_single, 0, 'ProTop/proof_report.pl').
used(member, 2, 'ProTop/proof_report.pl').
used(make_report_init, 2, 'ProTop/proof_report.pl').
used(report_read_file, 2, 'ProTop/proof_report.pl').
used(make_report_tables, 1, 'ProTop/proof_report.pl').
used(make_report_exit, 1, 'ProTop/proof_report.pl').
used(proof_report_latex, 1, 'ProTop/proof_report.pl').
used(summary_spec, 2, 'ProTop/proof_report.pl').
used(make_summary_table, 2, 'ProTop/proof_report.pl').
used(collect_options, 2, 'ProTop/proof_report.pl').
used(is_option, 1, 'ProTop/proof_report.pl').
used(log_flag, 1, 'ProTop/proof_report.pl').
used(log_preamble, 1, 'ProTop/proof_report.pl').
used(err, 3, 'ProTop/proof_report.pl').
used(log_do, 3, 'ProTop/proof_report.pl').
used(err, 2, 'ProTop/proof_report.pl').
used(log_label, 1, 'ProTop/proof_report.pl').
used(log_section, 1, 'ProTop/proof_report.pl').
used(summary_store, 3, 'ProTop/proof_report.pl').
used(log_time, 2, 'ProTop/proof_report.pl').
used(log_do_block, 2, 'ProTop/proof_report.pl').
used(make_report, 1, 'ProTop/proof_report.pl').
used(log_do_block_generic, 4, 'ProTop/proof_report.pl').
used(log_is_init, 0, 'ProTop/proof_report.pl').
used(log_title, 1, 'ProTop/proof_report.pl').
used(log_author, 1, 'ProTop/proof_report.pl').
used(log_info, 2, 'ProTop/proof_report.pl').
used(substring, 4, 'ProTop/proof_report.pl').
used(make_report_experiment, 2, 'ProTop/proof_report.pl').
used(report_push, 0, 'ProTop/proof_report.pl').
used(make_report, 2, 'ProTop/proof_report.pl').
used(log_proof, 1, 'ProTop/proof_report.pl').
used(proof_tree, 3, 'ProTop/proof_report.pl').
used(make_report_text, 3, 'ProTop/proof_report.pl').
used(log_comment, 1, 'ProTop/proof_report.pl').
used(log_matrix, 1, 'ProTop/proof_report.pl').
used(make_report_clause, 3, 'ProTop/proof_report.pl').
used(log_option, 1, 'ProTop/proof_report.pl').
used(ignore_option, 2, 'ProTop/proof_report.pl').
used(make_report_text_loop, 2, 'ProTop/proof_report.pl').
used(proof_tree, 3, 'ProTop/proof_tree.pl').
used(member, 2, 'ProTop/proof_tree.pl').
used(proof_tree_phase_1, 3, 'ProTop/proof_tree.pl').
used(position_level, 8, 'ProTop/proof_tree.pl').
used(reposition_nodes, 2, 'ProTop/proof_tree.pl').
used(node_position, 3, 'ProTop/proof_tree.pl').
used(node_son, 2, 'ProTop/proof_tree.pl').
used(node_info, 2, 'ProTop/proof_tree.pl').
used(tree_node, 3, 'ProTop/proof_tree.pl').
used(proof_tree_phase_1_args, 4, 'ProTop/proof_tree.pl').
used(append, 3, 'ProTop/proof_tree.pl').
used(store_neighbors, 2, 'ProTop/proof_tree.pl').
used(position_level_nodes, 7, 'ProTop/proof_tree.pl').
used(summary_assemble_format, 2, 'ProTop/report_summary.pl').
used(summary_guess_format, 2, 'ProTop/report_summary.pl').
used(member, 2, 'ProTop/report_summary.pl').
used(info_db, 3, 'ProTop/report_summary.pl').
used(summary_format, 3, 'ProTop/report_summary.pl').
used(typeset_one_title, 2, 'ProTop/report_summary.pl').
used(do, 3, 'ProTop/report_summary.pl').
used(typeset_one_row_item, 2, 'ProTop/report_summary.pl').
used(typeset_one_row, 2, 'ProTop/report_summary.pl').
used(match_time, 2, 'ProTop/report_summary.pl').
used(typeset_one_row_item_generic, 5, 'ProTop/report_summary.pl').
used(do_sum, 2, 'ProTop/report_summary.pl').
used(do_sum, 4, 'ProTop/report_summary.pl').
used(make_summary_table, 2, 'ProTop/report_summary.pl').
used(filter_module, 1, 'Prepare/prove.pl').
used(declare_filter, 1, 'Prepare/prove.pl').
used(prover_module, 1, 'Prepare/prove.pl').
used(define_hook, 1, 'Prepare/prove.pl').
used(define_option, 1, 'Prepare/prove.pl').
used(is_option, 2, 'Prepare/prove.pl').
used(find_file, 4, 'Prepare/prove.pl').
used(msg, 3, 'Prepare/prove.pl').
used(err, 3, 'Prepare/prove.pl').
used(err, 4, 'Prepare/prove.pl').
used(set_option, 1, 'Prepare/prove.pl').
used(run_hooks, 1, 'Prepare/prove.pl').
used(phase_1, 1, 'Prepare/prove.pl').
used(is_option, 1, 'Prepare/prove.pl').
used(member, 2, 'Prepare/prove.pl').
used(msg, 2, 'Prepare/prove.pl').
used(msg, 0, 'Prepare/prove.pl').
used(phase2, 0, 'Prepare/prove.pl').
used(err, 1, 'Prepare/prove.pl').
used(read_matrix_with_filter, 2, 'Prepare/prove.pl').
used('Clause', 3, 'Prepare/prove.pl').
used(reset_time, 0, 'Prepare/prove.pl').
used(init_connection_graph, 0, 'Prepare/prove.pl').
used(reset_time_and_print, 2, 'Prepare/prove.pl').
used(connection_graph, 0, 'Prepare/prove.pl').
used(log_reset_time_and_print, 2, 'Prepare/prove.pl').
used(empty_option, 1, 'Prepare/prove.pl').
used(define_goal_completion, 1, 'Prepare/prove.pl').
used(run_goals, 1, 'Prepare/prove.pl').
used(err, 2, 'Prepare/prove.pl').
used('GoalClause', 1, 'Prepare/complete_goals.pl').
used(have_negative_clause, 1, 'Prepare/complete_goals.pl').
used(msg, 1, 'Prepare/complete_goals.pl').
used('NegativeClause', 1, 'Prepare/complete_goals.pl').
used(have_positive_clause, 1, 'Prepare/complete_goals.pl').
used('PositiveClause', 1, 'Prepare/complete_goals.pl').
used(add_goal_mark, 1, 'Prepare/complete_goals.pl').
used(define_option, 1, 'Prepare/connection_graph.pl').
used(is_option, 2, 'Prepare/connection_graph.pl').
used(empty_option, 1, 'Prepare/connection_graph.pl').
used(connection_graph, 1, 'Prepare/connection_graph.pl').
used(is_option, 1, 'Prepare/connection_graph.pl').
used(remove_unreached_clauses, 0, 'Prepare/connection_graph.pl').
used('GoalClause', 1, 'Prepare/connection_graph.pl').
used('HashClause', 3, 'Prepare/connection_graph.pl').
used(connect_weak, 1, 'Prepare/connection_graph.pl').
used(connect_literals, 1, 'Prepare/connection_graph.pl').
used(connect_literals_all, 1, 'Prepare/connection_graph.pl').
used(connect_clauses, 1, 'Prepare/connection_graph.pl').
used(connect_clauses_all, 1, 'Prepare/connection_graph.pl').
used(connect_extend_weak, 2, 'Prepare/connection_graph.pl').
used('CP_by_index', 4, 'Prepare/connection_graph.pl').
used('Contrapositive', 4, 'Prepare/connection_graph.pl').
used(add_connection, 4, 'Prepare/connection_graph.pl').
used(member, 2, 'Prepare/connection_graph.pl').
used('LiteralDegree', 2, 'Prepare/connection_graph.pl').
used(negate_literal, 2, 'Prepare/connection_graph.pl').
used('Clause', 3, 'Prepare/connection_graph.pl').
used(length, 2, 'Prepare/connection_graph.pl').
used(connect_one_literal, 2, 'Prepare/connection_graph.pl').
used('Connection', 4, 'Prepare/connection_graph.pl').
used(add_connection, 2, 'Prepare/connection_graph.pl').
used(literal_functor, 3, 'Prepare/connection_graph.pl').
used(literal_neg_sign, 2, 'Prepare/connection_graph.pl').
used(make_literal, 4, 'Prepare/connection_graph.pl').
used(connect_one_literal_all, 2, 'Prepare/connection_graph.pl').
used(remove_Clause, 1, 'Prepare/connection_graph.pl').
used(msg, 3, 'Prepare/connection_graph.pl').
used(define_option, 1, 'Reductions/reductions.pl').
used(red_pure, 1, 'Reductions/reductions.pl').
used(is_option, 1, 'Reductions/reductions.pl').
used(print_reduction_statistics, 0, 'Reductions/reductions.pl').
used(msg, 1, 'Reductions/reductions.pl').
used(msg, 2, 'Reductions/reductions.pl').
used(msg, 0, 'Reductions/reductions.pl').
used(member, 2, 'Reductions/red_pure.pl').
used('Clause', 3, 'Reductions/red_pure.pl').
used(pure_p, 3, 'Reductions/red_pure.pl').
used(delete_contrapositive, 2, 'Reductions/red_pure.pl').
used(define_prover, 1, 'ProCom/procom.pl').
used(define_search, 1, 'ProCom/procom.pl').
used(define_reorder, 1, 'ProCom/procom.pl').
used(define_option, 1, 'ProCom/procom.pl').
used(is_option, 2, 'ProCom/procom.pl').
used(is_option, 1, 'ProCom/procom.pl').
used(set_option, 1, 'ProCom/procom.pl').
used(select_prover, 1, 'ProCom/procom.pl').
used(force_options, 1, 'ProCom/procom.pl').
used(hook, 2, 'ProCom/procom.pl').
used(err, 2, 'ProCom/procom.pl').
used(load_module, 3, 'ProCom/procom.pl').
used(msg, 3, 'ProCom/procom.pl').
used(err, 3, 'ProCom/procom.pl').
used(err, 5, 'ProCom/procom.pl').
used(require_options, 1, 'ProCom/procom.pl').
used(check_requirement, 1, 'ProCom/procom.pl').
used(msg, 2, 'ProCom/procom.pl').
used(msg, 1, 'ProCom/procom.pl').
used(procom_compile, 1, 'ProCom/procom.pl').
used(run_prover, 1, 'ProCom/procom.pl').
used(procom_check, 1, 'ProCom/procom.pl').
used(find_file, 4, 'ProCom/procom.pl').
used(put_open, 1, 'ProCom/procom.pl').
used(put_nl, 0, 'ProCom/procom.pl').
used(put_matrix, 0, 'ProCom/procom.pl').
used(reset_time, 0, 'ProCom/procom.pl').
used(msg, 0, 'ProCom/procom.pl').
used(put_close, 0, 'ProCom/procom.pl').
used(err, 1, 'ProCom/procom.pl').
used(log_reset_time_and_print, 2, 'ProCom/procom.pl').
used('CaPrI Check', 1, 'ProCom/procom.pl').
used('CaPrI Define Prover', 0, 'ProCom/capri.pl').
used('CaPrI Define Prover', 1, 'ProCom/capricore.pl').
used(msg, 1, 'ProCom/capricore.pl').
used(check, 1, 'ProCom/capricore.pl').
used(err, 1, 'ProCom/capricore.pl').
used(desc_get, 2, 'ProCom/capricore.pl').
used(err, 4, 'ProCom/capricore.pl').
used(put_boxed, 2, 'ProCom/capricore.pl').
used(apply_options, 2, 'ProCom/capricore.pl').
used(initialize_linker, 0, 'ProCom/capricore.pl').
used(is_option, 1, 'ProCom/capricore.pl').
used(add_link_files_from_options, 1, 'ProCom/capricore.pl').
used(add_link_files, 1, 'ProCom/capricore.pl').
used(add_link_path, 1, 'ProCom/capricore.pl').
used(require_predicate, 1, 'ProCom/capricore.pl').
used(provide_definition, 2, 'ProCom/capricore.pl').
used(use_reductions, 1, 'ProCom/capricore.pl').
used(immediate_link_from_option, 1, 'ProCom/capricore.pl').
used(is_option, 2, 'ProCom/capricore.pl').
used(err, 3, 'ProCom/capricore.pl').
used(set_option, 1, 'ProCom/capricore.pl').
used(puts, 1, 'ProCom/capricore.pl').
used(puts, 3, 'ProCom/capricore.pl').
used(initialize_proof_steps, 2, 'ProCom/capricore.pl').
used(compile_autonomous, 2, 'ProCom/capricore.pl').
used(compile_procedures, 2, 'ProCom/capricore.pl').
used('Clause', 3, 'ProCom/capricore.pl').
used(put_clause, 1, 'ProCom/capricore.pl').
used(compile_goal, 0, 'ProCom/capricore.pl').
used(link_runtime_system, 0, 'ProCom/capricore.pl').
used(put_report, 3, 'ProCom/capricore.pl').
used(compile_desc, 5, 'ProCom/capricore.pl').
used(collect_aux, 3, 'ProCom/capricore.pl').
used(put_optimized_clause, 1, 'ProCom/capricore.pl').
used(make_template, 3, 'ProCom/capricore.pl').
used(put_report, 5, 'ProCom/capricore.pl').
used(make_driver, 3, 'ProCom/capricore.pl').
used(put_report, 2, 'ProCom/capricore.pl').
used(make_driver_clauses, 3, 'ProCom/capricore.pl').
used(number_clauses, 2, 'ProCom/capricore.pl').
used(reorder, 4, 'ProCom/capricore.pl').
used(number_clauses_1, 2, 'ProCom/capricore.pl').
used(append, 3, 'ProCom/capricore.pl').
used(extract_diagonal, 2, 'ProCom/capricore.pl').
used(negate_literal, 2, 'ProCom/capricore.pl').
used(get_diagonal_goal, 2, 'ProCom/capricore.pl').
used(make_literal, 8, 'ProCom/capricore.pl').
used(compile_spec, 11, 'ProCom/capricore.pl').
used(make_internal_rep, 2, 'ProCom/capricore.pl').
used(apply_unsorted_definitions, 3, 'ProCom/capricore.pl').
used(add, 3, 'ProCom/capricore.pl').
used(evaluate_get, 4, 'ProCom/capricore.pl').
used(weight, 2, 'ProCom/capricore.pl').
used(compile_template, 11, 'ProCom/capricore.pl').
used(err, 2, 'ProCom/capricore.pl').
used(merge_literal_signs, 2, 'ProCom/capricore.pl').
used(add_args, 4, 'ProCom/capricore.pl').
used(add_args, 3, 'ProCom/capricore.pl').
used(compile_body, 7, 'ProCom/capricore.pl').
used(unify, 2, 'ProCom/capricore.pl').
used(skip_index, 2, 'ProCom/capricore.pl').
used('HashClause', 3, 'ProCom/capricore.pl').
used(evaluate_get_predicates, 1, 'ProCom/capricore.pl').
used('CP_by_index', 4, 'ProCom/capricore.pl').
used(literal_functor, 3, 'ProCom/capricore.pl').
used(literal_is_negative, 1, 'ProCom/capricore.pl').
used(extract_diagonal_from_template, 3, 'ProCom/capricore.pl').
used(extract_diagonal_from_template_list, 3, 'ProCom/capricore.pl').
used(member, 2, 'ProCom/capricore.pl').
used(extract_diagonal_from_template_map, 3, 'ProCom/capricore.pl').
used(delete, 3, 'ProCom/capricore.pl').
used(get_diagonal_goal_succ, 2, 'ProCom/capricore.pl').
used(reset_proof_steps, 0, 'ProCom/capricore.pl').
used(init_proof_steps, 2, 'ProCom/capricore.pl').
used(put_proof_steps, 3, 'ProCom/capricore.pl').
used(make_functor, 3, 'ProCom/capricore.pl').
used(get_step, 3, 'ProCom/capricore.pl').
used(clear_definitions, 0, 'ProCom/linker.pl').
used(is_option, 2, 'ProCom/linker.pl').
used(add_link_path, 2, 'ProCom/linker.pl').
used(initialize_linker, 0, 'ProCom/linker.pl').
used(add_link_files, 1, 'ProCom/linker.pl').
used('Link Path', 1, 'ProCom/linker.pl').
used(find_file, 4, 'ProCom/linker.pl').
used(add_link_file, 1, 'ProCom/linker.pl').
used(add_link_files_from_options, 1, 'ProCom/linker.pl').
used(find_file, 2, 'ProCom/linker.pl').
used('Link File', 1, 'ProCom/linker.pl').
used(analyze_library, 2, 'ProCom/linker.pl').
used(puts, 3, 'ProCom/linker.pl').
used(err, 3, 'ProCom/linker.pl').
used(provide_predicate, 2, 'ProCom/linker.pl').
used(require_predicate, 2, 'ProCom/linker.pl').
used(expand_predicate, 1, 'ProCom/linker.pl').
used('Expand Predicate', 1, 'ProCom/linker.pl').
used(store_definition, 3, 'ProCom/linker.pl').
used(provide_definition, 2, 'ProCom/linker.pl').
used(is_option, 1, 'ProCom/linker.pl').
used('Provide Predicate', 3, 'ProCom/linker.pl').
used('Require Predicate', 3, 'ProCom/linker.pl').
used(append, 3, 'ProCom/linker.pl').
used(determine_files, 3, 'ProCom/linker.pl').
used('Def', 3, 'ProCom/linker.pl').
used(put_clause, 1, 'ProCom/linker.pl').
used(static_linker, 1, 'ProCom/linker.pl').
used(dynamic_linker, 1, 'ProCom/linker.pl').
used(put_nl, 0, 'ProCom/linker.pl').
used(put_report, 3, 'ProCom/linker.pl').
used(static_link, 1, 'ProCom/linker.pl').
used(immediate_link_from_option, 1, 'ProCom/linker.pl').
used(is_option, 1, 'ProCom/optimize.pl').
used(get_clause, 2, 'ProCom/optimize.pl').
used(put_simplified_clause, 1, 'ProCom/optimize.pl').
used(maplist, 2, 'ProCom/optimize.pl').
used(put_nl, 0, 'ProCom/optimize.pl').
used(puts, 1, 'ProCom/optimize.pl').
used(simplify, 2, 'ProCom/optimize.pl').
used(put_clause, 1, 'ProCom/optimize.pl').
used(decompose_unify, 4, 'ProCom/optimize.pl').
used(simplify_arithm, 2, 'ProCom/optimize.pl').
used('Definition', 3, 'ProCom/optimize.pl').
used(get_clause_from_proc, 3, 'ProCom/optimize.pl').
used(merge_conjunction, 3, 'ProCom/optimize.pl').
used(member, 2, 'ProCom/optimize.pl').
used(split_conjunction, 3, 'ProCom/optimize.pl').
used(merge_disjunction, 3, 'ProCom/optimize.pl').
used(conjunction_length, 2, 'ProCom/p--simple.pl').
used(term_vars, 2, 'ProCom/p--simple.pl').
used(term_vars, 3, 'ProCom/p--simple.pl').
used(term_vars, 4, 'ProCom/p--simple.pl').
used(is_option, 2, 'ProCom/p__depth_first.pl').
used(err, 2, 'ProCom/p__depth_first.pl').
used(add_link_files, 1, 'ProCom/p__depth_first.pl').
used(is_option, 2, 'ProCom/p__iterative_broadening.pl').
used(expand_predicate, 1, 'ProCom/p__iterative_broadening.pl').
used(provide_definition, 2, 'ProCom/p__iterative_broadening.pl').
used(require_predicate, 1, 'ProCom/p__iterative_broadening.pl').
used(add_link_files, 1, 'ProCom/p__iterative_broadening.pl').
used(is_option, 2, 'ProCom/p__iterative_deepening.pl').
used(err, 2, 'ProCom/p__iterative_deepening.pl').
used(expand_predicate, 1, 'ProCom/p__iterative_deepening.pl').
used(provide_definition, 2, 'ProCom/p__iterative_deepening.pl').
used(require_predicate, 1, 'ProCom/p__iterative_deepening.pl').
used(add_link_files, 1, 'ProCom/p__iterative_deepening.pl').
used(is_option, 2, 'ProCom/p__iterative_inferences.pl').
used(msg, 1, 'ProCom/p__iterative_inferences.pl').
used(set_option, 1, 'ProCom/p__iterative_inferences.pl').
used(err, 2, 'ProCom/p__iterative_inferences.pl').
used(expand_predicate, 1, 'ProCom/p__iterative_inferences.pl').
used(provide_definition, 2, 'ProCom/p__iterative_inferences.pl').
used(require_predicate, 1, 'ProCom/p__iterative_inferences.pl').
used(add_link_files, 1, 'ProCom/p__iterative_inferences.pl').
used(is_option, 2, 'ProCom/p__iterative_widening.pl').
used(err, 2, 'ProCom/p__iterative_widening.pl').
used(expand_predicate, 1, 'ProCom/p__iterative_widening.pl').
used(provide_definition, 2, 'ProCom/p__iterative_widening.pl').
used(require_predicate, 1, 'ProCom/p__iterative_widening.pl').
used(add_link_files, 1, 'ProCom/p__iterative_widening.pl').
used(compile_body, 7, 'ProCom/p_body.pl').
used(compile_body_2, 8, 'ProCom/p_body.pl').
used(compile_body_1, 7, 'ProCom/p_body.pl').
used(make_functor_from_template, 4, 'ProCom/p_body.pl').
used(proof_steps, 3, 'ProCom/p_body.pl').
used(negate_literal, 2, 'ProCom/p_body.pl').
used(make_literal, 8, 'ProCom/p_body.pl').
used(is_option, 1, 'ProCom/p_body.pl').
used(make_internal_rep, 2, 'ProCom/p_body.pl').
used(msg, 1, 'ProCom/p_check_desc.pl').
used(check, 1, 'ProCom/p_check_desc.pl').
used(desc_get, 2, 'ProCom/p_check_desc.pl').
used(err, 4, 'ProCom/p_check_desc.pl').
used(make_functor, 3, 'ProCom/p_driver.pl').
used(negate_functor, 2, 'ProCom/p_driver.pl').
used(unify_args, 3, 'ProCom/p_driver.pl').
used(negate_literal, 2, 'ProCom/p_driver.pl').
used(make_literal, 8, 'ProCom/p_driver.pl').
used(is_option, 1, 'ProCom/p_driver.pl').
used(require_predicate, 1, 'ProCom/p_driver.pl').
used('Need Path', 1, 'ProCom/p_driver.pl').
used(make_cl, 8, 'ProCom/p_driver.pl').
used(append, 3, 'ProCom/p_driver.pl').
used(is_option, 2, 'ProCom/p_driver.pl').
used(member, 2, 'ProCom/p_driver.pl').
used('GoalClause', 1, 'ProCom/p_goal.pl').
used(require_predicate, 1, 'ProCom/p_goal.pl').
used(puts, 1, 'ProCom/p_goal.pl').
used(puts, 2, 'ProCom/p_goal.pl').
used(compile_goals, 5, 'ProCom/p_goal.pl').
used(reorder, 4, 'ProCom/p_goal.pl').
used(is_option, 2, 'ProCom/p_goal.pl').
used(prepare_goal_list, 2, 'ProCom/p_goal.pl').
used(prepare_goal_list, 3, 'ProCom/p_goal.pl').
used(is_option, 1, 'ProCom/p_goal.pl').
used(put_optimized_clause, 1, 'ProCom/p_goal.pl').
used(compile_one_goal, 6, 'ProCom/p_goal.pl').
used('CP_by_index', 4, 'ProCom/p_goal.pl').
used(varlist, 3, 'ProCom/p_goal.pl').
used(compile_body, 6, 'ProCom/p_goal.pl').
used(prepare_goal_list, 4, 'ProCom/p_goal.pl').
used(strip_body, 2, 'ProCom/p_goal.pl').
used(add_args, 3, 'ProCom/p_goal.pl').
used(add_args, 4, 'ProCom/p_goal.pl').
used(err, 2, 'ProCom/p_info.pl').
used('Need Reduction', 1, 'ProCom/p_info.pl').
used(err, 2, 'ProCom/p_options.pl').
used(require_opts, 1, 'ProCom/p_options.pl').
used(is_option, 2, 'ProCom/p_options.pl').
used(member, 2, 'ProCom/p_options.pl').
used(set_option, 1, 'ProCom/p_options.pl').
used(force_options, 1, 'ProCom/p_options.pl').
used(use_path, 1, 'ProCom/p_options.pl').
used(use_reductions, 1, 'ProCom/p_options.pl').
used(get_functor, 1, 'ProCom/p_options.pl').
used('Clause', 3, 'ProCom/p_options.pl').
used('Connection', 2, 'ProCom/p_options.pl').
used(make_functor, 3, 'ProCom/p_options.pl').
used('GoalClause', 1, 'ProCom/p_options.pl').
used('CP_by_index', 4, 'ProCom/p_options.pl').
used(make_functor_from_template, 4, 'ProCom/p_predicate.pl').
used(make_internal_rep, 2, 'ProCom/p_predicate.pl').
used(make_functor, 3, 'ProCom/p_predicate.pl').
used(unify_args, 3, 'ProCom/p_predicate.pl').
used(make_literal, 8, 'ProCom/p_predicate.pl').
used(is_option, 1, 'ProCom/p_predicate.pl').
used('Need Path', 1, 'ProCom/p_predicate.pl').
used(unify_arguments, 4, 'ProCom/p_predicate.pl').
used(translate_head, 6, 'ProCom/p_predicate.pl').
used(eq_member, 2, 'ProCom/p_predicate.pl').
used(require_predicate, 1, 'ProCom/p_predicate.pl').
used(translate_head, 7, 'ProCom/p_predicate.pl').
used(show_matrix, 2, 'ProCom/p_put.pl').
used(puts, 1, 'ProCom/p_put.pl').
used('Put List', 2, 'ProCom/p_put.pl').
used(is_option, 1, 'ProCom/p_put.pl').
used(put_clause_to_stream, 2, 'ProCom/p_put.pl').
used(apply_vars, 1, 'ProCom/p_put.pl').
used(put_body_to_stream, 5, 'ProCom/p_put.pl').
used(get_meta_goals, 4, 'ProCom/p_put.pl').
used(indent_to, 2, 'ProCom/p_put.pl').
used(put_d_i_on_stream, 5, 'ProCom/p_put.pl').
used(put_disjunction_to_stream, 5, 'ProCom/p_put.pl').
used(append, 3, 'ProCom/p_put.pl').
used(get_meta_vars_from_list, 3, 'ProCom/p_put.pl').
used(get_meta_goals, 2, 'ProCom/p_put.pl').
used(get_attribute, 2, 'ProCom/p_put.pl').
used(is_option, 2, 'ProCom/p_reorder.pl').
used(empty_option, 1, 'ProCom/p_reorder.pl').
used(err, 3, 'ProCom/p_reorder.pl').
used(err, 4, 'ProCom/p_reorder.pl').
used(qsort, 4, 'ProCom/p_reorder.pl').
used(get_pattern, 2, 'ProCom/unfold.pl').
used(member, 2, 'ProCom/unfold.pl').
used(apply_definitions, 3, 'ProCom/unfold.pl').
used(apply_definitions_once, 3, 'ProCom/unfold.pl').
used(apply_one_definition, 5, 'ProCom/unfold.pl').
used(get_literal, 4, 'ProCom/unfold.pl').
used(unify, 2, 'ProCom/unfold.pl').
used(define_otter, 1, 'Otter/otter.pl').
used(define_option, 1, 'Otter/otter.pl').
used(set_option, 1, 'Otter/otter.pl').
used(otter, 0, 'Otter/otter.pl').
used(otter_determine_files, 2, 'Otter/otter.pl').
used(otter_preamble, 1, 'Otter/otter.pl').
used(otter_save_matrix, 1, 'Otter/otter.pl').
used(is_option, 2, 'Otter/otter.pl').
used(otter_analyze, 2, 'Otter/otter.pl').
used(is_option, 1, 'Otter/otter.pl').
used(delete_file, 1, 'Otter/otter.pl').
used(find_file, 4, 'Otter/otter.pl').
used(member, 2, 'Otter/otter.pl').
used(otter_flag, 2, 'Otter/otter.pl').
used(otter_numeric_flag, 3, 'Otter/otter.pl').
used(otter_boolean_flag, 3, 'Otter/otter.pl').
used(err, 2, 'Otter/otter.pl').
used(err, 4, 'Otter/otter.pl').
used(msg, 3, 'Otter/otter.pl').
used(err, 3, 'Otter/otter.pl').
used('CP_by_index', 4, 'Otter/otter.pl').
used(otter_save_literal, 2, 'Otter/otter.pl').
used(otter_analyze, 3, 'Otter/otter.pl').
used(otter_analyze_times, 2, 'Otter/otter.pl').
used(otter_analyze_proof, 2, 'Otter/otter.pl').
used(otter_analyze_proof_step, 1, 'Otter/otter.pl').
used('otter proof', 3, 'Otter/otter.pl').
used(otter_analyze_proof_term, 4, 'Otter/otter.pl').
used(substring, 4, 'Otter/otter.pl').
used('otter proof', 1, 'Otter/otter.pl').
used(otter_analyze_proof_term_subproofs, 2, 'Otter/otter.pl').
used(define_setheo, 1, 'Setheo/setheo.pl').
used(define_option, 1, 'Setheo/setheo.pl').
used(setheo_determine_files, 4, 'Setheo/setheo.pl').
used(setheo_save_matrix, 1, 'Setheo/setheo.pl').
used(is_option, 2, 'Setheo/setheo.pl').
used(collect_flags, 2, 'Setheo/setheo.pl').
used(execute, 1, 'Setheo/setheo.pl').
used(analyze_setheo, 2, 'Setheo/setheo.pl').
used(is_option, 1, 'Setheo/setheo.pl').
used(delete_file, 1, 'Setheo/setheo.pl').
used(err, 1, 'Setheo/setheo.pl').
used(find_file, 4, 'Setheo/setheo.pl').
used(member, 2, 'Setheo/setheo.pl').
used('CP_by_index', 4, 'Setheo/setheo.pl').
used(analyze_setheo_proof, 2, 'Setheo/setheo.pl').
used(save_proof, 2, 'Setheo/setheo.pl').
used(show_proof, 2, 'Setheo/setheo.pl').
used(analyze_setheo_proof_step, 2, 'Setheo/setheo.pl').
used(convert_literal, 2, 'Setheo/setheo.pl').
used(append, 3, 'Setheo/setheo.pl').
used(define_option, 1, 'Pool/pool.pl').
used(is_option, 2, 'Pool/pool.pl').
used(err, 2, 'Pool/pool.pl').
used(reset_time, 0, 'Pool/pool.pl').
used(set_depth_bound, 4, 'Pool/pool.pl').
used(is_option, 1, 'Pool/pool.pl').
used('GoalClause', 1, 'Pool/pool.pl').
used('HashClause', 3, 'Pool/pool.pl').
used(solve_pool, 3, 'Pool/pool.pl').
used(msg, 1, 'Pool/pool.pl').
used(select_hook, 3, 'Pool/pool.pl').
used(solve_hook, 4, 'Pool/pool.pl').
used(select_literal, 3, 'Pool/pool.pl').
used(negate_literal, 2, 'Pool/pool.pl').
used(solve_literal, 4, 'Pool/pool.pl').
used(sound_member, 2, 'Pool/pool.pl').
used('Clause', 3, 'Pool/pool.pl').
used(print_modes, 0, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
used(time_body, 2, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
used(ptags, 1, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
used(history_collect, 1, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
used(write_history, 2, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
used(compile_selection, 1, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
used(compile_stream_, 2, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
used(maplist_body, 4, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
used(checklist_body, 3, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
used(member, 2, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
used(memberchk, 2, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
used(append, 3, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
used(nonmember, 2, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
used(delete, 3, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
used(intersection, 3, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
used(subtract, 3, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
used(union, 3, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
used(subset, 2, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
used(list_check, 3, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
used(length, 3, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
used(length1, 2, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
used(flatten, 3, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
used(reverse, 3, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
used(print_list, 1, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
used(numbervars, 5, '/usr/local/lib/eclipse/3.4.5/lib/numbervars.pl').
used(numbervars, 3, '/usr/local/lib/eclipse/3.4.5/lib/numbervars.pl').
used(append, 3, '/usr/local/lib/eclipse/3.4.5/lib/strings.pl').
used(length, 3, '/usr/local/lib/eclipse/3.4.5/lib/strings.pl').
used(var_or_string, 1, '/usr/local/lib/eclipse/3.4.5/lib/strings.pl').
used(concat_chk, 3, '/usr/local/lib/eclipse/3.4.5/lib/strings.pl').
used(first_substring, 4, '/usr/local/lib/eclipse/3.4.5/lib/strings.pl').
used(halve, 4, '/usr/local/lib/eclipse/3.4.5/lib/sorts.pl').
used(compare, 5, '/usr/local/lib/eclipse/3.4.5/lib/sorts.pl').
used(merge, 5, '/usr/local/lib/eclipse/3.4.5/lib/sorts.pl').
used(compare, 4, '/usr/local/lib/eclipse/3.4.5/lib/sorts.pl').
used(combine, 3, '/usr/local/lib/eclipse/3.4.5/lib/sorts.pl').
used(prune, 3, '/usr/local/lib/eclipse/3.4.5/lib/sorts.pl').
used(prune1, 3, '/usr/local/lib/eclipse/3.4.5/lib/sorts.pl').

defined(add_args, 3, 'System/add_arg.pl').
defined(add_args, 4, 'System/add_arg.pl').
defined(add_args, 5, 'System/add_arg.pl').
defined(add_args, 6, 'System/add_arg.pl').
defined(add_args, 7, 'System/add_arg.pl').
defined(unify_args, 3, 'System/add_arg.pl').
defined(unify_args, 4, 'System/add_arg.pl').
defined(eq_member, 2, 'System/eq_member.pl').
defined(eq_subtract, 3, 'System/eq_member.pl').
defined(find_file, 4, 'System/find_file.pl').
defined(load_and_call, 6, 'System/find_file.pl').
defined(find_module, 4, 'System/find_module.pl').
defined(check_requirement, 2, 'System/find_module.pl').
defined(call_in_module, 4, 'System/find_module.pl').
defined(define_hook, 1, 'System/hook.pl').
defined(hook, 2, 'System/hook.pl').
defined(unhook, 2, 'System/hook.pl').
defined('Hook', 2, 'System/hook.pl').
defined('Hook', 3, 'System/hook.pl').
defined(define_hook, 2, 'System/hook.pl').
defined(hook, 3, 'System/hook.pl').
defined(unhook, 3, 'System/hook.pl').
defined(run_hooks, 1, 'System/hook.pl').
defined(run_hooks, 2, 'System/hook.pl').
defined(literal_is_positive, 1, 'System/literal.pl').
defined(literal_is_negative, 1, 'System/literal.pl').
defined(literal_functor, 3, 'System/literal.pl').
defined(literal_sign, 2, 'System/literal.pl').
defined(literal_neg_sign, 2, 'System/literal.pl').
defined(negate_literal, 2, 'System/literal.pl').
defined(make_literal, 4, 'System/literal.pl').
defined('Macro literal_is_positive', 2, 'System/literal.pl').
defined('Macro literal_is_negative', 2, 'System/literal.pl').
defined('Macro literal_functor', 2, 'System/literal.pl').
defined('Macro literal_sign', 2, 'System/literal.pl').
defined('Macro literal_neg_sign', 2, 'System/literal.pl').
defined(merge_literal_signs, 2, 'System/literal.pl').
defined(log_reset_time_and_print, 2, 'System/log.pl').
defined(log_reset_time_and_print, 3, 'System/log.pl').
defined(maplist, 2, 'System/maplist.pl').
defined('Maplist', 3, 'System/maplist.pl').
defined('CheckPath', 1, 'System/matrix.pl').
defined('ClauseVars', 3, 'System/matrix.pl').
defined('ClauseLength', 2, 'System/matrix.pl').
defined('Connection', 4, 'System/matrix.pl').
defined('Contrapositive', 4, 'System/matrix.pl').
defined('CP_by_index', 4, 'System/matrix.pl').
defined('GoalClause', 1, 'System/matrix.pl').
defined('Label', 2, 'System/matrix.pl').
defined('LiteralDegree', 2, 'System/matrix.pl').
defined('NegativeClause', 1, 'System/matrix.pl').
defined('PositiveClause', 1, 'System/matrix.pl').
defined(reset_matrix, 0, 'System/matrix.pl').
defined('MaxClause', 1, 'System/matrix.pl').
defined('Clause', 3, 'System/matrix.pl').
defined('HashClause', 3, 'System/matrix.pl').
defined('Connection', 2, 'System/matrix.pl').
defined(remove_Clause, 1, 'System/matrix.pl').
defined(add_connection, 2, 'System/matrix.pl').
defined(add_connection, 4, 'System/matrix.pl').
defined(remove_connection, 2, 'System/matrix.pl').
defined(make_index, 3, 'System/matrix.pl').
defined(store_label, 1, 'System/matrix.pl').
defined(store_clause, 1, 'System/matrix.pl').
defined(store_clause_vars, 3, 'System/matrix.pl').
defined(add_goal_mark, 1, 'System/matrix.pl').
defined(prepare_clause, 5, 'System/matrix.pl').
defined(store_contrapositives, 2, 'System/matrix.pl').
defined(add_contrapositive, 3, 'System/matrix.pl').
defined(delete_contrapositive, 2, 'System/matrix.pl').
defined(delete_clause, 1, 'System/matrix.pl').
defined(delete_clauses, 1, 'System/matrix.pl').
defined(delete_literal, 1, 'System/matrix.pl').
defined(delete_literals, 1, 'System/matrix.pl').
defined(show_matrix, 1, 'System/matrix.pl').
defined(show_matrix, 2, 'System/matrix.pl').
defined(show_clause, 3, 'System/matrix.pl').
defined(msg, 0, 'System/message.pl').
defined(msg, 1, 'System/message.pl').
defined(msg, 2, 'System/message.pl').
defined(msg, 3, 'System/message.pl').
defined(msg, 4, 'System/message.pl').
defined(msg, 5, 'System/message.pl').
defined(msg_list, 1, 'System/message.pl').
defined(err, 0, 'System/message.pl').
defined(err, 1, 'System/message.pl').
defined(err, 2, 'System/message.pl').
defined(err, 3, 'System/message.pl').
defined(err, 4, 'System/message.pl').
defined(err, 5, 'System/message.pl').
defined(err_list, 1, 'System/message.pl').
defined(err_list, 2, 'System/message.pl').
defined(is_option, 2, 'System/options.pl').
defined(is_option, 1, 'System/options.pl').
defined(define_option, 1, 'System/options.pl').
defined(reset_all_options, 0, 'System/options.pl').
defined(set_option, 1, 'System/options.pl').
defined(load_options, 1, 'System/options.pl').
defined(empty_option, 1, 'System/options.pl').
defined(delete_file, 1, 'System/os.pl').
defined(execute, 1, 'System/os.pl').
defined(read_matrix, 1, 'System/parse.pl').
defined(read_matrix_from_stream, 1, 'System/parse.pl').
defined(parse_clause, 2, 'System/parse.pl').
defined(parse_clause, 1, 'System/parse.pl').
defined(add_to_clause, 4, 'System/parse.pl').
defined(save_bindings, 4, 'System/proof.pl').
defined(save_proof, 2, 'System/proof.pl').
defined(show_proof, 2, 'System/proof.pl').
defined(show_proof_list, 3, 'System/proof.pl').
defined(show_proof_term, 3, 'System/proof.pl').
defined(qsort, 3, 'System/qsort.pl').
defined(qsort, 4, 'System/qsort.pl').
defined(qpartition, 6, 'System/qsort.pl').
defined(sort3, 8, 'System/qsort.pl').
defined(filter_module, 1, 'System/run_filter.pl').
defined(declare_filter, 1, 'System/run_filter.pl').
defined(read_matrix_with_filter, 2, 'System/run_filter.pl').
defined(run_filter, 2, 'System/run_filter.pl').
defined(run_prover, 0, 'System/run_prover.pl').
defined(run_prover, 1, 'System/run_prover.pl').
defined(run_prover_timeout_handler, 0, 'System/run_prover.pl').
defined(run_report_timeout, 1, 'System/run_prover.pl').
defined(reset_time, 0, 'System/time.pl').
defined(reset_time, 1, 'System/time.pl').
defined(reset_time_and_print, 2, 'System/time.pl').
defined(reset_time_and_print, 3, 'System/time.pl').
defined(time_to_string, 2, 'System/time.pl').
defined(unify, 2, 'System/unify.pl').
defined(varlist, 2, 'System/varlist.pl').
defined(varlist, 3, 'System/varlist.pl').
defined(varlist, 6, 'System/varlist.pl').
defined(varlist, 7, 'System/varlist.pl').
defined('ProTop macro', 3, 'ProTop/protop.pl').
defined(protop, 0, 'ProTop/protop.pl').
defined(protop_set_error_handlers, 0, 'ProTop/protop.pl').
defined(protop_interrupt_handler, 0, 'ProTop/protop.pl').
defined(protop_interrupt_handler, 1, 'ProTop/protop.pl').
defined(protop_error_handler, 3, 'ProTop/protop.pl').
defined(get_argv, 1, 'ProTop/protop.pl').
defined(get_argv, 3, 'ProTop/protop.pl').
defined(protop, 1, 'ProTop/protop.pl').
defined(protop, 3, 'ProTop/protop.pl').
defined(protop_end, 1, 'ProTop/protop.pl').
defined(protop_trace, 2, 'ProTop/protop.pl').
defined(pt, 1, 'ProTop/protop.pl').
defined(protop_open_log_stream, 1, 'ProTop/protop.pl').
defined(protop_do_report, 2, 'ProTop/protop.pl').
defined(protop_status, 1, 'ProTop/protop.pl').
defined(add_dir, 3, 'ProTop/protop.pl').
defined(file_type, 1, 'ProTop/protop.pl').
defined(log_flag, 1, 'ProTop/proof_report.pl').
defined(log_author, 1, 'ProTop/proof_report.pl').
defined(log_title, 1, 'ProTop/proof_report.pl').
defined(log_preamble, 1, 'ProTop/proof_report.pl').
defined(log_label, 1, 'ProTop/proof_report.pl').
defined(log_section, 1, 'ProTop/proof_report.pl').
defined(log_matrix, 1, 'ProTop/proof_report.pl').
defined(log_option, 1, 'ProTop/proof_report.pl').
defined(log_proof, 1, 'ProTop/proof_report.pl').
defined(log_time, 2, 'ProTop/proof_report.pl').
defined(log_text, 1, 'ProTop/proof_report.pl').
defined(log_comment, 1, 'ProTop/proof_report.pl').
defined(log_info, 2, 'ProTop/proof_report.pl').
defined(log_hostinfo, 2, 'ProTop/proof_report.pl').
defined(log_is_init, 0, 'ProTop/proof_report.pl').
defined(summary_spec, 2, 'ProTop/proof_report.pl').
defined(proof_report_define_table, 2, 'ProTop/proof_report.pl').
defined(proof_report_remove_table, 1, 'ProTop/proof_report.pl').
defined(proof_report, 1, 'ProTop/proof_report.pl').
defined(proof_report, 2, 'ProTop/proof_report.pl').
defined(proof_report, 3, 'ProTop/proof_report.pl').
defined(proof_report_latex, 0, 'ProTop/proof_report.pl').
defined(proof_report_latex, 1, 'ProTop/proof_report.pl').
defined(make_report_tables, 1, 'ProTop/proof_report.pl').
defined(make_report_init, 2, 'ProTop/proof_report.pl').
defined(collect_options, 2, 'ProTop/proof_report.pl').
defined(make_report_exit, 1, 'ProTop/proof_report.pl').
defined(report_read_file, 2, 'ProTop/proof_report.pl').
defined(log_clear_single, 0, 'ProTop/proof_report.pl').
defined(report_push, 0, 'ProTop/proof_report.pl').
defined(log_do, 3, 'ProTop/proof_report.pl').
defined(log_do_block, 2, 'ProTop/proof_report.pl').
defined(log_do_block_generic, 4, 'ProTop/proof_report.pl').
defined(make_report, 1, 'ProTop/proof_report.pl').
defined(make_report_experiment, 2, 'ProTop/proof_report.pl').
defined(make_report, 2, 'ProTop/proof_report.pl').
defined(ignore_option, 2, 'ProTop/proof_report.pl').
defined(make_report_text, 3, 'ProTop/proof_report.pl').
defined(make_report_text_loop, 2, 'ProTop/proof_report.pl').
defined(make_report_clause, 3, 'ProTop/proof_report.pl').
defined(node_info, 2, 'ProTop/proof_tree.pl').
defined(node_position, 3, 'ProTop/proof_tree.pl').
defined(node_lr, 3, 'ProTop/proof_tree.pl').
defined(node_son, 2, 'ProTop/proof_tree.pl').
defined(proof_tree, 2, 'ProTop/proof_tree.pl').
defined(proof_tree, 3, 'ProTop/proof_tree.pl').
defined(tree_node, 3, 'ProTop/proof_tree.pl').
defined(proof_tree_phase_1, 3, 'ProTop/proof_tree.pl').
defined(proof_tree_phase_1_args, 4, 'ProTop/proof_tree.pl').
defined(position_level, 8, 'ProTop/proof_tree.pl').
defined(position_level_nodes, 7, 'ProTop/proof_tree.pl').
defined(store_neighbors, 2, 'ProTop/proof_tree.pl').
defined(info_db, 3, 'ProTop/report_summary.pl').
defined(info_label, 1, 'ProTop/report_summary.pl').
defined(summary_clear, 0, 'ProTop/report_summary.pl').
defined(summary_store, 3, 'ProTop/report_summary.pl').
defined(summary_assemble_format, 2, 'ProTop/report_summary.pl').
defined(summary_guess_format, 2, 'ProTop/report_summary.pl').
defined(summary_format, 3, 'ProTop/report_summary.pl').
defined(make_summary_table, 2, 'ProTop/report_summary.pl').
defined(typeset_one_title, 2, 'ProTop/report_summary.pl').
defined(typeset_one_row, 2, 'ProTop/report_summary.pl').
defined(typeset_one_row_item, 2, 'ProTop/report_summary.pl').
defined(typeset_one_row_item_generic, 5, 'ProTop/report_summary.pl').
defined(do_sum, 2, 'ProTop/report_summary.pl').
defined(do_sum, 4, 'ProTop/report_summary.pl').
defined(match_time, 2, 'ProTop/report_summary.pl').
defined(do, 3, 'ProTop/report_summary.pl').
defined(go, 0, 'ProTop/report_summary.pl').
defined(go1, 0, 'ProTop/report_summary.pl').
defined(prove, 2, 'Prepare/prove.pl').
defined(phase_1, 1, 'Prepare/prove.pl').
defined(phase_2, 0, 'Prepare/prove.pl').
defined(run_goals, 1, 'Prepare/prove.pl').
defined(complete_goals, 0, 'Prepare/complete_goals.pl').
defined(have_positive_clause, 1, 'Prepare/complete_goals.pl').
defined(have_negative_clause, 1, 'Prepare/complete_goals.pl').
defined('LiteralDegree', 2, 'Prepare/connection_graph.pl').
defined(init_connection_graph, 0, 'Prepare/connection_graph.pl').
defined(connection_graph, 0, 'Prepare/connection_graph.pl').
defined(connection_graph, 1, 'Prepare/connection_graph.pl').
defined(connect_weak, 1, 'Prepare/connection_graph.pl').
defined(connect_extend_weak, 2, 'Prepare/connection_graph.pl').
defined(connect_clauses, 1, 'Prepare/connection_graph.pl').
defined(connect_literals, 1, 'Prepare/connection_graph.pl').
defined(connect_one_literal, 2, 'Prepare/connection_graph.pl').
defined(connect_clauses_all, 1, 'Prepare/connection_graph.pl').
defined(connect_literals_all, 1, 'Prepare/connection_graph.pl').
defined(connect_one_literal_all, 2, 'Prepare/connection_graph.pl').
defined(remove_unreached_clauses, 0, 'Prepare/connection_graph.pl').
defined(apply_reductions, 0, 'Reductions/reductions.pl').
defined(print_reduction_statistics, 0, 'Reductions/reductions.pl').
defined(reorder_literals, 0, 'Reductions/reorder.pl').
defined(reorder_clauses, 0, 'Reductions/reorder.pl').
defined(pure_p, 3, 'Reductions/red_pure.pl').
defined(red_pure, 0, 'Reductions/red_pure.pl').
defined(red_pure, 1, 'Reductions/red_pure.pl').
defined(red_pure, 2, 'Reductions/red_pure.pl').
defined(info, 3, 'ProCom/procom.pl').
defined(define_reorder, 1, 'ProCom/procom.pl').
defined(define_prover, 1, 'ProCom/procom.pl').
defined(main_read_matrix_hook, 0, 'ProCom/procom.pl').
defined(select_prover, 1, 'ProCom/procom.pl').
defined(procom, 1, 'ProCom/procom.pl').
defined(load_module, 3, 'ProCom/procom.pl').
defined(procom_compile, 1, 'ProCom/procom.pl').
defined(procom_check, 1, 'ProCom/procom.pl').
defined(force_option, 2, 'ProCom/capri.pl').
defined(require_option, 2, 'ProCom/capri.pl').
defined(requirement, 1, 'ProCom/capri.pl').
defined(require_predicate, 1, 'ProCom/capri.pl').
defined(library_file, 1, 'ProCom/capri.pl').
defined(library_path, 1, 'ProCom/capri.pl').
defined(provide_definition, 1, 'ProCom/capri.pl').
defined(start_descriptor, 1, 'ProCom/capri.pl').
defined(end_descriptor, 1, 'ProCom/capri.pl').
defined('CaPrI Define Prover', 0, 'ProCom/capricore.pl').
defined('CaPrI Define Prover', 1, 'ProCom/capricore.pl').
defined('CaPrI Check', 1, 'ProCom/capricore.pl').
defined(check, 1, 'ProCom/capricore.pl').
defined(desc_get, 2, 'ProCom/capricore.pl').
defined('CaPrI Description Interpreter', 1, 'ProCom/capricore.pl').
defined(compile_autonomous, 2, 'ProCom/capricore.pl').
defined(compile_procedures, 2, 'ProCom/capricore.pl').
defined(number_clauses, 2, 'ProCom/capricore.pl').
defined(number_clauses_1, 2, 'ProCom/capricore.pl').
defined(collect_aux, 3, 'ProCom/capricore.pl').
defined(compile_desc, 5, 'ProCom/capricore.pl').
defined(compile_spec, 11, 'ProCom/capricore.pl').
defined(compile_template, 11, 'ProCom/capricore.pl').
defined(skip_index, 2, 'ProCom/capricore.pl').
defined(evaluate_get, 4, 'ProCom/capricore.pl').
defined(evaluate_get_predicates, 1, 'ProCom/capricore.pl').
defined(extract_diagonal, 2, 'ProCom/capricore.pl').
defined(extract_diagonal_from_template, 3, 'ProCom/capricore.pl').
defined(extract_diagonal_from_template_list, 3, 'ProCom/capricore.pl').
defined(extract_diagonal_from_template_map, 3, 'ProCom/capricore.pl').
defined(get_diagonal_goal, 2, 'ProCom/capricore.pl').
defined(get_diagonal_goal_succ, 2, 'ProCom/capricore.pl').
defined(initialize_proof_steps, 2, 'ProCom/capricore.pl').
defined(init_proof_steps, 2, 'ProCom/capricore.pl').
defined(get_step, 3, 'ProCom/capricore.pl').
defined(add, 3, 'ProCom/capricore.pl').
defined(weight, 2, 'ProCom/capricore.pl').
defined('Link Path', 1, 'ProCom/linker.pl').
defined('Link File', 1, 'ProCom/linker.pl').
defined('Provide Predicate', 3, 'ProCom/linker.pl').
defined('Require Predicate', 3, 'ProCom/linker.pl').
defined('Expand Predicate', 1, 'ProCom/linker.pl').
defined('Def', 3, 'ProCom/linker.pl').
defined(initialize_linker, 0, 'ProCom/linker.pl').
defined(initialize_linker, 1, 'ProCom/linker.pl').
defined(add_link_path, 1, 'ProCom/linker.pl').
defined(add_link_path, 2, 'ProCom/linker.pl').
defined(find_file, 2, 'ProCom/linker.pl').
defined(add_link_files, 1, 'ProCom/linker.pl').
defined(add_link_files_from_options, 1, 'ProCom/linker.pl').
defined(add_link_file, 1, 'ProCom/linker.pl').
defined(analyze_library, 2, 'ProCom/linker.pl').
defined(provide_definition, 2, 'ProCom/linker.pl').
defined(require_predicate, 1, 'ProCom/linker.pl').
defined(require_predicate, 2, 'ProCom/linker.pl').
defined(provide_predicate, 1, 'ProCom/linker.pl').
defined(provide_predicate, 2, 'ProCom/linker.pl').
defined(expand_predicate, 1, 'ProCom/linker.pl').
defined(determine_files, 3, 'ProCom/linker.pl').
defined(link_runtime_system, 0, 'ProCom/linker.pl').
defined(dynamic_linker, 1, 'ProCom/linker.pl').
defined(static_linker, 1, 'ProCom/linker.pl').
defined(static_link, 1, 'ProCom/linker.pl').
defined(immediate_link_from_option, 1, 'ProCom/linker.pl').
defined(put_optimized_clause, 1, 'ProCom/optimize.pl').
defined(put_simplified_clause, 2, 'ProCom/optimize.pl').
defined(put_simplified_clause, 1, 'ProCom/optimize.pl').
defined(simplify, 2, 'ProCom/optimize.pl').
defined(simplify_arithm, 2, 'ProCom/optimize.pl').
defined(decompose_unify, 4, 'ProCom/optimize.pl').
defined('Definition', 3, 'ProCom/optimize.pl').
defined(store_definition, 3, 'ProCom/optimize.pl').
defined(clear_definitions, 0, 'ProCom/optimize.pl').
defined(get_clause, 2, 'ProCom/optimize.pl').
defined(get_clause_from_proc, 3, 'ProCom/optimize.pl').
defined(is_meta, 1, 'ProCom/optimize.pl').
defined(merge_conjunction, 3, 'ProCom/optimize.pl').
defined(split_conjunction, 3, 'ProCom/optimize.pl').
defined(merge_disjunction, 3, 'ProCom/optimize.pl').
defined(compare_extensions, 4, 'ProCom/p--simple.pl').
defined(compare_prolog_clauses, 2, 'ProCom/p--simple.pl').
defined(term_vars, 2, 'ProCom/p--simple.pl').
defined(term_vars, 3, 'ProCom/p--simple.pl').
defined(term_vars, 4, 'ProCom/p--simple.pl').
defined(conjunction_length, 2, 'ProCom/p--simple.pl').
defined(init_search, 0, 'ProCom/p__depth_first.pl').
defined(init_search, 0, 'ProCom/p__iterative_broadening.pl').
defined(init_search, 0, 'ProCom/p__iterative_deepening.pl').
defined(init_search, 0, 'ProCom/p__iterative_inferences.pl').
defined(init_search, 0, 'ProCom/p__iterative_widening.pl').
defined(proof_steps, 3, 'ProCom/p_body.pl').
defined(reset_proof_steps, 0, 'ProCom/p_body.pl').
defined(put_proof_steps, 3, 'ProCom/p_body.pl').
defined(compile_body, 6, 'ProCom/p_body.pl').
defined(compile_body, 7, 'ProCom/p_body.pl').
defined(compile_body_1, 7, 'ProCom/p_body.pl').
defined(compile_body_2, 8, 'ProCom/p_body.pl').
defined('CaPrI Check', 1, 'ProCom/p_check_desc.pl').
defined(check, 1, 'ProCom/p_check_desc.pl').
defined(make_driver, 3, 'ProCom/p_driver.pl').
defined(make_driver_clauses, 3, 'ProCom/p_driver.pl').
defined(make_cl, 8, 'ProCom/p_driver.pl').
defined(compile_goal, 0, 'ProCom/p_goal.pl').
defined(compile_goals, 5, 'ProCom/p_goal.pl').
defined(compile_one_goal, 6, 'ProCom/p_goal.pl').
defined(strip_body, 2, 'ProCom/p_goal.pl').
defined(prepare_goal_list, 2, 'ProCom/p_goal.pl').
defined(prepare_goal_list, 3, 'ProCom/p_goal.pl').
defined(prepare_goal_list, 4, 'ProCom/p_goal.pl').
defined('Need Path', 1, 'ProCom/p_info.pl').
defined('Need Reduction', 1, 'ProCom/p_info.pl').
defined(use_path, 1, 'ProCom/p_info.pl').
defined(use_reductions, 1, 'ProCom/p_info.pl').
defined(require_options, 1, 'ProCom/p_options.pl').
defined(require_opts, 1, 'ProCom/p_options.pl').
defined(check_requirement, 1, 'ProCom/p_options.pl').
defined(force_options, 1, 'ProCom/p_options.pl').
defined(apply_options, 2, 'ProCom/p_options.pl').
defined(get_functor, 1, 'ProCom/p_options.pl').
defined(negate_functor, 2, 'ProCom/p_predicate.pl').
defined(is_negative, 1, 'ProCom/p_predicate.pl').
defined(build_functor, 3, 'ProCom/p_predicate.pl').
defined(make_functor, 3, 'ProCom/p_predicate.pl').
defined(make_functor_from_template, 4, 'ProCom/p_predicate.pl').
defined(make_template, 3, 'ProCom/p_predicate.pl').
defined(make_internal_rep, 2, 'ProCom/p_predicate.pl').
defined(make_literal, 8, 'ProCom/p_predicate.pl').
defined(unify_arguments, 4, 'ProCom/p_predicate.pl').
defined(translate_head, 3, 'ProCom/p_predicate.pl').
defined(translate_head, 6, 'ProCom/p_predicate.pl').
defined(translate_head, 7, 'ProCom/p_predicate.pl').
defined(put_open, 1, 'ProCom/p_put.pl').
defined(put_close, 0, 'ProCom/p_put.pl').
defined(put_matrix, 0, 'ProCom/p_put.pl').
defined(put_nl, 0, 'ProCom/p_put.pl').
defined(puts, 3, 'ProCom/p_put.pl').
defined(puts, 2, 'ProCom/p_put.pl').
defined(puts, 1, 'ProCom/p_put.pl').
defined('Put List', 2, 'ProCom/p_put.pl').
defined(put_report, 2, 'ProCom/p_put.pl').
defined(put_report, 3, 'ProCom/p_put.pl').
defined(put_report, 4, 'ProCom/p_put.pl').
defined(put_report, 5, 'ProCom/p_put.pl').
defined(put_boxed, 2, 'ProCom/p_put.pl').
defined(put_clause, 1, 'ProCom/p_put.pl').
defined(put_clause, 2, 'ProCom/p_put.pl').
defined(apply_vars, 1, 'ProCom/p_put.pl').
defined(put_clause_to_stream, 2, 'ProCom/p_put.pl').
defined(put_body_to_stream, 5, 'ProCom/p_put.pl').
defined(put_d_i_on_stream, 5, 'ProCom/p_put.pl').
defined(put_disjunction_to_stream, 5, 'ProCom/p_put.pl').
defined(indent_to, 2, 'ProCom/p_put.pl').
defined(get_meta_goals, 4, 'ProCom/p_put.pl').
defined(get_meta_goals, 2, 'ProCom/p_put.pl').
defined(get_meta_vars, 3, 'ProCom/p_put.pl').
defined(get_meta_vars_from_list, 3, 'ProCom/p_put.pl').
defined(get_attribute, 2, 'ProCom/p_put.pl').
defined(reorder, 4, 'ProCom/p_reorder.pl').
defined(apply_unsorted_definitions, 3, 'ProCom/unfold.pl').
defined(get_pattern, 2, 'ProCom/unfold.pl').
defined(apply_definitions, 3, 'ProCom/unfold.pl').
defined(apply_definitions_once, 3, 'ProCom/unfold.pl').
defined(apply_one_definition, 5, 'ProCom/unfold.pl').
defined(get_literal, 4, 'ProCom/unfold.pl').
defined(info, 3, 'Otter/otter.pl').
defined(otter, 1, 'Otter/otter.pl').
defined(otter, 0, 'Otter/otter.pl').
defined(otter_determine_files, 2, 'Otter/otter.pl').
defined(otter_preamble, 1, 'Otter/otter.pl').
defined(otter_numeric_flag, 3, 'Otter/otter.pl').
defined(otter_boolean_flag, 3, 'Otter/otter.pl').
defined(otter_flag, 2, 'Otter/otter.pl').
defined(otter_save_matrix, 1, 'Otter/otter.pl').
defined(otter_save_literal, 2, 'Otter/otter.pl').
defined('otter proof', 3, 'Otter/otter.pl').
defined('otter proof', 1, 'Otter/otter.pl').
defined(otter_analyze, 2, 'Otter/otter.pl').
defined(otter_analyze, 3, 'Otter/otter.pl').
defined(otter_analyze_proof, 2, 'Otter/otter.pl').
defined(otter_analyze_proof_step, 1, 'Otter/otter.pl').
defined(otter_analyze_proof_term, 4, 'Otter/otter.pl').
defined(otter_analyze_proof_term_subproofs, 2, 'Otter/otter.pl').
defined(otter_analyze_times, 2, 'Otter/otter.pl').
defined(info, 3, 'Setheo/setheo.pl').
defined(setheo, 0, 'Setheo/setheo.pl').
defined(setheo_determine_files, 4, 'Setheo/setheo.pl').
defined(setheo_save_matrix, 1, 'Setheo/setheo.pl').
defined(analyze_setheo, 2, 'Setheo/setheo.pl').
defined(analyze_setheo_proof, 2, 'Setheo/setheo.pl').
defined(analyze_setheo_proof_step, 2, 'Setheo/setheo.pl').
defined(convert_literal, 2, 'Setheo/setheo.pl').
defined(collect_flags, 2, 'Setheo/setheo.pl').
defined(info, 3, 'Pool/pool.pl').
defined(pool, 0, 'Pool/pool.pl').
defined(solve_pool, 3, 'Pool/pool.pl').
defined(select_hook, 3, 'Pool/pool.pl').
defined(solve_hook, 4, 'Pool/pool.pl').
defined(select_literal, 3, 'Pool/pool.pl').
defined(solve_literal, 4, 'Pool/pool.pl').
defined(sound_member, 2, 'Pool/pool.pl').
defined(set_depth_bound, 4, 'Pool/pool.pl').
defined(protop_version, 1, 'ProTop/protop.cfg').
defined(protop_home, 1, 'ProTop/protop.cfg').
defined(prover_system_path, 1, 'ProTop/protop.cfg').
defined(default_option, 1, 'ProTop/protop.cfg').
defined(pt_command, 1, 'ProTop/protop.cfg').
defined(define_goal_completion, 1, 'Prepare/prepare.cfg').
defined(define_reductions, 1, 'Prepare/prepare.cfg').
defined(define_prover, 1, 'ProCom/procom.cfg').
defined(define_search, 1, 'ProCom/procom.cfg').
defined(define_reorder, 1, 'ProCom/procom.cfg').
defined(define_otter, 1, 'Otter/otter.cfg').
defined(define_setheo, 1, 'Setheo/setheo.cfg').
defined(add_path, 1, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
defined(add_suffix, 1, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
defined(streams, 0, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
defined(stream, 1, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
defined(read_line, 2, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
defined(read_line, 1, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
defined(between, 3, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
defined(stat, 1, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
defined(time, 1, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
defined(time_body, 2, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
defined(ptags_all, 0, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
defined(write_history, 0, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
defined(write_history, 2, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
defined(compiled, 0, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
defined(list_error, 3, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
defined(compile_selection, 0, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
defined(compile_selection, 1, '/usr/local/lib/eclipse/3.4.5/lib/util.pl').
defined(maplist, 3, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
defined(checklist, 2, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
defined(member, 2, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
defined(memberchk, 2, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
defined(append, 3, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
defined(nonmember, 2, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
defined(delete, 3, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
defined(intersection, 3, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
defined(subtract, 3, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
defined(union, 3, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
defined(subset, 2, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
defined(list_check, 3, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
defined(length, 2, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
defined(length, 3, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
defined(length1, 2, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
defined(maplist_body, 4, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
defined(checklist_body, 3, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
defined(flatten, 2, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
defined(flatten, 3, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
defined(reverse, 2, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
defined(reverse, 3, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
defined(print_list, 1, '/usr/local/lib/eclipse/3.4.5/lib/lists.pl').
defined(numbervars, 3, '/usr/local/lib/eclipse/3.4.5/lib/numbervars.pl').
defined(numbervars, 5, '/usr/local/lib/eclipse/3.4.5/lib/numbervars.pl').
defined(append, 3, '/usr/local/lib/eclipse/3.4.5/lib/strings.pl').
defined(length, 2, '/usr/local/lib/eclipse/3.4.5/lib/strings.pl').
defined(length, 3, '/usr/local/lib/eclipse/3.4.5/lib/strings.pl').
defined(append_strings, 3, '/usr/local/lib/eclipse/3.4.5/lib/strings.pl').
defined(concat_chk, 3, '/usr/local/lib/eclipse/3.4.5/lib/strings.pl').
defined(var_or_string, 1, '/usr/local/lib/eclipse/3.4.5/lib/strings.pl').
defined(substring, 4, '/usr/local/lib/eclipse/3.4.5/lib/strings.pl').
defined(halve, 4, '/usr/local/lib/eclipse/3.4.5/lib/sorts.pl').
defined(merge, 5, '/usr/local/lib/eclipse/3.4.5/lib/sorts.pl').
defined(compare, 5, '/usr/local/lib/eclipse/3.4.5/lib/sorts.pl').
defined(compare, 4, '/usr/local/lib/eclipse/3.4.5/lib/sorts.pl').
defined(combine, 3, '/usr/local/lib/eclipse/3.4.5/lib/sorts.pl').
defined(keysort, 2, '/usr/local/lib/eclipse/3.4.5/lib/sorts.pl').
defined(msort, 2, '/usr/local/lib/eclipse/3.4.5/lib/sorts.pl').
defined(sort, 2, '/usr/local/lib/eclipse/3.4.5/lib/sorts.pl').
defined(merge, 3, '/usr/local/lib/eclipse/3.4.5/lib/sorts.pl').
defined(prune_instances, 2, '/usr/local/lib/eclipse/3.4.5/lib/sorts.pl').
defined(prune, 3, '/usr/local/lib/eclipse/3.4.5/lib/sorts.pl').
defined(prune1, 3, '/usr/local/lib/eclipse/3.4.5/lib/sorts.pl').
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
