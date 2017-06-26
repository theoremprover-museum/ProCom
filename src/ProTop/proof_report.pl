%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: proof_report.pl,v 1.12 1995/05/01 19:47:03 gerd Exp $
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

:- module_interface(proof_report). /*%-----------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.12 $}

This module analyzes a log file of \ProTop{} and generates a \LaTeX{} file
from it.

\PL*/
:- export proof_report/1,
	  proof_report/2,
	  proof_report/3,
	  proof_report_latex/0,
	  proof_report_latex/1,
	  proof_report_define_table/2,
	  proof_report_remove_table/1.

:- begin_module(proof_report).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:-	lib(lists),
	lib(os),
	lib(literal),
	lib(options),
	lib(message),
	lib(strings),
	lib(proof_tree),
	lib(report_summary).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The information from the log file is stored in the Prolog data base
temporarily. For this purpose we need some predicates to be dynamic. Those are
declared now.

\PL*/
:- dynamic log_flag/1,
	   log_author/1,
	   log_title/1,
	   log_preamble/1,
	   log_label/1,
	   log_section/1,
	   log_matrix/2,
	   log_contrapositive/1,
	   log_option/1,
	   log_proof/1,
	   log_time/2,
	   log_text/1,
	   log_comment/1,
	   log_info/2,
	   log_hostinfo/2,
	   log_is_init/0.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- define_option 'report:file'		 = "report.tex".
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The option |report:file| determines which \LaTeX{} file to generate by
|generate_report|.

\PL*/
:- define_option 'report:ignore_options' = ['run:remove_prover', 
					    match('ProTop'),
					    match(verbose),
					    match('report:'),
					    constraint_theory_file,
					    log_file,
					    output_file].
:- define_option 'report:style_options'	 = ["protop"].
:- define_option 'report:style_path'	 = "./inputs".
:- define_option 'report:style'		 = "article".
:- define_option 'report:latex'		 = "latex".
:- define_option 'report:use_latex209'	 = on.
:- define_option 'report:flags'		 = [preamble,
					    titlepage,
					    text,
					    comment,
					    time,
					    options,
					    matrix,
					    tree].
:- define_option 'report:sections'	 = [comment,
					    text,
					    matrix,
					    options,
					    times,
					    proof].
:- define_option 'report:tree_flags'	 = [dx(100),dy(100),tree,info].

:- dynamic summary_spec/2.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate proof_report_define_table/2().

\PL*/
proof_report_define_table(Name,Spec) :-
	retract_all(summary_spec(Name,_)),
	assert(summary_spec(Name,Spec)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate proof_report_remove_table/1().

\PL*/
proof_report_remove_table(Name) :-
	retract_all(summary_spec(Name,_)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate proof_report/1(Files).

\PL*/
proof_report(Files) :-
	is_option('report:file',Report),
	is_option('report:flags',Flags),
	proof_report(Files,Report,Flags).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate proof_report/2(File,Report).

\PL*/
proof_report(File,Report) :-
	is_option('report:flags',Flags),
	proof_report(File,Report,Flags).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate proof_report/3(File,Report,Flags).

\PL*/
proof_report(File,Report,Flags) :-
	retract_all(log_is_init),
	retract_all(log_flag(_)),
	retract_all(log_info(_,_)),
	retract_all(log_hostinfo(_,_)),
	retract_all(log_author(_)),
	retract_all(log_title(_)),
	retract_all(log_preamble(_)),
	summary_clear,
	log_clear_single,
	(   member(F,Flags),
	    assert(log_flag(F)),
	    fail
	;   true
	),
	make_report_init(Report,ReportStream),
	( functor(File,'.',2) ->
	    (	member(F,File),
		report_read_file(F,ReportStream),
		fail
	    ;	true
	    )
	; File == [] ->
	    true
	;   report_read_file(File,ReportStream)
	),

	make_report_tables(ReportStream),

	make_report_exit(ReportStream),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate proof_report_latex/0().

\PL*/
proof_report_latex :-
	is_option('report:file',ReportFile),
	proof_report_latex(ReportFile).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate proof_report_latex/1(+File).

\PL*/
proof_report_latex(ReportFile) :-
	ReportFile \== [],
	(   atom(ReportFile)
	;   string(ReportFile)
	),
	is_option('report:style_path',Path),
	is_option('report:latex',LaTeX),
	execute_ignore([ "csh -fc 'setenv TEXINPUTS \"",
			 Path,
			 ":$TEXINPUTS\" ;",
			 LaTeX,
			 " \"\\nonstopmode\\input ",
			 ReportFile,
			 "\"'"
			  ]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate make_report_tables/1(+Stream).

\PL*/
make_report_tables(Stream) :-
	(   summary_spec(_,Spec),
	    printf("+%b",[]),
	    make_summary_table(Spec,Stream),
	    fail
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate make_report_init/2().

\PL*/
make_report_init(File,Stream) :-
	open(File,write,Stream),
	date(Date),
	getenv("USER",User),
	printf(Stream,
	       "%%%%%% %w\n%%%%%% Creator: %w\n%%%%%% Date:     %w",
	       ["ProTop Proof Report",User,Date]),
	is_option('report:style_path',Path),
	printf(Stream,"%%%%%%------------------------------------------\n",[]),
	printf(Stream,"%%%%%% Style path: %w\n\n",[Path]),

	is_option('report:style_options',StylesList),
	collect_options(StylesList,SL),
	concat_string(SL,Styles),
	is_option('report:style',Style),

	( is_option('report:use_latex209') ->
	    printf(Stream,
		   "\\documentstyle[%w]{%w}\n\n\\begin{document}\n\n",
		   [Styles,Style])
	;
	    printf(Stream,"\\documentclass{%w}\n",[Style]),
	    (	member(Package,Styles),
		printf(Stream,"\\usepackage{%w}\n",[Package]),
		fail
	    ;	true
	    ),
	    printf(Stream,"\\\n\\begin{document}\n\n",[])
	),
	(   log_flag(preamble),
	    log_preamble(Pre),
	    writeln(Stream,Pre),
	    fail
	;   true
	),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate collect_options/2().

\PL*/
collect_options([],[]).
collect_options([H],[H]) :-
	!.
collect_options([H|T],[H,","|S]) :-
	collect_options(T,S).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate make_report_exit/1().

\PL*/
make_report_exit(Stream) :-
	printf(Stream,"\n\\end{document}\n",[]),
	close(Stream),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate report_read_file/2().

\PL*/
report_read_file(File,ReportStream) :-
	File \== [],
	(   atom(File)
	;   string(File)
	),
	!,
	( exists(File) ->
	    true
	;   err("*** File ",File," not found."),
	    fail
	),
	printf("[%w",[File]),
	open(File,read,Stream),
	repeat,
	read(Stream,Term),
	( Term = end_of_file ->
	    true
	;   log_do(Term,Stream,ReportStream),
	    fail
	),
	close(Stream),
	printf("]\n",[]),
	!.
report_read_file(File,_) :-
	err("*** Type error for file name:",File),
	fail.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate log_clear_single/0().

\PL*/
log_clear_single :-
	retract_all(log_matrix(_,_)),
	retract_all(log_option(_)),
	retract_all(log_proof(_)),
	retract_all(log_time(_,_)),
	retract_all(log_section(_)),
	retract_all(log_label(_)),
	retract_all(log_comment(_)),
	retract_all(log_text(_)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate report_push/0().

\PL*/
report_push :-
	( log_label(Label) ->
	    (	log_section(A),
		summary_store(Label,section,A),
		fail
	    ;	log_time(Type,Time),
		summary_store(Label,time(Type),Time),
		fail
	    ;	true
	    )
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate log_do/3(Type,Stream).

\PL*/
log_do(begin(Token),Stream,_)	:-
	!,
	log_do_block(Token,Stream).	
log_do(log_info(A,B),_,_)	:- 
	!,
	assert(log_info(A,B)).
log_do(host_info(A,B),_,_)	:-
	!,
	assert(log_hostinfo(A,B)).
log_do(title(Title),_,_)	:-
	!,
	assert(log_title(Title)).
log_do(author(Author),_,_)	:-
	!,
	assert(log_author(Author)).
log_do(section(Section),_,_)	:-
	!,
	assert(log_section(Section)).
log_do(label(Label),_,_)	:-
	!,
	assert(log_label(Label)).
log_do(text(Text),_,_)		:-
	log_flag(text),
	!,
	assert(log_text(Text)).
log_do(comment(Comment),_,_)	:- 
	log_flag(comment),
	!, 
	assert(log_comment(Comment)).
log_do(time(L,Time),_,_)	:-
	log_flag(time),
	!,
	assert(log_time(L,Time)).
log_do(runtime(Time),_,_)	:- 
	log_flag(time),
	!,
	assert(log_time(runtime,Time)).
log_do(proof(A,B),_,ReportStream) :- 
	!,
	assert(log_proof(proof(A,B))),
	make_report(ReportStream).
log_do(timeout,_,ReportStream)	 :-
	!,
	assert(log_time(runtime,timeout)),
	make_report(ReportStream).
log_do(_,_,_).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate log_do_block/2(Type, Stream).

\PL*/
log_do_block(matrix,Stream) :- 
	log_flag(matrix),
	!,
	log_do_block_generic(matrix,
			     Stream,
			     clause(Index,Clause),
			     assert(log_matrix(Index,Clause))).
log_do_block(options,Stream) :- 
	log_flag(options),
	!,
	log_do_block_generic(options,
			     Stream,
			     Opt,
			     assert(log_option(Opt))).

log_do_block(Token,Stream)	:- 
	log_do_block_generic(Token,Stream,_,fail).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate log_do_block_generic/4(Token,Stream,Term,Goal).

\PL*/
log_do_block_generic(Token,Stream,Term,Goal) :-
	repeat,
	read(Stream,Term0),
	( Term0 == end_of_file ->
	    true
	; Term0 = end(Token) ->
	    true
	;   Term = Term0,
	    once(Goal),
	    fail
	),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate make_report/1(+Stream).

\PL*/
make_report(Stream) :-
	printf(".%b",[]),
	( log_is_init ->
	    true
	; log_flag(titlepage) ->
	    assert(log_is_init),
	    ( log_title(Title) ->
		printf(Stream,"\\Title{%w}\n",[Title])
	    ;	printf(Stream,"\\Title{Proof Report}\n",[])
	    ),
	    ( log_author(Author) ->
		printf(Stream,"\\Author{%w}\n",[Author])
	    ; log_info(User,_) ->
		printf(Stream,"\\Author{%w}\n",[User])
	    ;	true
	    ),
	    ( log_info(_,D) ->
		substring(D,1,24,Date),
		printf(Stream,"\\Date{%w}\n",[Date])
	    ;	printf(Stream,"\\Date{Printed \\today}\n",[])
	    ),
	    printf(Stream,"\n\\MakeTitle\n\n",[])
	;
	    assert(log_is_init)
	),

	( log_section(Section) -> true; Section = ""),
	printf(Stream,"\n\\begin{Experiment}{%w}\n",[Section]),

	is_option('report:sections',Sections),
	make_report_experiment(Sections,Stream),
	printf(Stream,"\\end{Experiment}\n",[]),
	!,
	report_push,
	log_clear_single.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate make_report_experiment/2(+List, +Stream).

\PL*/
make_report_experiment([],_).
make_report_experiment([H|T],Stream) :-
	make_report(H,Stream),
	make_report_experiment(T,Stream).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate make_report/2(+Type, +Stream).

\PL*/
make_report(proof,Stream) :-
	( log_flag(tree),
	  log_proof(Proof) ->
	    is_option('report:tree_flags',Flags),
	    proof_tree(Proof,Flags,Stream)
	;   true
	).
make_report(text,Stream) :-
	( log_flag(text) ->
	    make_report_text(Stream,log_text,"Text")
	;   true
	).

make_report(comment,Stream) :-
	(   log_comment(C),
	    printf(Stream,"%% %w\n",[C]),
	    fail
	;   true
	).
make_report(matrix,Stream) :-
	( log_matrix(_,_) ->
	    printf(Stream," \\begin{Clauses}\n",[]),
	    (	log_matrix(_I,C),
		( C = (?- Clause) ->
		    printf(Stream,"  \\Clause{?-}",[])
		;   printf(Stream,"  \\Clause{}",[]),
		    Clause = C
		),
		make_report_clause(Clause,"",Stream),
		fail
	    ;	true
	    ),
	    printf(Stream," \\end{Clauses}\n",[])
	;   true
	).

make_report(times,Stream) :-
	printf(Stream," \\begin{TimeTable}\n",[]),
	(   log_time(Name,Time),
	    ( Name == runtime ->
		N = "Runtime:"
	    ;	N = Name
	    ),
	    ( Time == timeout ->
		T = "\\Timeout"
	    ;	T = Time
	    ),
	    printf(Stream,"  \\Time{%w}{%w}\n",[N,T]),
	    fail
	;   true
	),
	printf(Stream," \\end{TimeTable}\n",[]).

make_report(options,Stream) :-
	( log_flag(options) ->
	    printf(Stream," \\begin{Options}\n",[]),
	    (	log_option(Opt=Val),
		( once(ignore_option(Opt,Val)) ->
		    printf(Stream,"  \\DefaultOption{%w}{%q}\n",[Opt,Val])
		;   printf(Stream,"  \\Option{%w}{%q}\n",[Opt,Val])
		),
		fail
	    ;	true
	    ),
	    printf(Stream," \\end{Options}\n",[])
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate ignore_option/2(Option, Value).

\PL*/
ignore_option(Option,Value) :-
	call(option_name(Option,Value),options).
ignore_option(Option,Value) :-
	is_option('report:ignore_options',Opts),
	member(Cand,Opts),
	(   Cand = (Option=Value)
	;   Cand = Option
	;   Cand = match(Pattern),
	    atom(Option),
	    concat_string([Option],OptString),
	    concat_string([Pattern],PatternString),
	    substring(OptString,PatternString,_)	    
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate make_report_text/3(+Stream, +Token, +Env).

\PL*/
make_report_text(Stream,Token,Env) :-
	Goal =.. [Token,T],
	(   \+ \+ Goal ->
	    printf(Stream," \\begin{%w}\n",[Env]),
	    (	Goal,
		make_report_text_loop(T,Stream),
		fail
	    ;	true
	    ),
	    printf(Stream," \\end{%w}\n",[Env])
	;   true
	).
make_report_text_loop([],_) :- !.
make_report_text_loop([H|T],Stream) :-
	printf(Stream,"	 %w\n",[H]),
	make_report_text_loop(T,Stream).
make_report_text_loop(String,Stream) :-
	string(String),
	printf(Stream,"	 %w\n",[String]).
make_report_text_loop(Atom,Stream) :-
	atom(Atom),
	printf(Stream,"	 %w\n",[Atom]).
make_report_text_loop(insert(File,Env),Stream) :-
	printf(Stream,"\\begin{%w}\n",[Env]),
	make_report_text_loop(insert(File),Stream),
	printf(Stream,"\\end{%w}\n",[Env]).
make_report_text_loop(insert(File),Stream) :-
	(   string(File)
	;   atom(File)
	),
	exists(File),
	open(File,read,InStream),
	repeat,
	( at_eof(InStream) ->
	    true
	;   read_string(InStream,"\n",_,S),
	    writeln(Stream,S),
	    fail
	),
	close(InStream),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate make_report_clause/3(+LiteralList, +Prefix, +Stream).

\PL*/
make_report_clause([],_,_).
make_report_clause([literal(H,_I)|T],Prefix,Stream) :-
	( H = (-- L) ->
	    Sign = "\\n\eg "
	; H = (++ L) ->
	    Sign = ""
	;   Sign = "",
	    L = H
	),
	printf(Stream,"%w[%s%w]%%\n",[Prefix,Sign,L]),
	make_report_clause(T,"\t   ",Stream).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
