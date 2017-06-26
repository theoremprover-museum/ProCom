%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: procom.pl,v 1.43 1995/03/20 21:24:47 gerd Exp $
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

:- module_interface(procom). /*%-----------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.43 $}


\PL*/
:- export procom/1,
	  procom/0. 

:- begin_module(procom).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The module has to be identified for \ProTop.

\PL*/
info(prover,
	"$Revision: 1.43 $",
	"ProCom is a prover based on the PTTP technique.").
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

First of all the required modules are loaded.

\PL*/
:-	lib(lists).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:-	lib(log),
	lib(time),
	lib(hook),
	lib(options),
	lib(message),
	lib(matrix),
	lib(linker),
	lib(find_file),
	lib(dynamic_module),
	lib(p_put),
	lib(p_options),
	lib(run_prover).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:-	lib(capricore).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In addition to the libraries we need to evaluate the configuration file. This
configuration file is named {\sf procom.cfg} and located in the current
directory. The configuration file contains facts for the predicate
|define_prover/1|. At least one of those facts is required.

The argument of |define_prover/1| is a Prolog symbol indicating the module
containing an additional prover. Those names need to be distingt.

In a failure driven loop the predicate |define_prover/1| is used to determine
the prover modules and load them as well.

\PL*/
:- dynamic define_reorder/1,
	   define_prover/1.

:-	compile('procom.cfg').

:-	(   define_prover(ProverModule),
	    use_module(library(ProverModule)),
	    fail
	;   define_search(SearchModule),
	    concat_atom(['p__',SearchModule],File),
	    use_module(library(File)),
	    fail
	;   define_reorder(ReorderModule),
	    concat_atom(['p--',ReorderModule],File),
	    use_module(library(File)),
	    fail
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\subsection{Options of ProCom}

Procom makes use of several options. Some of them are defined globally in
other modules.

The options defined in this module start with the prefix |ProCom:|.  Those
options having the prefix |ProCom::| contain file names to be used by the
linker. The options and their default values are described below.

\PL*/
:- define_option 'ProCom:verbose'	    = off.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The option |ProCom:verbose| enables the enrichment of the generated
Prolog code with comments. Those comments are suppressed if this
option is off.
\PL*/
:- define_option 'ProCom:link'		    = static.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The option |ProCom:link| enables the linking with library routines if
it is not turned |off|. It can take the value |static| to indicate
static linking, |shared| to indicate that only pointers to libraries
should be used, and |dynamic| to allow optimizations and macro expansion.
\PL*/
:- define_option 'ProCom:proof'		    = on.
:- define_option 'ProCom::proof'	    = 'proof.pl'.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The option |ProCom:proof| enables the generation of a proof term.
This takes some time and space and should be turned off when a fast
prover is required.
\PL*/
:- define_option 'ProCom:trace'		    = on.

:- define_option 'ProCom::trace'	    = 'no-debugger.pl'.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The option |ProCom:trace| compiles in code for the debugger.
Tracing of the depth bound, spying and other debugging facilities can
be achieved with this switch.
\PL*/
:- define_option 'ProCom:module'	    = off.
:- define_option 'ProCom::module'	    = 'module.pl'.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The option |ProCom:module| forces the generation of a module head
in the Prolog code. Maybe this will be required to avoid confusion.
\PL*/
:- define_option 'ProCom:ancestor_pruning'  = on.
:- define_option 'ProCom::ancestor_pruning' = 'prune.pl'.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The option |ProCom:ancestor_pruning| enables generation of code
for identical ancestor pruning.
\PL*/
:- define_option 'ProCom::timing'	    = 'time.pl'.
:- define_option 'ProCom:time_limit'	    = off.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The option |ProCom:timing| enables generation of code for timing
of the generated prover.
\PL*/
:- define_option 'ProCom:show_proof'	    = on.
:- define_option 'ProCom:show_result'	    = on.
:- define_option 'ProCom::show_result'	    = 'show.pl'.
:- define_option 'ProCom:proof_limit'	    = off.
:- define_option 'ProCom::proof_limit'	    = 'proof_limit.pl'.
:- define_option 'ProCom::more'		    = 'more.pl'.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The option |ProCom:show_result| enables the generation of code to
present variable bindings of the goal and backtracking to next solutions.
\PL*/
:- define_option 'ProCom::path'		    = 'path-simple.pl'.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- define_option 'ProCom:occurs_check'	    = on.
:- define_option 'ProCom::unify'	    = 'unify.pl'.
:- define_option 'ProCom::unify_simple'	    = 'unify-no-oc.pl'.
:- define_option 'ProCom:unifier_argument'  = off.
:- define_option 'ProCom:set_variable'	    = off.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The option |ProCom::unify| contains the name of the library to be
linked when the |unify/2| predicate is required for sound unification
with occurs check.
\PL*/
:- define_option 'ProCom::member'	    = 'member.pl'.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The option |ProCom::member| contains the name of the library to be
linked when a sound member predicate is required.
\PL*/
:- define_option 'ProCom:literal_selection' = deterministic.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The predicate |ProCom::search/3| establishes a mapping from the search
strategy and the selection strategy to the a library file to use.  The
selection strategy is determined by the option
|ProCom:literal_selection|. Supported values are |deterministic| and |random|.
\PL*/
:- define_option 'ProCom:optimize' = on.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The option |ProCom:optimize| is intended to turn on/off code optimization.
\PL*/
:- define_option 'ProCom:expand'	    = on.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The option |ProCom:expand| controlls when predicates are 
expanded which are linked from a library.
\PL*/
:- define_option 'ProCom:expand_aux'	    = on.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The option |ProCom:expand_aux| controlls when auxiliary predicates are 
expanded.
\PL*/
:- define_option 'ProCom:dynamic_reordering' = off.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- define_option 'ProCom:reorder_ext'			= ''.
:- define_option 'ProCom:reorder_goal'			= ''.
:- define_option 'ProCom:reorder_clauses'		= ''.
:- define_option 'ProCom:reorder_goal_clauses'		= ''.
:- define_option 'ProCom:reorder_prolog_clauses'	= ''.
:- define_option 'ProCom:reorder_aux_clauses'		= ''.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- define_option 'ProCom:add_contrapositives'		= off.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The option |ProCom:add_contrapositives| controlls the output of of the
contrapositives into the compiled program. This may be neccessary if a dynamic
heuristic needs access to them.

\PL*/
:- define_option 'ProCom:automatic_put_on_path'		= on.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Most of the time it is desirable to put the current literal onto the path
automatically. This means the path is managed automatically. The option
|ProCom:automatic_put_on_path| can be used to turn off this behaviour. The
writer of a descriptor file can now put literals on the path in a controlled
way.

\PL*/
:- define_option 'ProCom:report_actions'		= off.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The option |ProCom:report_actions| controlls the generation of verbose reports
on the actions taken during the compilation.

\PL*/
:- define_option 'ProCom:link_path' = [ '.',
					'ProCom'
				      ].
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The option |ProCom:link_path| determines where the linker searches for it's
files. The value of the option |prolog| is appended to this option.

\PL*/
:- define_option 'ProCom::immediate_link'	= ['init.pl'].
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The option |ProCom::immediate_link| specifies a list of libraries which are
added to the code immediately after the initialization.

\PL*/
:- define_option 'ProCom::post_link'		= [].
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The option |ProCom::post_link| specifies a list of libraries which are
added to the end of the code.

\PL*/
:- define_option 'ProCom:ignore_link_errors'    = off.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- define_option 'ProCom:put_pretty'		= on.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- define_option 'ProCom:init_goal_list'	= [].
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
This option specifies a list of Prolog predicates which are called when a goal
is launched.

Note that you have to ensure that the predicates given are present in the
compiled Prolog program. This can be done by specifying appropriate libraries
containing the Prolog code.

\PL*/
:- define_option 'ProCom:init_level_list'	= [].
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
This predicate specifies a list of predicates which are called whenever a new
depth level is started, i.e. right after |set_depth_bound/1| has returned a
new depth. The depth is added as additional argument to the predicates given.

Note that you have to ensure that the predicates given are present in the
compiled Prolog program. This can be done by specifying appropriate libraries
containing the Prolog code.

\PL*/
:- define_option 'ProCom:post_goal_list'	= [].
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
This option specifies a list of Prolog predictes which are called after the
proof of a goal has been successful. As additional arguments the list of
variable bindings and the proof tree is added to the predicate given.

Consider the |ProCom:post_goal_list| containing the element |pgl(1,47)|.  Then
the additional goal |pgl(1,47,Vars,Proof)| is compiled into the prover, where
|Vars| and |Proof| are instantiated to the variable bindings and the proof
tree respectively.

Note that you have to ensure that the predicates given are present in the
compiled Prolog program. This can be done by specifying appropriate libraries
containing the Prolog code.

\PL*/
:- define_option 'ProCom::lemma'		= 'lemma.pl'.
:- define_option 'ProCom:lemma'			= off.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- define_option 'ProCom:extra_procedures'	= [].
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- define_option 'ProCom:capri_path'		= ['.'].
:- define_option 'ProCom:capri_extensions'	= ["",".pl"].
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- define_option 'ProCom:use_path'		= reduction.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- define_option 'ProCom::literal' 	        = 'literal_static.pl'.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- define_option 'ProCom:do' = prove.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\subsection*{Internal Flags}

Some options are internal to \ProCom{} and should not be visible
outside. Thus we use global variables instead of the options facility.

\PL*/
:-	setval('ProCom:add_depth_argument',on),
	setval('ProCom:depth_pass_through',off),
	setval('ProCom:need_weight',off).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


\Predicate main_read_matrix_hook/0().

This predicate is used to perform some actions after the matrix has
been read. For this purpose the hook |main_read_matrix| is used.
Two main tasks have to be performed.
\begin{itemize}
  \item A prover has to be selected.\\
	This is done according to the following scheme. First of all the
	prover given in the option |prover| is considered. If this prover
	fails to be applicable all provers defined with |define_prover/1| (in
	{\sf procom\_config.pl}) are tried in turn. If the required options are
	set (as defined with the instruction |require_option|) then the prover
	is applicable. Otherwise another one is tried.
  \item The selected prover has to be initialized.\\
	This initialization mainly involves the adjusting of options.
\end{itemize}

\begin{description}
  \item [Note:] 
	These actions are performed {\em after} the matrix has been
	read but {\em before} any preprocessing has been performed.
\end{description}

\PL*/ 
main_read_matrix_hook :-
	is_option(prover,P),
	functor(P,procom,_),
	( is_option('ProCom:ancestor_pruning') ->
	    set_option('ProCom:use_path'=on)
	;   true
	),
	set_option(remove_unreached_clauses=off),
	select_prover(Prover),
	force_options(Prover),
	!.

:- hook(main_read_matrix,main_read_matrix_hook/0).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate select_prover/1(-Prover).

This predicate tries to find an appropriate prover. For this purpose the
required options are checked. The predicate checks also some trivial cases of
misuse and issues an error if one is found.

Upon success the selected prover is returned. If necessary the option |prover|
is adjusted accordingly. If no prover can be used then this predicate fails
with an error message.
\PL*/
select_prover(Prover) :-
	is_option(prover,procom(Prover)),

	is_option('ProCom:capri_path',Path),
	is_option('ProCom:capri_extensions',Extensions),
	( \+ atom(Prover) ->
	    err("*** ProCom Error: The option prover is not an atom: ",Prover),
	    fail
	; define_prover(Prover) ->
	    true
	; load_module([Prover/0,require_option/2,force_option/2],
	    		Prover,capri,Path,Extensions,
	    		["--- ProCom: CaPrI module ",Prover," loaded.\n"]) ->
	    assert(define_prover(Prover))
        ),
	( \+ require_options(Prover) ->
	    msg("--- ProCom prover ",Prover," is not applicable."),
	    fail
	; \+ check_requirement(Prover) ->
	    msg("--- ProCom prover ",Prover,
	        " doesn't fulfill it's requirements."),
	    fail
	;   true
	),
	!.
select_prover(Prover) :-
	define_prover(Prover),
	require_options(Prover),
	check_requirement(Prover),
	!,
	msg("--- ProCom: Using alternate prover ",Prover),
	set_option(prover=procom(Prover)).
select_prover(_) :-
	msg("*** ProCom Error: No prover is applicable."),
	set_option(prover=procom(_)),
	fail.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate procom/0().

\PL*/
procom :-
	once(define_prover(Prover)),
	procom(Prover).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate procom/1(+Prover).

This predicate provides the main entry for any \ProCom{} type prover.  A
caller has to provide the matrix in the module {\sf matrix}. Then a call like

\begin{BoxedSample}
  procom(extension\_procedure)
\end{BoxedSample}

starts compilation of the current matrix using the descriptors in module
|extension_procedure| provided that this module is defined as a prover in {\sf
  procom.cfg}. Otherwise \ProCom{} tries to load an appropriate file
dynamically.

The output is written to the file declared in the option |output_file|.

\PL*/
procom(Name) :-
	atom(Name),
	is_option('ProCom:do',Action),
	( Action = compile ->
	    procom_compile(Name)
	; Action = run ->
	    is_option(output_file,File),
	    run_prover(File)
	; Action = check ->
	    procom_check(Name)
	;   procom_compile(Name),
	    is_option(output_file,File),
	    run_prover(File)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate procom_compile/1(+Name).

\PL*/
procom_compile(Name) :-
	atom(Name),
	( define_prover(Name) ->
	    true
	; current_module(Name) ->
	    msg("--- ProCom: Using dynamic module ",Name)
	;   err("*** ProCom Error: Prover ",Name," is not loaded."),
	    fail
	),
	is_option(output_file,Outfile),
	put_open(Outfile),
	put_nl,
	( is_option('ProCom:verbose') ->
	    put_matrix
	;   true
	),
	reset_time,
	(   call(Name,Name) ->
	    msg,
	    put_close
	;   
	    msg,
	    put_close,
	    err("*** ProCom Error: Compilation aborted abnormally."),
	    fail
	),
	log_reset_time_and_print("Compile time: ",""),
	nl.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Alternatively a consistency check can be performed on a loaded set of
descriptors. In this case some checks are performed which are usually
omitted for performace reasons. A call like

|procom(check(extension_procedure))|

perform the checks on the module |extension_procedure|.

\PL*/
procom_check(Name) :-
	atom(Name),
	( define_prover(Name) ->
	    'CaPrI Check'(Name)
	;   err("*** Error: Prover ",Name," is not loaded."),
	    fail
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
