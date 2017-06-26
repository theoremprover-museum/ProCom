%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: prove.pl,v 1.12 1995/07/03 11:35:12 gerd Exp gerd $
%%%============================================================================
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

:- module_interface(prove). /*
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.12 $}

\PL*/
:- export (prove)/2,
	  prove_log/1,
	  filter_module/1,
	  prover_module/1. 

:- begin_module(prove).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We need some external modules which are loaded first.

\PL*/
:-	lib(lists).

:-	lib(os),
	lib(log),
	lib(hook),
	lib(time),
	lib(unify),
	lib(matrix),
	lib(message),
	lib(literal),
	lib(options),
	lib(run_filter),
	lib(dynamic_module),
	lib(eq_member).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Load the configuration file to get the settings stored there.

\PL*/
:-	compile('prove.cfg').
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In a failure driven loop we load all modules requested as filters. For this
purpose the solutions of the predicate |filter_module/1| are used. This
predicate should be defined in {\sf prove.cfg}.

\PL*/
:-	(   filter_module(Filter),
	    declare_filter(Filter),
	    fail
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In a failure driven loop we load all modules requested as provers. For this
purpose the solutions of the predicate |prover_module/1| are used. This
predicate should be defined in {\sf prove.cfg}.

\PL*/
	;   prover_module(Prover),
	    lib(Prover),
	    fail
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This file uses the hooks |startup| and |main_read_matrix|. These hooks
are declared.

\PL*/
:- define_hook startup/0.
:- define_hook main_read_matrix/0.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following definitions are used to specify some options.

\PL*/
:- define_option prolog		    = eclipse.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The option |prolog is used for configuration. It names the target Prolog
dialect (to be used by compiling theorem provers).
\PL*/
:- define_option verbose	    = on.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The option |verbose| turns on general verbosity of actions. Several modules
may decide to uise their own verbosity options.
\PL*/
:- define_option search		    = iterative_deepening(1,1,1).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The option |search| indicates which search strategy should be used.
\begin{description}
\item [depth\_first]\ \\ 
  This search strategy uses the unbound depth first search like Prolog
  provides it.
\item [iterative\_deepening($\delta_0$, $\alpha$, $\beta$)]\ \\
  This search strategy successively increments a depth bound.  The search
  space is searched to this limit. The limit is incremented upon failure.
  \\ 
  The initial depth $d_0$\/ is $\delta_0$.  The next depth $d_{n+1}$\/ is
  computed from the previous depth $d_n$\/ according to the formula
  \[ d_{n+1} = \alpha\cdot d_n + \beta
  \]
  If $\alpha$\/ is $1$\/ then a linear function is achieved. If $\alpha$\/ is
  greater then $1$\/ then an exponential function can be forced.
\item [iterative\_deepening($\delta_0$, $\beta$)]\ \\
  This is the same as iterative\_deepening($\delta_0$, 1, $\beta$).
\item [iterative\_deepening($\delta_0$)]\ \\
  This is the same as iterative\_deepening($\delta_0$, 1, 1).
\item [iterative\_deepening]\ \\
  This is the same as iterative\_deepening(1, 1, 1).
\item [iterative\_inferences($\delta_0$, $\alpha$, $\beta$)]
  This search strategy successively increments a inference bound.  The search
  space is searched to this limit. The limit is incremented upon failure.
  \\
  The initial depth $d_0$\/ is $\delta_0$.  The next depth $d_{n+1}$\/ is
  computed from the previous depth $d_n$\/ according to the formula
  \[ d_{n+1} = \alpha\cdot d_n + \beta
  \]
  If $\alpha$\/ is $1$\/ then a linear function is achieved. If $\alpha$\/ is
  greater then $1$\/ then an exponential function can be forced.
\item [iterative\_inferences($\delta_0$, $\beta$)]\ \\
  This is the same as iterative\_inferences($\delta_0$, 1, $\beta$).
\item [iterative\_inferences($\delta_0$)]\ \\
  This is the same as iterative\_inferences($\delta_0$, 1, 1).
\item [iterative\_inferences]\ \\
  This is the same as iterative\_inferences(1, 1, 1).
\item [iterative\_widening(a,b,c)]\ \\
  This is an experimental mode implementing iterative widening according to
  \cite{ginsberg.harvey:iterative}.

  {\bf Not ready yet!}
\item [iterative\_broadening(a,b,c)]\ \\
  This is an experimental mode implementing iterative broadening. 
\end{description}

\PL*/
:- define_option prover		    = procom(extension_procedure).
:- define_option input_filter	    = [log_filter,mult_taut_filter].
:- define_option log_file	    = [].
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- define_option 'prove:log_items'  = [prover,matrix,contrapositives].
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- define_option 'prove:red_goals'  = [complete_goals,connection_graph].
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- define_option 'prove:red_path'   = ['.'].
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- define_option 'prove:path'       = ['.'].
:- define_option 'prove:extension'  = ['','.pl'].
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
These are automatic options. I.e.\ they are managed by the system and should
not be set by a program or in a configuration file. In fact setting this
variables in any way shoud be ignored (but who knows:-)
\PL*/
:- define_option input_file	    = ''.
:- define_option output_file	    = ''.
:- define_option equality	    = off.
:- define_option setvar		    = off.
:- define_option interactive	    = off.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate prove/2(InFile,OutFile).

This predicate performs all tasks required for proving a problem.

\PL*/
prove(InputFile,OutFile) :-
	is_option(prover,ProverTerm),
	functor(ProverTerm,Prover,Arity),
	get_flag(library_path,LPath),
	is_option('prove:path',PPath),
	append(PPath,LPath,Path),
	is_option('prove:extension',Extension),
	load_module([Prover/Arity],
		    Prover,
		    prover,
		    Path,
		    Extension,
		    ["Dynamic prover module ",Prover," has been loaded.\n"]),
   
	set_option(input_file  = InputFile),
	set_option(output_file = OutFile),
	run_hooks(startup),

	( read_phase(InputFile) ->
	    run_hooks(main_read_matrix),	    
	    ( is_option('ProTop:verbose') ->
		setof((O=V),is_option(O,V),Options),
		(   member(Opt,Options),
		    msg("% Option ",Opt),
		    fail
		;   msg
		)
	    ;	true
	    ),
	    ( prepare_phase ->
		is_option(prover,RealProver),
		functor(RealProver,Module,_),
		prove(RealProver,Module,Path,Extension)
	    ;   err("*** Preparation failed. Proving abandoned."),
	        fail
	    )
	;   err("*** Reading failed. Proving abandoned."),
	    fail
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate prove/4(+Prover, +Module, +Path, +Extension).

\PL*/
prove(Prover,Module,Path,Extension) :-
	( \+ 'Clause'(_) ->
	    msg("--- The matrix seems to be empty.\n",
	        "Runtime 0 ms\n Yes\n",
	        "--- In fact the prover has not even been started.\n"),
	    fail
	; 'ClauseLength'(_,0) ->
	    msg("--- An empty clause has been encountered.\n",
	        "Runtime 0 ms\n No (more) solutions\n",
	        "--- In fact the prover has not even been started.\n"),
	    fail
	; \+ 'GoalClause'(_) ->
	    msg("??? No goal clause is left.\n",
	        "Runtime 0 ms\n No (more) solutions\n",
	        "--- In fact the prover has not even been started.\n"),
	    fail
	; once(load_and_call(Prover,Module,prover,Path,Extension,
		["Dynamic prover module ",Prover," has been loaded."])) ->
	    true
	;   err("*** The prover has failed."),
	    fail
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate read_phase/1(+File).

The first phase consists of reading the matrix and setting some automatic
options. The reading is done with respect to the filters defined in the option
|input_filter|.

\PL*/
read_phase(File) :-
	is_option(input_filter,Filter),
	read_matrix_with_filter(Filter,File),
	flush(output),
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Some options are set automatically after the matix has been read. This task is
performed by the following instructions. The matrix is inspected and the
options set accordingly.

Currently two options are supported. When the predicate |=/2| occurs in the
matrix then the option |equality| is turned on. If the predicate |:/2| occurs
in the matix then the option |setvar| is turned on. In fact the |:| indicates
a predicate variable.

\PL*/	
	( ( 'Contrapositive'(-- (_=_),_,_)
	  ; 'Contrapositive'(++ (_=_),_,_) ) ->
	    set_option(equality=on)
	;   set_option(equality=off)
	),

	( ( 'Contrapositive'(-- (_:_),_,_)
	  ; 'Contrapositive'(++ (_:_),_,_) ) ->
	    set_option(setvar=on)
	;   set_option(setvar=off)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate prepare_phase/0().

The prepare phase consists of mainly two actions. The reduction modules given
in the option |prove:red_goals| are invoked and the items given in the option
|prove:log_items| are written to the log file.

\PL*/
prepare_phase :-
	is_option('prove:red_goals',Steps),
	is_option('prove:red_path',Path),
	( functor(Steps,'.',2) ->
	    run_goals(Steps,Path)
	; Steps == [] ->
	    true
	;   err("*** Type error for option 'prove:red_goals'. List expected."),
	    fail
	),
	is_option('prove:log_items',LogItems),
	prove_log(LogItems).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate run_goals/1(+GoalList, +Path).

\PL*/
run_goals(GoalList,Path) :-
	(   member(Goal,GoalList),
	    ( Goal = (Module:Call) ->
		true
	    ;   functor(Goal,Module,_),
		Call = Goal
	    ),
	    load_module(Call,Module,red,Path,["",".pl"],[]),
	    concat_string(["Red ",Module,":"],Message),
	    reset_time,
	    ( once(call(Call,Module)) ->
		log_reset_time_and_print(Message,"\n")
	    ;   err("*** Goal failed in module ",Module,": ",Call)
	    ),
	    fail
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate prove_log/1(+LogItems).

This predicate writes the items given in |LogItems| to the logfile. See
|open_log_stream/1| and |prove_log/2| for details.

\PL*/
prove_log(LogItems) :-
	( open_log_stream(Stream) ->
	    prove_log(LogItems,Stream),
	    close(Stream)
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate prove_log/2(+Item, +Stream).

This predicate writes the term |Item| to the output stream |Stream|. |Item|
can be one of several predefined symbolic names or a list of such. According
to the symbolic name the real information is extracted and written to the
stream.

A list is decomposed and the items contained are written to the output
stream. Even nested lists are handled properly.

\PL*/
prove_log([],_) :- !.
prove_log([H|T],Stream) :-
	!,
	prove_log(H,Stream),
	prove_log(T,Stream).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
\item[user]\ \\
  The name of the user is written to the log file. The user information is
  stored in the log term |user/2|. The first argument is the login name of the
  user and the second name is the full name of the user as given in the GCOS
  field of the passwd file.
\end{description}

\PL*/
prove_log(user,Stream) :-
	!,
	full_user_name(User,Name),
	printf(Stream,"user(%Qw,%Qw).\n",[User,Name]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
\item[host]\ \\
  The host information is written to the log file. The host information is
  stored in the log term |host_info/2|. The first argument contains the
  hostname. The second argument contains the host architecture.
\end{description}

\PL*/
prove_log(host,Stream) :-
	!,
	get_flag(hostarch,Arch),
	get_flag(hostname,Host),
	printf(Stream,"host_info(%Qw,%Qw).\n",[Host,Arch]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
\item[date]\ \\
  The current date is written to the log file. The date is stored in the log
  term |date/1|. The argument is a string containing the date in the format as
  given by the command |date/1|.
\end{description}

\PL*/
prove_log(date,Stream) :-
	!,
	date(Date),
	printf(Stream,"date(%Qw).\n",[Date]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
\item[prover]\ \\
  The prover to be used is written to the log file. This information could
  also be extracted from the options, but sometimes this is the only option we
  are interested in.

  The prover name is stored in the log term |prover/1|.
\end{description}

\PL*/
prove_log(prover,Stream) :-
	!,
	is_option(prover,Prover),
	printf(Stream,"prover(%w).\n",[Prover]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
\item[matrix]\ \\

  The list of clauses is written to the log file. The clauses are enclosed in
  |begin(matrix)| and |end(matrix)|. This environment contains log terms of
  the following types:
  \begin{description}
  \item[GoalClause/1] contains the indices of the goal clauses.
  \item[Label/2] contains the labels of the clauses. The first argument is a
    clause index and the second argument is its label.
  \item[Clause/2] contains the clauses. The first argument is the index of
    this clause. The second argument is the list of literals. Each literal is
    of the form |literal(|$L$|,|$I$|)| where $L$\/ is a signed predicate and
    $I$\/ is its index.
  \end{description}

\end{description}

\PL*/
prove_log(matrix,Stream) :-
	!,
	printf(Stream,"begin(matrix).\n",[]),
	(   'GoalClause'(Index),
	    printf(Stream,"  goal(%QDMw).\n",[Index]),
	    fail
	;   'Label'(Index,Label),
	    printf(Stream,"  label(%QDMw,%QDMw).\n",[Index,Label]),
	    fail
	;   'Clause'(Index,Body),
	    printf(Stream,"  clause(%QDMw,%QDMw).\n",[Index,Body]),
	    fail
	;   true
	),
	printf(Stream,"end(matrix).\n\n",[]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
\item[contrapositives]\ \\
  The list of contrapositives is written to the log file. The contrapositives
  are enclosed in |begin(contrapositives)| and |end(contrapositives)|. This
  environment contains log terms of the type |contrapositive/3|. The first
  argument is the head of the contrapositive. The second argument is the body,
  i.e. the list of literals without the head. The third argument contains the
  index of the contrapositive.
\end{description}

\PL*/
prove_log(contrapositives,Stream) :-
	!,
	printf(Stream,"begin(contrapositives).\n",[]),
	(   'Contrapositive'(Head,Body,Index),
	    printf(Stream,
		   "  contrapositive(%QDMw,%QDMw,%QDMw).\n",
		   [Head,Body,Index]),
	    fail
	;   true
	),
	printf(Stream,"end(contrapositives).\n\n",[]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}

\end{description}

\PL*/
prove_log(options,Stream) :-
	!,
	printf(Stream,"begin(options).\n",[]),
	setof((A=B),is_option(A,B),Options),
	(   member(Opt,Options),
	    printf("  %q.\n",[Opt]),
	    fail
	;   true
	),
	printf(Stream,"end(options).\n\n",[]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Well, if everything else fails we kindly inform the user that something is
teribly wrong.

\PL*/
prove_log(Item,_Stream) :-
	err("*** Log_item unknown: ",Item).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog*/
