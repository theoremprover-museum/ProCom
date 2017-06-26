%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: protop.pl,v 1.28 1995/07/03 11:35:12 gerd Exp gerd $
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

:- module_interface(protop). /*%-----------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.28 $}
\input{../Doc/doc.cfg}

This module provides an interactive interface to perform various sorts of
actions related to using a theorem prover. In addition to the interactive use,
the commands can also be stored in a file and executed in batch mode. A
powerful macro mechanism and some control constructs allow a broad variety of
actions to be specified and executed.

\PL*/
:- export protop/0,
	  protop/1.
:- begin_module(protop).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

First we load the configuration file and act according to the settings given
there.

\PL*/
:-	compile('protop.cfg').
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The file search path for Prolog libraries is enhanced. For this purpose all
solutions of the predicate |prover_system_path/1| are collected and appended
to the approriate \eclipse{} flag.

\PL*/
:-	get_flag(library_path,OldPath),
	findall(Dir, prover_system_path(Dir), Path),
	union(Path,OldPath,FullPath),
	set_flag(library_path,FullPath).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To end the loading phase we load the modules from the Prolog libraries and the
other parts of the system.

\PL*/
:-	lib(util).

:-	lib(log),
	lib(time),
	lib(options),
	lib(message),
	lib(matrix),
	lib(prove),
	lib(find_file),
	lib(dynamic_module),
	lib(proof_report).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- define_option 'ProTop:welcome'	   = on.
:- define_option 'ProTop:debug'		   = off.
:- define_option 'ProTop:verbose'	   = on.
:- define_option 'ProTop:resource_file'	   = ".protop".
:- define_option 'ProTop:prover_file'	   = "...prover.pl".
:- define_option 'ProTop:path'		   = [".","Scripts","~"].
:- define_option 'ProTop:script_extension' = ["",".pt"].
:- define_option 'ProTop:prompt'	   = "ProTop ->".
:- define_option 'ProTop:backup'	   = on.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

First of all I reset the prompt to add a space to |ProTop:prompt|.
\PL*/
:-	get_prompt(user, _ ,Stream),
	set_prompt(user," ",Stream).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Syntactic sugar. Some of the instructions can be typed in without the
parenthesis when some operators are declared properly:

\PL*/
:-	op(1000, fy,(for)),
	op( 999,xfy,(in)),
	op(1010,xfy,(do)),
	op(1005,xfy,(:=)),
	op(1010, fy,(repeat)),
	op(1010,xfy,(times)),
	op( 900, fy,(author)),		% for report
	op( 900, fy,(call)),
	op( 900, fy,(cd)),
	op( 900, fy,(comment)),		% for report
	op( 900, fy,(defined)),
	op( 900, fy,(delete)),
	op( 900, fy,(delete_macro)),
	op( 900, fy,(exists)),
	op( 900, fy,(file)),
	op( 900, fy,(init)),		% for report
	op( 900, fy,(include)),
	op( 900, fy,(label)),		% for report
	op( 900, fy,(library_path)),
	op( 900, fy,(list)),
	op( 900, fy,(macro)),
	op( 900, fy,(ls)),
	op( 900, fy,(module)),		% for status
	op( 900, fy,(prove)),
	op( 900, fy,(pwd)),
	op( 900, fy,(read)),
	op( 900, fy,(read_string)),
	op( 900, fy,(report)),
	op( 900, fy,(section)),		% for report
	op( 900, fy,(show)),
	op( 900, fy,(status)),
	op( 900, fy,(text)),		% for report
	op( 900, fy,(title)),		% for report
	op( 900, fy,(write)),
	op( 900, fy,(writeln)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We want to allow really long strings. I.e. we encourage users to write long
comments into strings to be included into the reports.

\PL*/
:-	set_flag(syntax_option,nl_in_quotes).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:-	make_local_array(load_rc),
	setval(load_rc,on).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate ProTop macro/3(Head, Tail, Source).

\ProTop{} macros are stored in the Prolog data base. For this purpose the
predicate |ProTop macro/3| is used.

\PL*/
:- dynamic 'ProTop macro'/3.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate protop/0().

This predicate is used to start the \ProTop{} top level loop. All
initializations are performed here. Command line switches are evaluated and
the interrupt/error handlers are defined appropriately.

\PL*/
protop :-
	retract_all('ProTop macro'(_,_,user)),
	pt(reset_options),
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Now the command line arguments are collected in the list |ARGV|. The command
line arguments are evaluated by the predicate |evaluate_argv/1|.

\PL*/
	argc(ARGC),
	N is ARGC - 1,
	findall(A,(between(1,N,1,I),argv(I,A)),ARGV),
	evaluate_argv(ARGV,LOAD_RC),
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
	is_option('ProTop:resource_file',RC),
	( LOAD_RC == off ->
	    true
	; empty_option(RC) ->
	    true
	; find_file(RC,[".","~"],[""],RC_File) ->
	    pt(include(RC_File))
	; is_option('ProTop:verbose') ->
	    printf("--- You seem to have no resource file: %w\n",[RC])
	;   true
	),
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
	( is_option('ProTop:welcome') ->
	    protop_version(Version),
	    printf("\n\t\t--- Welcome to ProTop (%w) ---\n\n",[Version])
	;   true
	),
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\ProTop\ tries to catch any error occuring in the \eclipse\ system and recover
by restarting the top level loop. For this purpose a block is used. If an
error is encountered then this block fails and in a failure driven loop the
toplevel is started again.

\PL*/
	is_option('ProTop:prompt',Prompt),
	protop_set_error_handlers,
	repeat,
	block(protop(Prompt,"<user>",input),
	      protop,
	      fail),
	!,
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Finally we have to undo all changes we have mnade to the error and interrupt
handling routines. 
\PL*/
	reset_error_handlers,
	reset_interrupt_handlers.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If the initialization or evaluation of command line argument fails then
|protop/0| should simply succeed. For this purpose the following clause is
included. 

\PL*/
protop.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate usage/0().

This predicate prints a summary of the command line options. This should be
the usual behaviour of any UNIX program.

\PL*/
usage :-
	writeln("Usage: protop [options]"),
	writeln("\t-d\tturn on debugging."),
	writeln("\t-f file\tinclude the script file `file'."),
	writeln("\t-h\tshow usage and exit."),
	writeln("\t-q\tdo not load the .protop file."),
	writeln("\t-R\tload the file .protop now."),
	writeln("\t-v\tprint the version of ProTop and exit."),
	writeln("\t-x\timmediately exit the ProTop process."),
	exit(0).	
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate evaluate_argv/2(+ListOfArgs,-LoadRC).

The argument |ListOfArgs| is the list of command line arguments of \ProTop.
They are split and evaluated at once.


The empty list of command line arguments is simply ignored. This is the base
case of the recursion looping over all arguments.
\PL*/
evaluate_argv([],_).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
\item[--a]\ 
  \\
  The backward compatibility file is loaded when this flag is given. The
  loading occurs at once. Thus it is possible to overwrite things later.
\end{description}

\PL*/
evaluate_argv(["-a"|Tail],LoadRC) :-
	!,
	( pt(include(ancient)) -> true; true ),
	evaluate_argv(Tail,LoadRC).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
\item[--d]\ 
  \\
  The debugging of script files is turned on. This is identical to turning on
  the option |ProTop:debug|.
\end{description}

\PL*/
evaluate_argv(["-d"|Tail],LoadRC) :-
	!,
	set_option('ProTop:debug'=on),
	evaluate_argv(Tail,LoadRC).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
\item[--f {\em file}]\ 
  \\
  The script file {\em file}\/ is evaluated. This has the same effect as the
  \ProTop{} instruction {\bf include({\em file}\/)}. The success or failure is
  ignored.  I.e. the initializing sequence continues even if the file could not
  be found.
\end{description}

\PL*/
evaluate_argv(["-f",File|Tail],LoadRC) :-
	!,
	( pt(include(File)) -> true; true ),
	evaluate_argv(Tail,LoadRC).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
\item[--h] 
\item[--he] 
\item[--hel] 
\item[--help] \ 
  \\
  Display a shoprt description of the command line arguments of \ProTop{}.
  Afterwards the \ProTop{} process is terminated. The exit status 0 is
  returned to the operating system.
\end{description}

\PL*/
evaluate_argv(["-h"|_],_) :-
	!,
	usage.
evaluate_argv(["-he"|_],_) :-
	!,
	usage.
evaluate_argv(["-hel"|_],_) :-
	!,
	usage.
evaluate_argv(["-help"|_],_) :-
	!,
	usage.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
\item[--q]\ 
  \\
  Ususally the user's file {\sf .protop} is evaluated after the command line
  arguments. This switch turns off this behaviour.
\end{description}

\PL*/
evaluate_argv(["-q"|Tail],off) :-
	!,
	evaluate_argv(Tail,_LoadRC).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
\item[--R]\ 
  \\
  The user's {\sf .protop} file is loaded at once. The additional evaluation
  after the end of the command line arguments is disabled. No message is
  printed in case it is not found. Thus this switch can be used to suppress
  the message indicating a missing {\sf .protop} file.
\end{description}

\PL*/
evaluate_argv(["-r"|Tail],off) :-
	!,
	is_option('ProTop:resource_file',RC),
	( find_file(RC,[".","~"],[""],File) ->
	    pt(include(File))
	;   true
	),
	evaluate_argv(Tail,_LoadRC).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
\item[--v]\ 
  \\
  The version of \ProTop is printed and then the \ProTop{} process is
  terminated. The exit status 0 is returned to the operating system.
\end{description}

\PL*/
evaluate_argv(["-v"|_],_) :-
	!,
	pt(show(version)),
	exit(0).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
\item[--x]\ 
  \\
  The \ProTop{} process is terminated. The exit status 0 is returned to the
  operating system.
\end{description}

\PL*/
evaluate_argv(["-x"|_],_) :-
	!,
	exit(0).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Command line arguments which are not understood are silently ignored.

\PL*/
evaluate_argv([_|Tail],LoadRC) :-
	evaluate_argv(Tail,LoadRC).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate protop_set_error_handlers/0().

This predicate is used to adjust the behaviour in case of errors. Instead of
calling one of several error handling predicates provided with \eclipse{} the
error handler of \ProTop{} is used instead. Tyhe same applies for the
interrupt handlers.

Any interrupt or error leads to a restart of the top level loop.

Some interrupts are triggered in situation where other actions than reporting
an error and restarting the top level loop is required. Those error handlers
are left unchanged.

\PL*/
protop_set_error_handlers :-
	set_interrupt_handler(int,protop_interrupt_handler/0),
	set_interrupt_handler(ill,protop_interrupt_handler/1),
	set_interrupt_handler(trap,protop_interrupt_handler/1),
	set_interrupt_handler(iot,protop_interrupt_handler/1),
	set_interrupt_handler(emt,protop_interrupt_handler/1),
	set_interrupt_handler(fpe,protop_interrupt_handler/1),
	set_interrupt_handler(bus,protop_interrupt_handler/1),
	set_interrupt_handler(segv,protop_interrupt_handler/1),
	set_interrupt_handler(sys,protop_interrupt_handler/1),
	set_interrupt_handler(pipe,protop_interrupt_handler/1),
	set_interrupt_handler(io,protop_interrupt_handler/1),
	set_interrupt_handler(usr1,protop_interrupt_handler/1),
	set_interrupt_handler(usr2,protop_interrupt_handler/1),
	(   between(1,256,1,N),
	    N \== 190,
	    N \== 146,
	    error_id(N,_),
	    get_error_handler(N,Pred,_),
	    member(Pred,[error_handler/2,
			 warning_handler/2,
			 parser_error_handler/2,
			 compiler_error_handler/2,
			 compiler_abort_handler/2,
			 kegi_error_handler/2]),
	    set_error_handler(N,protop_error_handler/3),
	    fail
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate protop_interrupt_handler/0().

This predicate is executed when an interrupt is encountered. E.g. such an
interrupt is raised when the user presses |<Control-C>|. A message is printed
on the screen and the block |protop| is left. This predicate is used by the
\ProTop{} top level routine.

\PL*/
protop_interrupt_handler :-
	set_interrupt_handler(alrm,true/0),
	writeln("\n*** Interrupted."),
	exit_block(protop).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate protop_interrupt_handler/1(+Signal).

This predicate is executed when an interrupt is encountered. E.g. such an
interrupt is raised when the user presses |<Control-C>|. A message is printed
on the screen and the block |protop| is left. This predicate is used by the
\ProTop{} top level routine. In addition to the predicate
|protop_interrupt_handler/0| the signal number is presented.

\PL*/
protop_interrupt_handler(Signal) :-
	set_interrupt_handler(alrm,true/0),
	printf("\n*** Interrupted: %w.",[Signal]),
	exit_block(protop).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate protop_error_handler/3(+Number,Culprit,Module).

The error handler is called whenever \eclipse{} encounters an error which is
caught by \ProTop. It prints an error message and tries to restart the
\ProTop{} top level loop. This is done by exiting the block labeled |protop|.

\PL*/
protop_error_handler(Number,Culprit,Module) :-
	set_interrupt_handler(alrm,true/0),
	error_id(Number,Message),
	( var(Module) ->
	    printf("\n*** Error: %w: %w\n",[Message,Culprit])
	;   printf("\n*** Error in module %w: %w: %w\n",
		   [Module,Message,Culprit])
	),
	exit_block(protop).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate protop/1(+Command).

This predicate can be used to start one or more \ProTop{} commands from within
Prolog. 

If the argument is a list then the elements are interpreted as \ProTop{}
commands and they are given to the \ProTop{} interpreter. As a special case
the empty list just succeeds.

\PL*/
protop([]) :-
	!.
protop([H|T]) :-
	!,
	pt([H|T]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If the argument is the empty string or the empty symbol then an interactive
toplevel loop ist started. In contrast to the toplevel loop started by
|protop/0| no initializations are perfomed and no provisions are made to catch
errors or interrrupts.

\PL*/
protop('') :-
	!,
	is_option('ProTop:prompt',Prompt),
	protop(Prompt,"<user>",input).
protop("") :-
	!,
	is_option('ProTop:prompt',Prompt),
	protop(Prompt,"<user>",input).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Finally any other atom or string argument is interpreted as a file name and
the file name is given to the \ProTop{} top level loop to read commands from.

\PL*/
protop(File) :-
	(   atomic(File)
	;   string(File)
	),
	retract_all('ProTop macro'(_,_,user)),
	pt(include(File)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate protop/3(+Prompt, +File, +Stream).

This predicate is the driver for the \ProTop{} top level loop. It can be used
to control the interactive read-eval-print loop as well as the processing of
the contents of a file. 

|Prompt| is a string which should be printed before a new term is read. It is
also used to indicate the mode of operation. If |Prompt| is no string then
this predicate assumes that no interactive operation is active. In this case
no prompt and no responses are displayed.

|File| is the name of the current file as a string. It is used for debugging
purposes. |Stream| is a input stream where the terms to be interpreted are
read from. 

\PL*/
protop(Prompt,File,Stream) :-
	( string(Prompt) -> 
	    PROMPT = (write(Prompt),flush(stdout)),
	    OK	   = write("ok.\n"),
	    KO	   = write("no.\n")
	;   PROMPT = true,
	    OK	   = true,
	    KO	   = true
	),
	repeat,
	call(PROMPT),
	read(Stream,Term),
	( protop_end(Term) ->
	    true
	;   protop_trace(File,Term),
	    ( once(pt(Term)) ->
		call(OK)
	    ;	call(KO)
	    ),
	    fail
	),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate protop_end/1(?Symbol).

This predicate unifies |Symbol| with the end tokens of the toplevel loop.

\PL*/
protop_end(end_of_file).
protop_end(end).
protop_end(quit).
protop_end(stop).
protop_end(exit).
protop_end(halt).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate protop_trace/2(+Prefix, +Instruction).

\PL*/
protop_trace(Prefix,Instruction) :-
	( is_option('ProTop:debug') ->
	    printf("%w>> %q\n",[Prefix,Instruction])
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate pt/1(+Instruction).

This predicate provides the main switch for all constructs provided by
\ProTop. 

First some noops.

\PL*/
pt(end_of_file) :- !.
pt(end)		:- !.
pt(stop)	:- !.
pt(quit)	:- !.
pt(exit)	:- !.
pt(true)	:- !.
pt([])		:- !.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
pt(fail) :- 
	!,
	fail.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
pt(halt) :-
	!,
	exit(0).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
  \item [include({\em File})]\index{ProTop!include}\ 
    \\
    This instruction reads commands from the file {\em File} and executes
    them.  The option |ProTop:path| can be used to specify the search path.
    The option |ProTop:script\_ext| can be used to specify the extensions to
    be appended when searching for the actual file.
  %
\begin{BoxedSample}
  ProTop -> |include("some_script").|
  |---| ProTop script file some\_script not found.
  no.
  ProTop -> |include(".protop").|
  ok.
  ProTop -> 
\end{BoxedSample}
\end{description}

\PL*/
pt(include(File)) :-
	!,
	is_option('ProTop:path',Path),
	is_option('ProTop:script_extension',Ext),
	( \+ file_type(File) ->
	    err("*** Type error in ",include(File),"\n"),
	    fail
	; find_file(File,Path,Ext,FileName) ->
	    open(FileName,read,Stream),
	    protop([],File,Stream),
	    close(Stream),
	    !
	; 
	    err("--- ProTop script file ",File," not found."),
	    fail
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
  \item ["{\em File}"]\index{ProTop!include}\ 
    \\
    A file name marked as a string --- i.e. enclosed in double quotes --- Is
    taken as a file name to be included. Thus it is equivalent to
    {\bf include({\em File}\/)}.

  \item [end\_of\_file]\index{ProTop!end\_of\_file}\ \\
  This token is returned by the Prolog reading apparatus upon end of file.
  Thus it can be used in this sense. Upon end of file the session is usually
  ended. If encountered in a script file the rest of the file is ignored.
\end{description}

\PL*/
pt(File) :-
	string(File),
	!,
	pt(include(File)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
  \item [help]\index{ProTop!help}\ 
    \\
    This instruction prints a short information which should help you to carry
    on. The help command can also be given an argument. In this case some
    information on the argument is given. This is the same as executing the
    |status| command with an argument. For details see the description of
    |status|.
  %
\begin{BoxedSample}
  ProTop -> |help.|
  The command `show' can be used to get information on the
  system as a whole or on parts of it.
    show.
  presents the status of some important parts of ProTop.

    show commands.
  gives a list of all commands available.

  To leave ProTop type
    halt.
  ok.
  ProTop ->
\end{BoxedSample}
\end{description}

\PL*/
pt(help) :-
	!,
	writeln("The command `show' can be used to get information on the"),
	writeln("system as a whole or on parts of it."),
	writeln("  show."),
	writeln("presents the status of some important parts of ProTop."),
	writeln(""),
	writeln("  show commands."),
	writeln("gives a list of all commands available."),
	writeln(""),
	writeln("To leave ProTop type"),
	writeln("  halt.").
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}

  \item [status]\index{ProTop!status}\ \\
  This instruction prints the status of various parts of \ProTop{} to the
  scrren. The status of parts can be displayed seperately by the next
  instruction.
  %
\begin{BoxedSample}
ProTop -> |status.|
        This is ProTop version \Version
        Filters:
                E\_flatten
                constraints
                none
        Provers:
                procom
                otter
        Matrix:
                No matrix loaded.
        Macros:
        0 macros are defined.
ok.
\end{BoxedSample}
\end{description}

\PL*/
pt(status) :-
	!,
	protop_status(all).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}

\item [status({\em Part}\/)]\index{ProTop!status}\ \\ This instruction
  displays the status of the requested part {\em Part}\/ to the screen. {\em
    Part}\/ can also be a list of parts in which case the status of all parts
  given in the list is displayed. The following parts can be specified:
  \begin{description}
  \item [version]\ \\ prints the version of \ProTop.
  \item [macros]\ \\  prints a summary of macros defined in \ProTop.
  \item [prover]\ \\  prints a list of provers loaded.
  \item [filter]\ \\  prints a list of filters loaded.
  \item [matrix]\ \\  prints a summary of the matrix currently loaded.
  \item [clauses]\ \\ prints a list of clauses currently stored in the
    matrix.
  \item [contrapositives]\ \\ prints a list of contrapositives currently
    stored in the matrix.
  \item [modules]\ \\ prints a list of the loaded modules and the associated
    documentation strings.
  \item [module({\em Module}\/)]\ \\ prints documentation string for module
    {\em Module}. It fails if {\em Module}\/ is not instanciated properly or
    it contains no \ProTop{} module.
  \item [options]\ \\ prints a list of all options.
  \item [option({\em Option}\/)]\ \\ prints the value of the option {\em
      Option}. 
  \item [file({\em File}\/)]\ \\ displays the contents of the file {\em
      File} on the screen.
  \item [macros]\ \\ prints the list of all macros to the screen.
  \item [macro({\em Macro}\/)]\ \\ displays the definition of all macros for
    which the head unify with {\em Macro}.
  \item [all]\ \\     does {\bf version}, {\bf filter}, {\bf prover}, {\bf
      matrix} and {\bf macros}.
  \end{description}
  
  This instruction fails if one of the requested parts is not known or it is
  called with a variable argument.
\end{description}

\PL*/
pt(status(Part)) :-
	!,
	protop_status(Part).
pt(list(Part)) :-
	!,
	protop_status(Part).
pt(show(Part)) :-
	!,
	protop_status(Part).
pt(help(Part)) :-
	!,
	protop_status(Part).
pt(info(Part)) :-
	!,
	protop_status(Part).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}

  \item [{[{\em Instruction}, \ldots, {\em Instruction}]}]\index{ProTop![...]}\ \\
  A list of instructions is executed like a conjunction (see below).
  %
\begin{BoxedSample}
ProTop -> |[write(1),write(2),nl].|
12
ok.
\end{BoxedSample}
\end{description}

\PL*/
pt([Instruction1 | Instruction2]):-
	!,
	protop_trace("_",Instruction1),
	pt(Instruction1),
	pt(Instruction2).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}

  \item [({\em Instruction}, \ldots, {\em Instruction})]\index{ProTop!(,)}\ \\
  Conjunctions are executed from left to right until one instruction fails.
  In this case the whole conjunction fails. If no instruction fails then
  the conjunction succeeds.
  %
\begin{BoxedSample}
ProTop -> |write(1),write(2),nl.|
12
ok.
\end{BoxedSample}
\end{description}

\PL*/
pt((Instruction1, Instruction2)) :-
	!,
	protop_trace(".",Instruction1),
	pt(Instruction1),
	pt(Instruction2).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
  \item [({\em Instruction}; \ldots; {\em Instruction})]\index{ProTop!(;)}\ \\
  Disjunctions are executed from left to right until one instruction succeeds.
  In this case the whole disjunction succeeds. If no instruction succeeds then
  the disjunction fails.
  %
\begin{BoxedSample}
ProTop -> |(writeln(first),fail); writeln(second).|
first
second
ok.
\end{BoxedSample}
\end{description}

\PL*/
pt((Instruction1; Instruction2)) :-
	!,
	protop_trace(".",Instruction1),
	( once(pt(Instruction1)) ->
	    true
	;   once(pt(Instruction2))
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
  \item [repeat {\em N}\/ times {\em Command}]\index{ProTop!repeat}\ \\
  This instruction repeats the execution of {\em Command}\/ several times.
  {\em N}\/ is an integer denoting the number of repetitions.
  %
\begin{BoxedSample}
ProTop -> |repeat 5 times write("----+").|
----+----+----+----+----+ok.
ProTop ->       
\end{BoxedSample}
\end{description}

\PL*/
pt((repeat N times Command)) :-
	!,
	(   between(1,N,1,_),
	    protop_trace("",Command),
	    once(pt(Command)),
	    fail
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


\begin{description}
  \item [for {\em Var}\/ in {\em Spec}\/ do {\em Command}]\index{ProTop!for}\ \\
  This instruction repeats {\em Command}\/ for all elements of {\em Spec}\/
  assigned to {\em Var}. This is done in a failure driven manor, i.e.
  variable bindings are undone after each loop. {\em Spec}\/ is expanded to a
  list before the loop is performed. The following constructs for {\em Spec}\/
  are supported:
  \begin{description}
    \item [{[{\em ...}]}]\ \\
    A list of any elements is the simplest form of a specification. No
    expansion takes place in this case.
    %
\begin{BoxedSample}
ProTop -> |for Var in [1,2,3] do writeln(Var).|
1
2
3
ok.
\end{BoxedSample}
  \end{description}
\end{description}

\PL*/
pt((for Var in Spec do Command)) :-
	!,
	( var(Spec) ->
	    fail
	; functor(Spec,'.',2) ->
	    Values = Spec
	; Spec = files(Dir) ->
	    read_directory(Dir,"*",_,V),
	    add_dir(Dir,V,Values)
	; Spec = files(Dir,Pattern) ->
	    read_directory(Dir,Pattern,_,V),
	    add_dir(Dir,V,Values)
	; Spec = directories(Dir) ->
	    read_directory(Dir,"*",V,_),
	    add_dir(Dir,V,Values)
	;
	    fail
	),
	(   member(Var,Values),
	    protop_trace("",Command),
	    once(pt(Command)),
	    fail
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\subsection{Conditionals}

\begin{description}

  \item [if {\em Condition}\/ then {\em Then}\/ else {\em Else}]\index{ProTop!if}\ \\
  This instruction evaluates the commands {\em Condition}. If this evaluation
  succeeds then the commands {\em Then}\/ are evaluated afterwards. Otherwise
  the commands {\em Else}\/ are executed.
    %
\begin{BoxedSample}
ProTop -> |if exists("/etc/motd")|
\          |then show("/etc/motd")|
\          |else writeln("Sorry").|
SunOS Release 4.1.3 (GENERIC) \#3: Mon Jul 27 16:44:16 PDT 1992
ok. 
\end{BoxedSample}
\end{description}

\PL*/
pt((if Condition then Then else Else)) :-
	!,
	( pt(Condition) ->
	    pt(Then)
	;   pt(Else)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The case of an missing else part is covered next.

\PL*/
pt((if Condition then Then)) :-
	!,
	( pt(Condition) ->
	    pt(Then)
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}

  \item [not({\em Condition}\/)]\index{ProTop!not}\ \\
  This instruction executes {\em Condition}\/ and reverses the return status.
  I.e. it succeeds if {\em Condition}\/ fails and vice versa.
  %
\begin{BoxedSample}
ProTop -> |not(true).|
no.
ProTop -> |not(fail).|
ok.
\end{BoxedSample}
\end{description}

\PL*/
pt(not(Condition)) :-
	!,
	\+ once(pt(Condition)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\subsection{Output Routines}

\begin{description}
  \item [nl]\index{ProTop!nl}\ \\
  This instruction writes a newline to the screen.
\end{description}

\PL*/
pt(nl) :-
	!,
	nl.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
  \item [write({\em Term})]\index{ProTop!write}\ \\
  This instruction writes {\em Term}\/ to the screen. {\em Term} is {\sc not}
  followed by a newline.
  %
\begin{BoxedSample}
ProTop -> |write("hello world.").|
hello world.ok.
ProTop -> 
\end{BoxedSample}
\end{description}

\PL*/
pt(write(Term)) :-
	!,
	write(Term).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
  \item [writeln({\em Term})]\index{ProTop!writeln}\ \\
  This instruction writes {\em Term}\/ to the screen. {\em Term} is followed
  by a newline.
  %
\begin{BoxedSample}
ProTop -> |writeln("hello world.").|
hello world.
ok. 
\end{BoxedSample}
\end{description}

\PL*/
pt(writeln(Term)) :-
	!,
	writeln(Term).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
  \item [printf({\em Format}, {\em Arguments})]\index{ProTop!printf}\ \\
  This instructions prints the {\em Arguments} according to the format string
  {\em Format}. {\em Format} is a string where \% is used as escape to specify
  the form of an argument. {\em Arguments} is a list of terms to be printed
  according to the format string. For details see the documentation of the
  \eclipse{} predicate |printf/2|.
  %
\begin{BoxedSample}
ProTop -> |printf(".....%w.....%w.....\n",[1,2]).|
.....1.....2.....
ok.
\end{BoxedSample}
\end{description}

\PL*/
pt(printf(Format,Arguments)) :-
	!,
	printf(Format,Arguments).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\subsection{OS Interface Routines}

\begin{description}

  \item [exists({\em File})]\index{ProTop!exists}\ \\
  This instruction checks the existence of the file {\em File}. {\em File}
  must be a symbol or string. Otherwise a type error is raised and the
  instruction fails.
  %
\begin{BoxedSample}
ProTop -> |exists("~/.login").|
ok.
ProTop -> |exists("~/.protop").|
no.
\end{BoxedSample}
\end{description}

\PL*/
pt(exists(File)) :-
	!,
	( \+ file_type(File) ->
	    err("*** Type error in ",exists(File)),
	    fail
	;   exists(File)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
  \item [find\_file({\em File},{\em Path},{\em Extensions},{\em FullFile})]\index{ProTop!find\_file}\ \\
  This instruction tries to find the existing file {\em File} in a list of
  directories {\em Path}. A list of extensions {\em Extensions}\/ is used to
  augment the file name with an additional extension. The result is unified
  with {\em FullFile}.
  %
\begin{BoxedSample}
ProTop -> |find_file(".PROTOP",[".","~"],[""],File),|
\          |writeln(File).|
no.
ProTop -> |find_file(".protop",[".","~"],[""],File),|
\          |writeln(File).|
./.protop
ok.
\end{BoxedSample}
  
\end{description}

\PL*/
pt(find_file(File,Path,Extension,FullFile)) :-
	!,
	( \+ file_type(File) ->
	    err("*** Type error in ",find_file(File)),
	    fail
	;   find_file(File,Path,Extension,FullFile)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}

  \item [pwd]\index{ProTop!pwd}\ \\
  This instruction prints the current working directory to the standard output
  stream.
  %
\begin{BoxedSample}
ProTop -> |pwd.|
/system/ProCom/
ok.
\end{BoxedSample}
\end{description}

\PL*/
pt(pwd) :-
	!,
	getcwd(Dir),
	msg(Dir).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}

  \item [pwd(Dir)]\index{ProTop!pwd}\ \\
  This instruction unifies it's argument with the current directory. Thus it
  is possible to get your hands on the current directory in script files.
  %
\begin{BoxedSample}
ProTop -> |pwd(X).|
ok.
ProTop -> |pwd(X),writeln(X).|
/system/ProCom/
ok.
\end{BoxedSample}
\end{description}

\PL*/
pt(pwd(Dir)) :-
	!,
	(   atom(Dir)
	;   string(Dir)
	),
	getcwd(Dir).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}

  \item [cd(Dir)]\index{ProTop!cd}\ \\
  This instruction tries to set the current directory to the one given as
  argument. |Dir| has to be a string or atom which corresponds to a valid
  directory name.
  %
\begin{BoxedSample}
ProTop -> |cd "~", pwd.|
/home/gerd/
ok.
\end{BoxedSample}
\end{description}

\PL*/
pt(cd(Dir)) :-
	!,
	( (\+ atom(Dir), \+ string(Dir)) ->
	    err("*** Type error in ",cd(Dir)),
	    fail
	; \+ exists(Dir) -> 
	    err("*** Directory does not exist in ",cd(Dir)),
	    fail
	;   cd(Dir)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}

  \item [ls]\index{ProTop!ls}\ \\
  This instruction lists all files in the current directory on the standard
  output stream.
\end{description}

\PL*/
pt(ls) :-
	!,
	system("ls").
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
  \item [ls(Dir)]\index{ProTop!ls}\ \\
  This instruction lists all files in the directory |Dir| on the standard
  output stream.
\end{description}

\PL*/
pt(ls(Dir)) :-
	!,
	( (atom(Dir); string(Dir)) ->
	    concat_string(["ls ",Dir],Command),
	    system(Command)
	;   err("*** Type error in ",ls(Dir)),
	    fail
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\subsection{Input Routines}

\begin{description}
  \item [read\_string({\em Prompt}, {\em String})]\index{ProTop!read\_string}\
  \\
  This instruction writes {\em Prompt} to the screen and reads all characters
  up to the next newline into the string {\em String}. The end of file also
  terminates the reading.
  %
\begin{BoxedSample}
ProTop -> |read_string("Enter a string:",String).|
Enter a string: |some string terminated by newline|
ok.
\end{BoxedSample}
\end{description}

\PL*/
pt(read_string(Prompt,String)) :-
	!,
	write(Prompt),
	read_string("\n",_,String).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
  \item [read\_string({\em String})]\index{ProTop!read\_string}\ \\
  This reads all characters up to the next newline into the string {\em
  String}. The end of file also terminates the reading.
  %
\begin{BoxedSample}
ProTop -> |read_string(String), writeln(String).|
 |some string terminated by newline|
some string terminated by newline
ok.
\end{BoxedSample}
\end{description}

\PL*/
pt(read_string(String)) :-
	!,
	read_string("\n",_,String).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
  \item [read({\em Term})]\index{ProTop!read}\ \\
  This instruction reads a prolog term from the standard input and unifies it
  with {\em Term}. If the unification fails then this command fails. The term
  has to be terminated by a colon.
  %
\begin{BoxedSample}
ProTop -> |read(Term), writeln(Term).|
\ |some_term(Var,const).|
some\_term(Var, const)
ok.
ProTop -> |read(term), writeln(Term).|
\ |some_term(Var,const).|
no.
\end{BoxedSample}
\end{description}

\PL*/
pt(read(Term)) :-
	!,
	read(Term).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
  \item [read({\em Prompt}, {\em Term})]\index{ProTop!read}\ \\
  This instruction writes {\em Prompt} to the screen and reads a prolog term
  from the standard input and unifies it with {\em Term}. If the unification
  fails then this command fails. The term has to be terminated by a colon.
  %
\begin{BoxedSample}
ProTop -> |read("Term: ",Term), writeln(Term).|
Term:  |some_term(Var,const).|
some\_term(Var, const)
ok.
\end{BoxedSample}

\end{description}

\PL*/
pt(read(Prompt,Term)) :-
	!,
	write(Prompt),
	read(Term).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
  \item [call({\em Goal})]\index{ProTop!call}\ \\
  This instruction forwards {\em Goal}\ to the underlying Prolog for execution.
  The Prolog goal |Goal| is called in the module |eclipse|.

  This instruction is strongly discouraged. It may be disabled in a future
  version of \ProTop.
\end{description}

\PL*/
pt(call(Goal)) :-
	!,
	set_error_handler(68,protop_error_handler/3),
	call(Goal,eclipse),
	reset_error_handler(68).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\subsection{Options}

\begin{description}
  \item [{\em Option} = {\em Value}]\index{ProTop!=}\ \\
  This instruction sets or gets the value of the option {\em Option}. If {\em
  Value} is not a variable then this value is stored in {\em Option}.
  Otherwise {\em Value}\/ is unified with the actual value of {\em Option}.
  {\em Option} is required to be a symbol.

\begin{BoxedSample}
  ProTop -> |verbose = off.|
  ok.
  ProTop -> |verbose = VERB, writeln(VERB).|
  off
  ok.
\end{BoxedSample}
\end{description}

\PL*/
pt(Option = Value) :-
	!,
	atom(Option),
	( var(Value) ->
	    is_option(Option,Value)
	;   set_option(Option=Value)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}

  \item [reset\_options]\index{ProTop!reset\_options}\ \\
  This instruction resets all options to their default values. It always
  succeeds and produces no output.
\end{description}

The predicate |default_option/1| is defined in the file {\sf protop.cfg}. It
contains default options which are given in the Makefile.

\PL*/
pt(reset_options) :-
	!,
	reset_all_options,
	(   default_option(X),
	    set_option(X),
	    fail
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
pt(library_path(Path)) :-
	!,
	get_flag(library_path,LP),
	( var(Path) ->
	    Path = LP
	; string(Path) ->
	    ( member(Path,LP) ->
		true
	    ;	set_flag(library_path,[Path|LP])
	    )
	; atom(Path), Path \== [] ->
	    atom_string(Path,P),
	    ( member(P,LP) ->
		true
	    ;	set_flag(library_path,[P|LP])
	    )
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}

  \item [prove({\em File})]\index{ProTop!prove}\ \\
  This instruction performs compilation and running of the compiled prover.
\end{description}

\PL*/
pt(prove(InputFile)) :-
	!,
	pt(['ProTop:prover_file'=ProverFile, prove(InputFile,ProverFile)]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
pt(prove(InputFile,OutFile)) :-
	!,
	prove(InputFile,OutFile).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\subsection{Report Generation}



\PL*/
pt(report(Action)) :-
	!,
	protop_report(Action).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
pt(define_table(Name,Spec)) :-
	!,
	proof_report_define_table(Name,Spec).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
pt(remove_table(Name)) :-
	!,
	proof_report_remove_table(Name).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}

  \item [generate\_report]\index{ProTop!generate\_report}\ \\
  This instruction reads the log file and generated the \LaTeX{} source from
  it. This report file can be processed using |latex_report| of processed by
  \LaTeX{} manually.

  {\bf Note:} A \LaTeX{} style file is required to process the generated
  \LaTeX{} code. This style file can be found in the {\sf input} subdirectory
  of the \ProTop{} installation directory.
\end{description}

\PL*/
pt(generate_report) :-
	!,
	pt([log_file=ReportFile, generate_report([ReportFile])]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}

  \item [generate\_report({\em Files})]\index{ProTop!generate\_report}\ \\
  This instruction reads the file {\em Files} and generated the \LaTeX{}
  source from it.
\end{description}

\PL*/
pt(generate_report(Files)) :-
	!,
	proof_report(Files).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}

  \item [latex\_report]\index{ProTop!latex\_report}\ \\
  This instruction tries to run \LaTeX{} on a formerly generated report.
\end{description}

\PL*/
pt(latex_report) :-
	!,
	proof_report_latex.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}

  \item [make\_report]\index{ProTop!make\_report}\ \\
  This instruction generates a report and runs \LaTeX{} on the resulting
  source file. The file given in the option |log_file| is used for analysis.
\end{description}

\PL*/
pt(make_report) :-
	!,
	pt([log_file = ReportFile,
	    generate_report([ReportFile]),
	    latex_report]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}

  \item [make\_report({\em Files})]\index{ProTop!make\_report}\ \\
  This instruction generates a report from the files {\em Files}\ and runs
  \LaTeX{} on the resulting source file.
\end{description}

\PL*/
pt(make_report(Files)) :-
	!,
	pt([generate_report(Files), latex_report]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\subsection{Macros and Error Handling}

Macros are a means to ease live. Tasks which have to be performed repeatedly
can be defined as a macro and executed when necessary. The simplest case is
a macro as abbreviation for some other instructions as in the following
example:
\begin{BoxedSample}
  ProTop -> |my_macro := write("hello "),|
  |            write("world"), write("."), nl.|
  ok.
  ProTop -> |my_macro.|
  hello world.
  ok.
\end{BoxedSample}
Ok, it's rather simple. This macro simply writes |hello world.| followed by a
newline --- an effect one can achieve simpler. The execution is triggered by a
call with its name.

Macros may also have arguments. Prolog variables are used to denote variable
parts of the arguments. Let us again consider out previous example and make it
a little bit more complicated:
\begin{BoxedSample}
  ProTop -> |my_macro(Arg) :=|
  |           write("hello "), write(Arg), write("."), nl.|
  ok.
  ProTop ->
\end{BoxedSample}

Note that this macro can coexist with the previous one since the number of
actual parameters distinguishes them. If we call this macro with the command
|my_macro("world").| we get the same as before. But now we can also specify
another argument, as in
\begin{BoxedSample}
  ProTop -> |my_macro("world").|
  hello world.
  ok.
  ProTop -> |my_macro("friends").|
  hello friends.
  ok.
\end{BoxedSample}

The coexistence of macros goes further. The selection of an appropriate macro
is done via unification of its head. We exploit this fact by further
developing our example:
\begin{BoxedSample}
  ProTop -> |my_macro(all,999) := my_macro("world").|
  ok.
  ProTop -> |my_macro(some,12) := my_macro("friends").|
  ok.
\end{BoxedSample}

These two instructions establish {\em two} definitions of |my_macro/2|. Now we
can test them and see how they work:

\begin{BoxedSample}
  ProTop -> |my_macro(all,X).|
  hello world.
  ok.
  ProTop -> |my_macro(some,X).|
  hello friends.
  ok.
  ProTop -> |my_macro(A,B), writeln('A'=A), writeln('B'=B).|
  hello world.
  A = all
  B = 999
  ok.
\end{BoxedSample}

\PL*/
pt(op(A,B,C)) :-
	!,
	op(A,B,C).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
  \item [{\em Macro} := {\em Code}]\index{ProTop!:=}\ \\
  This instruction stores {\em Code} as replacement text for {\em Macro}.
  Several clauses for the same macro can be specified.
\end{description}

\PL*/
pt((Macro := Code)) :-
	!,
	assert('ProTop macro'(Macro,Code,user)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\begin{description}

  \item [delete macro({\em Macro})]\index{ProTop!delete macro}\ \\
  This instruction deletes all macros that unify with {\em Macro}. As a
  consequence we can delete all macros with the command
  %
\begin{BoxedSample}
  ProTop -> |delete macro(_).|
  ok.
\end{BoxedSample}

  Note that the underscore |_| is the anonymous variable like in Prolog.
  A more natural use would be the following one:
  %
\begin{BoxedSample}
  ProTop -> |delete macro(my_private_macro(_,_)).|
  ok.
\end{BoxedSample}

  This example deletes all entries for the macro |my_private_macro| with two
  arguments.
\end{description}

\PL*/
pt(delete_macro(Macro)) :-
	!,
	retract_all('ProTop macro'(Macro,_,user)).
pt(delete(macro(Macro))) :-
	!,
	retract_all('ProTop macro'(Macro,_,user)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}

  \item [defined({\em Macro})]\index{ProTop!defined}\ \\
  This instruction checks for the existence of macros which unify with {\em
  Macro}. The return status indicates success or failure.
  %
\begin{BoxedSample}
ProTop -> |defined(mac).|
no.
ProTop -> |mac := true.|
ok.
ProTop -> |defined(mac).|
ok.
\end{BoxedSample}
\end{description}

\PL*/
pt(defined(Macro)) :-
	!,
	\+ \+ 'ProTop macro'(Macro,_,_).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}

  \item [{\em Macro}]\ \\
  A macro is executed when its name is encountered as command. Since the
  built-in commands have precedence over macros it is not possible to redefine
  a built-in.
\end{description}

\PL*/
pt(Macro) :-
	( 'ProTop macro'(Macro,Code,_) ->
	    !,pt(Code)
	; functor(Macro,F,A),
	  functor(Template,F,A),
	  'ProTop macro'(Template,_,_) ->
	    err("*** No matching clause for macro ",Macro),
	    fail
	;   err("*** Instruction not found: ",Macro),
	    fail
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate protop_status/1(+Spec).

\begin{description}
  \item [status({\em Part})]\index{ProTop!status}\ 
    \\
    This instruction displays the status of the requested part {\em Part} to
    the screen. {\em Part}\/ can also be a list of parts in which case the
    status of all parts given in the list is displayed. The following parts
    can be specified:
    \begin{description}
    \item [version]\ \\ prints the version of \ProTop.
    \item [macros]\ \\  prints a summary of macros defined in \ProTop.
    \item [prover]\ \\  prints a list of provers loaded.
    \item [filter]\ \\  prints a list of filters loaded.
    \item [matrix]\ \\  prints a summary of the matrix currently loaded.
    \item [clauses]\ \\ prints a list of clauses currently stored in the
      matrix.
    \item [contrapositives]\ \\ prints a list of contrapositives currently
      stored in the matrix.
    \item [modules]\ \\ prints a list of the loaded modules and the associated
      documentation strings.
    \item [module({\em Module}\/)]\ \\ prints documentation string for module
      {\em Module}. It fails if {\em Module}\/ is not instanciated properly or
      it contains no \ProTop{} module.
    \item [options]\ \\ prints a list of all options.
    \item [option({\em Option}\/)]\ \\ prints the value of the option {\em
        Option}. 
    \item [file({\em File}\/)]\ \\ displays the contents of the file {\em
        File} on the screen.
    \item [macros]\ \\ prints the list of all macros to the screen.
    \item [macro({\em Macro}\/)]\ \\ displays the definition of all macros for
      which the head unify with {\em Macro}.
    \item [all]\ \\     does {\bf version}, {\bf filter}, {\bf prover}, {\bf
        matrix} and {\bf macros}.
    \end{description}

    This instruction fails if one of the requested parts is not known or it is
    called with a variable argument.

  \item [info]\index{ProTop!info}\ 
    \\
    This is a variant of |status|. For details see above.

  \item [list]\index{ProTop!list}\ 
    \\
    This is a variant of |status|. For details see above.

  \item [show]\index{ProTop!show}\ 
    \\
    This is a variant of |status|. For details see above.

\end{description}


\PL*/
protop_status(X) :-
	var(X),
	!,
	fail.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
protop_status([]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
protop_status([H|T]) :-
	protop_status(H),
	protop_status(T).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
protop_status(all) :-
	protop_status([version,filter,prover,matrix,macros]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
protop_status(version) :-
	protop_version(Version),
	printf("  This is ProTop version %w\n",[Version]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
protop_status(commands) :-
	printf("The following commands and constructs are recognized:\n",[]),
	(   pt_command(Command),
	    printf("\t%w\n",[Command]),
	    fail
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
protop_status(macros) :-
	printf("  Macros:\n",[]),
	findall(x,'ProTop macro'(_,_,_),Macros),
	length(Macros,Nmacs),
	printf("\t%w macros are defined.\n",[Nmacs]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
protop_status(macro(Macro)) :-
	( \+ 'ProTop macro'(Macro,Value,_) ->
	    ( var(Macro) ->
		err("*** No macro defined.")
	    ;	err("*** Macro ",Macro," is not defined.")
	    ),
	    fail
	;   'ProTop macro'(Macro,Value,_),
	    printf("%QDw := %QDw\n",[Macro,Value]),
	    fail
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
protop_status(options) :-
	setof((A=B),is_option(A,B),Options),
	(   member((X=V),Options),
	    printf("\t%q\n",[(X=V)]),
	    fail
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
protop_status(option(Option)) :-
	(   is_option(Option,Value),
	    printf("\t%w\n",[(Option=Value)]),
	    fail
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
protop_status(module(Module)) :-
	module_info(Module,Type,Version,Description),
	printf("--- %w module %w [%w]\n%w\n",
	       [Type,Module,Version,Description]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
protop_status(prover) :-
	printf("  Provers:\n",[]),
	(   module_info(Prover,prover,_,_),
	    printf("\t%w\n",[Prover]),
	    fail
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
protop_status(filter) :-
	printf("  Filters:\n",[]),
	(   module_info(Filter,filter,_,_),
	    printf("\t%w\n",[Filter]),
	    fail
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
protop_status(matrix) :-
	printf("  Matrix:\n",[]),
	findall(x,'Contrapositive'(_,_,_),Contrapositives),
	( Contrapositives == [] ->
	    printf("\tNo matrix loaded.\n",[])
	;   length(Contrapositives,NumberOfContrapositives),
	    findall(x,'Clause'(_),Clauses),	
	    length(Clauses,NumberOfClauses),
	    findall(x,'GoalClause'(_),Goals),
	    length(Goals,NumberOfGoals),
	    plural(NumberOfContrapositives,
		   contrapositive,
		   contrapositives,
		   CP_contrapositives),
	    plural(NumberOfClauses,clause,clauses,C_clauses),
	    plural(NumberOfGoals,
		   ["is a",clause],
		   [are,clauses],
		   [G_are,G_goals]),
	    printf("\t%w %w stored for\n\t%w %w of which\n\t%w %w goal %w\n",
		   [NumberOfContrapositives,
		    CP_contrapositives,
		    NumberOfClauses,
		    C_clauses,
		    NumberOfGoals,
		    G_are,
		    G_goals]),
	    is_option(input_file,InputFile),
	    ( empty_option(InputFile) ->
		true
	    ;   printf("\tpresumedly from file %w\n",[InputFile])
	    )
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
protop_status(clauses) :-
	printf("  Matrix:\n",[]),
	show_matrix("  ",output).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
protop_status(contrapositives) :-
	printf("  Contrapositives:\n",[]),
	show_contrapositives("  ",output).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
protop_status(modules) :-
	printf("  Modules:\n",[]),
	(   current_module(Module),
	    \+ is_locked(Module),
	    call(current_predicate(info/3),Module),
	    call(info(Type,_Version,Doc),Module),
	    printf("%w module %w:\n\t%w\n",[Type,Module,Doc]),
	    fail
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
protop_status(module(Module)) :-
	atom(Module),
	current_module(Module),
	\+ is_locked(Module),
	call(current_predicate(info/3),Module),
	call(info(Type,Version,Doc),Module),
	printf("%w module %w (%w):\n\t%w\n",[Type,Module,Version,Doc]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
protop_status(file(File)) :-
	( \+ file_type(File) ->
	    err("*** Type error in ",show(File)),
	    fail
	; \+ exists(File) ->
	    err("*** File ",File," not found."),
	    fail
	;
	    open(File,read,Stream),
	    repeat,
	    ( at_eof(Stream) ->
		true
	    ;	read_string(Stream,"\n",_,String),
		writeln(String),
		fail
	    ),
	    close(Stream),
	    !
	).
protop_status(File) :-
	string(File),
	protop_status(file(File)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate plural/4(+Number, +Singular, +Plural, ?Form).

This predicate checks a number |Number| and unifies |Form| with the singular
|Singular| or the plural |Plural| as required.

\PL*/
plural(N,Singular,Plural,Form) :-
	( N == 1 ->
	    Form = Singular
	;   Form = Plural
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate add_dir/3(+Directory, +FileList, ?FullFileList).

This predicate prepends the directory |Directory| to each file in the list
|FileList|. The resulting list of extended files is returned in
|FullFileList|.

\PL*/
add_dir(_,[],[]).
add_dir(Dir,[H|T],[DH|DT]) :-
	concat_atom([Dir,'/',H],DH),
	add_dir(Dir,T,DT).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate file_type/1(+File).

This predicate checks |File| for valid file types. File names can be any
string or atom except the empty list --- which is an atom but not considered
as a valid file name. If you need it as file name make it a string!

\PL*/
file_type(Name) :-
	string(Name).
file_type(Name) :-
	atom(Name),
	Name \== [].
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^




\Predicate protop_report/1(+Item).

\PL*/
protop_report(table(A,B)) :-
	Term =.. [table,A,B],
	log_put(Term).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
  \item [report comment({\em Text})]\index{ProTop!report comment}\ \\
  This instruction writes the text {\em Text} as comment to the report file.
  This report file has to be initialized with |report init| before this
  instruction is used.
\end{description}

\PL*/
protop_report(comment(Text)) :-
	Term =.. [comment,Text],
	log_put(Term).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}

  \item [report text({\em Text})]\index{ProTop!report text}\ \\
  This instruction writes the text {\em Text} as descriptive text to the
  report file. The report file has to be initialized with |report init|
  before this instruction is used. 
\end{description}

\PL*/
protop_report(text(Text)) :-
	Term =.. [text,Text],
	log_put(Term).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
  \item [report section({\em Text})]\index{ProTop!report section}\ \\
  This instruction writes the text {\em Text} as short title of the following
  proof attempt to the report file. It may also be used when tables are
  typeset.  The report file has to be initialized with |report init|
  before this instruction is used. 
\end{description}

\PL*/
protop_report(section(Text)) :- 
	Term =.. [section,Text],
	log_put(Term).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
protop_report(label(Text)) :-
	Term =.. [label,Text],
	log_put(Term).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}

  \item [report title({\em Text})]\index{ProTop!report title}\ \\
  This instruction writes the text {\em Text} to the report file. This text is
  intended to be used as author of an automatically generated report.
\end{description}

\PL*/
protop_report(title(Text)) :-
	Term =.. [title,Text],
	log_put(Term).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}

  \item [report author({\em Text})]\index{ProTop!report author}\ \\
  This instruction writes the text {\em Text} to the report file. This text is
  intended to be used as title of an automatically generated report.
\end{description}

\PL*/
protop_report(author(Text)) :-
	Term =.. [author,Text],
	log_put(Term).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\begin{description}
  \item [report init({\em ReportFile})]\index{ProTop!report init}\ \\
  This instruction initializes the report file {\em ReportFile}. Subsequent
  actions may leave a message in this file. Depending on the value of the
  option |ProTop:backup| an existing file is saved or overwritten.
\end{description}

\PL*/
protop_report(init(ReportFile)) :-
	( ( \+ string(ReportFile),
	    \+ atom(ReportFile)) ->
	    err("*** Type error: ",report init(ReportFile)),
	    fail
	;
	    ( is_option('ProTop:backup'),
	      exists(ReportFile) ->
		between(1,30000,1,Bak),
		concat_string([ReportFile,".~-",Bak,"~"],Bak_File),
		\+ exists(Bak_File),
		rename(ReportFile,Bak_File),
		msg("--- Old report file moved to ",Bak_File)
	    ;	true
	    ),
	    set_option(log_file=ReportFile),
	    prove_log([user,date,host])
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


\begin{description}
  \item [report init]\index{ProTop!report init}\ \\
  This instruction initializes the report file from the option |log_file|. See
  also above.
\end{description}

\PL*/
protop_report(init) :-
	pt([log_file = ReportFile]),
	protop_report(init(ReportFile)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

As the final action of the loading phase of this module we force the reset of
the options to their default values.

\PL*/
:- pt(reset_options).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
