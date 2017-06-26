%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: hook.pl,v 1.13 1995/02/13 20:05:14 gerd Exp $
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

:- module_interface(hook). /*%-------------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.13 $}

Hooks are a means to allow  procedures to be  inserted without modification of
the code. The idea   and terminology is	 well  known in Lisp  programming.  To
implement a similar behaviour in \eclipse{} we propose this module.

This module provides several procedures to execute hooks. The usage is done as
follows. If a module or file wants to provide a hook the command |run_hooks/1|
has to be used.	 The argument of  this command is  a Prolog term.  This Prolog
term is called as goal in the module {\sf hook}.

If some	 module	 or file wants	 to add a  procedure to	 a hook	 a  one clause
procedure has to  be defined. The name has  to be {\tt hook:}{\it name}, where
{\it name}\/ is the name of the hook to be  chosen by the user. This procedure
has to be declared |multifile| and |dynamic|.

To illustrate the use of hooks let us consider the following example.  Suppose
we have a module {\sf write} which provides  the predicates |write_string| and
|newline|.

\begin{BoxedSample}
\begin{verbatim}
  :- use_module(hooks).

  :- define_hook write_string/1.
  :- define_hook newline/0.

  write_string(String) :-
          write(String),
          run_hooks(write_string,[String]).

  newline :- 
          nl,
          run_hooks(newline).
\end{verbatim}
\end{BoxedSample}


In both  procedures hooks are used  after the job is  done. Those hooks can be
used by  other modules to  perform additional   tasks. E.g.\ the  module  {\sf
protocal} can use the hooks to write a protocol to an output file.

\begin{BoxedSample}
\begin{verbatim}
  :- use_module(hooks).

  :- hook(write_string,write_string/1).
  :- hook(newline,newline/0).

  write_string(S) :-
          get_protocol_file(File),
          write(File,S).
  newline :-
          get_protocol_file(File),
          nl(File).
\end{verbatim}
\end{BoxedSample}

Another	 module	 may need   to fix the	 the  current column. This  module can
simultaneously use the same hooks to  monitor the characters written and store
the current column position.

After we  have seen the	 idea we  can come  to	the implementation.  Hooks are
implemented  as	 a  module.  The other	modules	 dynamically  make and enlarge
definitions in this  module. Special  routines	are provided  to execute  each
procedure hooked in turn.

\PL*/
:- export run_hooks/1,
	  run_hooks/2,
	  (define_hook)/2,
	  (unhook)/3,
	  (hook)/3.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We declare |define_hook| and |hook| as prefix operators. This is mainly
syntactic sugar.
\PL*/
:- op(1150,fx,(define_hook)).
:- op(1150,fx,(hook)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The predicates |define_hook/2|, |hook/3|, and |unhook/3| need the module from
which they are called. The tool concept of \eclipse{} allows us to get this
information without forcing the user to specify it.

\PL*/
:- tool((define_hook)/1,(define_hook)/2).
:- tool((hook)/2,(hook)/3).
:- tool(unhook/2,unhook/3).

:- begin_module(hook).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The hooks are stored in the Prolog data base in this module.

\PL*/
:- dynamic 'Hook'/2.
:- dynamic 'Hook'/3.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate define_hook/2(+Hook, +Module).

This predicate stores the information that there  is a hook in	a module.  The
information is stored in the dynamic predicate	|Hook/2| in the module hook. A
tool   interface is used to    get the name  of the    module provided by  the
\eclipse{} system.

\PL*/
define_hook(Hook,Module) :-
	assert('Hook'(Hook,Module)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate hook/3(+Hook, +PredSpec, +Module).

This predicate stores the information that a predicate	is attached to a hook.
This  information  is stored in the  dynamic  predicate |Hook/3| in the module
hook. Again the module name is usually provided by an \eclipse{} tool.

\PL*/
hook(Hook,Pred/Arity,Module) :-
%	printf("%n defining hook %w %w %w%n",[Hook,Pred/Arity,Module]),
	( 'Hook'(Hook,Module,Pred/Arity) ->
	    true
	;   assert('Hook'(Hook,Module,Pred/Arity))
        ).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate unhook/3(+Hook, +PredSpec, +Module).

This   predicate can be used  to  remove a predicate  from  a  hook. It is the
inverse of the predicate |hook/3|.

\PL*/
unhook(Hook,Pred/Arity,Module) :-
	retract_all('Hook'(Hook,Module,Pred/Arity)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate run_hooks/1(+Hook).

This  procedure runs all procedures  of	 a hook in  turn. In  any case it will
succeed and will not have an alternate solution. An exception might occur when
a hook which is not declared is called.

Each hook is  called in turn  using a failure driven  loop.  To avoid unsocial
behaviour of a hook ---	 using a cut --- the  first step of selecting a clause
and unifying the head is done manually.

\PL*/
run_hooks(Hook) :-
	(   'Hook'(Hook,Module,Pred/0),
	    call(Pred,Module),
	    fail
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate run_hooks/2(+HookName, +ArgumentList).

This procedure has the same behaviour as |run_hooks/1| except that the
hook is constructed from the the name given as first argument and the
list of arguments given as second argument.

\PL*/
run_hooks(Hook,Args) :-
	(   'Hook'(Hook,Module,Pred/_),
	    Goal =.. [Pred|Args],
	    call(Goal,Module),
	    fail
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- skipped run_hooks/1,
	  run_hooks/2,
	  (define_hook)/2,
	  (unhook)/3,
	  (hook)/3.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
