%%*****************************************************************************
%% $Id: dynamic_module.pl,v 1.2 1995/02/13 20:05:14 gerd Exp $
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

:- module_interface(dynamic_module). /*%---------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.2 $}

This module provides ther interface to the dynamic modules of \ProTop. Dynamic
modules are used whenever some modules can be specified by the user at run
time. To ensure that the appropriate module is called it is checked that a
declaration is contained in the module. This means that the predicate |info/3|
has to be defined:

{\em |info(|Type|,|Version|,|Description|)|}

{\em Type}\/ is an atom which identifies the type of the module. {\em
  Version}\/ is an identification of the version. It is a string or an atom.
E.g. RCS may be used to get this version automatically. {\em Description}\/ is
a short informative text about this module. It is also a string or an atom.
This might be presented to the user.

The following identification is taken from the module |procom|:

\begin{BoxedSample}
  info(prover,
       "$Revision: 1.2 $",
       "ProCom is a prover based on the PTTP technique.").
\end{BoxedSample}

\PL*/
:- export load_module/6,
	  load_and_call/6,
	  module_info/4.
:- begin_module(dynamic_module).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:-	lib(find_file),
	lib(message).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate load_module/6(+Goal, +Module, +Type, +Path, +Extension, +Message).

\PL*/
load_module([H|T],Module,Type,Path,Extension,Message) :-
	!,
	( \+ atom(Module) ->
	    err("*** The module specification is not an atom: ",Module),
	    fail
	; current_module(Module) ->
	    true
	; find_file(Module,Path,Extension,File) ->
	    compile(File),
	    msgs(Message)
	;   err("*** The module ",Module," could not be found on ",Path),
	    fail
	),
	( \+ current_module(Module) ->
	    err("*** ",Module," seems not to be loadable (no module)."),
	    fail
	; \+ call(current_predicate(info/3),Module) ->
	    err("*** Module ",Module," does not identify itself."),
	    fail
	; \+ call(info(Type,_,_),Module) ->
	    err("*** Module ",Module," does not identify itself as ",Type,"."),
	    fail
	;   ensure_callable([H|T],Module)
	).
load_module([],Module,Type,Path,Extension,Message) :-
	!,
	load_module([true/0],Module,Type,Path,Extension,Message).
load_module(F/A,Module,Type,Path,Extension,Message) :-
	!,
	load_module([F/A],Module,Type,Path,Extension,Message).
load_module(Goal,Module,Type,Path,Extension,Message) :-
	functor(Goal,F,A),
	load_module([F/A],Module,Type,Path,Extension,Message).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate load_and_call/6(+Goal, +Module, +Type, +Path, +Extension, +Message).

\PL*/
load_and_call(Goal,Module,Type,Path,Extension,Message) :-
	load_module(Goal,Module,Type,Path,Extension,Message),
	call(Goal,Module).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate ensure_callable/2(+SpecList, +Module).

This predicate checks that a list of predicates |SpecList| is defined in the
module |Module|. SpecList is a list of specifications of the form {\em
  functor|/|arity}.

\PL*/
ensure_callable([],_).
ensure_callable([F/A|T],Module) :-
	( call(current_predicate(F/A),Module) ->
	    ensure_callable(T,Module)
	;   err("*** Error: The predicate ",F/A," is not defined in ",Module),
	    fail
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate module_info/4(?Module, ?Type, ?Version, ?Description).

\PL*/
module_info(Module,Type,Version,Description) :-
	current_module(Module),
	\+ is_locked(Module),
	call(current_predicate(info/3),Module),
	call(info(Type,Version,Description),Module).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- skipped load_module/6,
	   load_and_call/6,
	   module_info/4.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
