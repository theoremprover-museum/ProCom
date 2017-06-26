%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: options.pl,v 1.6 1995/01/30 19:35:06 gerd Exp $
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

:- module_interface(options). /*%----------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.6 $}

Many software  systems should be designed  configurable. This  module provides
support for such a  configuration. A configuration is  determined by a set  of
options --- sometimes called flags.

A  option consists of a name  and a value. Thus options	 can be seen as global
variables. In addition this implementation provides a default or initial value
to every option which can be used even if the actual value has been modified.

The name  of a option  is a unique atom. According   to the value  and the use
basically two types of	options can be	distinguished --- the  boolean options
and the term valued options. Boolean options are  supposed to take the boolean
values |on| and	 |off| only. Boolean  options are  a special type  of the more
general term valued options. Term valued options can  have any Prolog terms as
values.

There are    several   basic  operations   on  options:	    declaration	  (and
initialization), modification, and  accessing the value. Predicates  for those
operations are exported by this module.

Before we describe the predicates in this module  we give a short illustrative
example of the use of options.

Suppose we have a Prolog  module to handle the output.	Here a option might be
appropriate to suppress the  output is desired. Thus  a option is declared ---
using the  |define_option| primitive. Afterwards it  can  be used to  check if
output should be produced or suppressed using the primitive |is_option/1|.

\begin{BoxedSample}
\begin{verbatim}
  :- use_module(options).
  :- define_option output_verbose = on.

  write_string(String) :-
          ( is_option(output_verbose) ->
              write(String)
          ;   true
          ).
\end{verbatim}
\end{BoxedSample}

After this module is loaded the user can redefine  the options declared within
this module  using the predicate  |set_option/1|. A fragment of another module
might look like

\begin{BoxedSample}
\begin{verbatim}
  :- use_module(options).

  set_switches :-
          set_option( output_verbose = off ).
          
  go :-
          reset_all_options,
          set_switches,
          write_string('Welcome to the show ...'),
          ...
\end{verbatim}
\end{BoxedSample}

All required  when  you change	your  mind  about options  is  to  modify  the
predicate |set_switches| to  reflect  your desired  option settings. This  can
even be done in an additional startup file. Thus the only thing required is to
load the appropriate Prolog files.

Well  there is one more	 thing to say  about options.  Options are {\em NOT}\/
local to modules. Thus you have	 to be careful	when choosing a option name. I
propose to use	the name of the actual	module as initial string when choosing
an option name.

Now we come to the detailed implementation issues.
\PL*/
:- op(1150,fx,'define_option').

:- export is_option/2,
	  is_option/1,
	  set_option/1,
	  reset_all_options/0,
	  load_options/1,
	  (define_option)/1,
	  empty_option/1.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- begin_module(options).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This version  is an implementation  which uses the  indexed Prolog database to
store options.

\PL*/
:-	lib(lists),
	lib(message).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate is_option/2(Name,Value).

This is the central predicate in this module. The indexed Prolog data base is
used to store name/value pairs of options in this predicate. Thus the general
access predicate is simply a table of name/value pairs.
\PL*/
is_option(Name,Value) :-
	atom(Name),
	concat_atoms('OPTION INIT: ',Name,Key),
	recorded(Key,_),
	!,
	concat_atoms('OPTION: ',Name,KeyVal),
	recorded(KeyVal,Value).
is_option(Name,Value) :-
	var(Name),
	recorded_list('Option List',Names),
	member(Name,Names),
	concat_atoms('OPTION: ',Name,KeyVal),
	recorded(KeyVal,Value).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate is_option/1(Name).

Since boolean  options play a  special role  we	 provide a  predicate to  test
wether a boolean option |Name| not turned |off|.

\PL*/
is_option(Option) :- 
	( Option = ( Name = Value ) ->
	    is_option(Name,Value)
	;   is_option(Option,on)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate define_option/1(Name=Value).

Initialize an option with a given value. Several type checks are performed and
appropriate warning messages issued.

As syntactic sugar |define_option| is declared as prefix operator.

\PL*/
define_option Name=InitValue :-
	( atom(Name) ->
	    concat_atoms('OPTION INIT: ',Name,Key),
	    concat_atoms('OPTION: ',Name,KeyVal),
	    ( recorded(Key,OldValue) ->
		( OldValue == InitValue ->
		    true
		;   err("*** Option >",Name,
			"< already defined. Using new default value.")
		),
		erase_all(Key)
	    ;
		record('Option List',Name)
	    ),
	    record(Key,InitValue),
	    ( recorded(KeyVal,_) ->
		true
	    ;	record(KeyVal,InitValue)
	    )
	;
	    err("*** Type error. Option should be an atom: ",Name)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate reset_all_options/0().

Reset all options to  their initial value. This	 predicate is implemented as a
failure driven loop with generator |option_name/2|.

\PL*/
reset_all_options :-
	(   recorded('Option List',Name),
	    concat_atoms('OPTION: ',Name,KeyVal),
	    concat_atoms('OPTION INIT: ',Name,Key),
	    recorded(Key,Value),
	    erase_all(KeyVal),
	    record(KeyVal,Value),
	    fail
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate set_option/1(+ListOfOptions).

This  predicate takes a single	pair {\em Name	|=| Value} or	a list of such
pairs and resets the option {\em Name} to the given value. Certain type checks
are preformed and appropriate warning messages given.

\PL*/
set_option([]).
set_option([Option|OtherOptions]) :-
	set_option(Option),
	set_option(OtherOptions).
set_option(Name=Value) :-
	( atom(Name) ->
	    concat_atoms('OPTION: ',Name,KeyVal),
	    erase_all(KeyVal),
	    record(KeyVal,Value)
	;   err("*** Type error. Option should be an atom: ",Name),
	    fail
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate load_options/1(+File).

This preedicate reads options from a file. The	file msut comtain lines of the
form {\em Option =  Value.} Prolog style  comments are allowed. Other type  of
Prolog terms are ignored.

\PL*/
load_options(File) :-
	open(File,read,Stream),
	repeat,
	read(Stream,Term),
	( Term = end_of_file ->
	    true
	; Term = (_=_) ->
	    set_option(Term),
	    fail
	),
	close(Stream),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate empty_option/1(?Option).

This predicate compares	 its argument |Option| with  an empty option. Anything
that is considered empty is the empty symbol (|''|),  the empty string (|""|),
the empty list (|[]|) or the symbol |off|. The comparison is done with respect
to identity. A variable argument will not be bound and it is not considered as
empty.

\PL*/
empty_option(Opt) :- Opt == ''.
empty_option(Opt) :- Opt == "".
empty_option(Opt) :- Opt == [].
empty_option(Opt) :- Opt == off.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Since this module seems to be fairly stable we disable the debugger for any
predicates exported. Finally we lock the module to hide any internals.

\PL*/
:- skipped is_option/2,
	   is_option/1,
	   set_option/1,
	   reset_all_options/0,
	   load_options/1,
	   (define_option)/1,
	   empty_option/1.
:- lock(options).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
