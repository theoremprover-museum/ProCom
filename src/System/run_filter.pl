%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: run_filter.pl,v 1.12 1995/03/30 13:09:18 gerd Exp $
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

:- module_interface(run_filter). /*%-------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.12 $}

Filters are   a	 means to  process  a	stream of Prolog  terms	 with  several
predicates.  This idea	is used	 in \ProTop{} to  allow a  modification of the
input matrix before it is parsed and stored in the module matrix.

This section describes how to write an input filter for \ProTop. A filter is a
simple \eclipse{} module which satisfies certain restrictions. The most simple
filter is  delivered  as  the  file  {\sf Filters/none.pl}. This   filter does
nothing but  pass everything it	 reads to the  next filter. It gives a general
structure for any special filter.

The first thing we have to take care of is  the naming convention for filters.
Three entities which can be used independently are now linked together:
\begin{enumerate}
  \item The name of the module. Let's call it {\em filter} for the moment.
  \item The  name  of the file containing   the	 module. This  has  to be {\em
      filter.pl}, i.e. the name of the module plus an extension of {\tt .pl}.
  \item The only    predicate exported by     {\em filter}\/ has to   be  {\em
      filter/2}.
\end{enumerate}

Thus the  beginning  of our filter {\em	  filter} in the file  {\em filter.pl}
looks as follows:

\begin{BoxedSample}\raggedright\tt
  :- module\_interface(filter).
  :- export filter/2.
  :- begin\_module(filter).
  :- lib(op\_def).
\end{BoxedSample}

The final  instruction loads the library  containing  definition of operators.
Especially the \verb|#| is defined there. The meaning  and use of this functor
will be described later.

The  predicate {\em filter/2}  takes as first  argument an input stream and as
second	 argument an output stream.  Opening  and closing of  these streams is
performed by \ProTop. All {\em	filter/2} has to do  is to read from the input
stream and write the result to the output stream.

Since many Prolog terms may be waiting to be processed a loop  has to be used.
The termination condition is the term \verb|end_of_file|. If this term is read
{\em filter/2} should	return with success  without  leaving a	 choice point.
{\em filter/2} is not allowed to fail!

The non-failing condition can easily be satisfied by a failure driven loop. The
omission of a choice  point is guaranteed by a	terminating cut (which is good
practice for a failure driven loop anyhow).

\begin{BoxedSample}\raggedright\tt\obeyspaces
filter(Stream,OutStream) :-
	 repeat,
	 read(Stream,Term),
	 ( Term = end\_of\_file ->
	     true
	 ;   writeclause(OutStream,Term),
	     fail
	 ),
	 !.
\end{BoxedSample}

This simple  example can  now  be  enhanced  by replacing   the	 |writeclause|
predicate by a	more complicated construct which  transforms the term read and
writes the result to |OutStream|.

The predicate |writeclause|  is highly recommended  since it  is guaranteed to
produce output which can be  read back into Prolog\footnote{Currently this  is
not really true in \eclipse.}.

To allow certain filters  to have private informations	 in the input we  have
developed a  discipline of programming filters. The  |#| is assigned a special
meaning	 only in filters. |#| is  declared as prefix  operator.	 The following
rules must be honored:

\begin{itemize}
\item Any  |#| instruction not	understood by a	 filter must be passed	to the
  next filter unchanged.

\item The  construction	 |#|  {\em  Option}|=|{\em Value}|.|  is  reserved for
  assignments of options.

\item |#begin(|{\em section}|)| and |#end(|{\em	 section}|)| serve as grouping
  constructs.  Grouping	 constructs for the same  section   can not be nested.
  Unknown groups must be passed unchanged to the output stream.
\end{itemize}





\PL*/
:- export read_matrix_with_filter/2,
	  declare_filter/1.

:- begin_module(run_filter).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:-	lib(options),
	lib(find_file),
	lib(dynamic_module),
	lib(message),
	lib(time),
	lib(parse).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The search  path for filter files is  determined by  the option |filter:path|.
This option is initialized to the  value of the	 flag |library_path| --- there
all \eclipse{} libraries are  searched. Additionally the current  directory is
considered.

\PL*/
:-	get_flag(library_path,Path),
	(define_option 'filter:path'		 = ['.'|Path]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The filter files  are normal Prolog  file thus they  are searched as given and
with the additional extension  |.pl|. this is  the initial value of the option
|filter:extensions|.

\PL*/
:-	define_option 'filter:extensions'	= ['','.pl'].
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- dynamic filter_module/1.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate declare_filter/1(+Filter).

When a filter is  declared two operations are  performed. The information that
the filter  has been declared is  stored in the Prolog	data  base as the fact
|filter_module/1|.  Additionally the associated	 Prolog	 module is loaded. The
argument of the predicate |filter_module/1| is an atom. Thus a string argument
for   |Filter| is converted   into an atom. Other types	  of arguments are not
allowed and lead to a failure.

\PL*/
declare_filter(Filter) :-
	( string(Filter) ->
	    atom_string(F,Filter),
	    A = 0
	; atom(Filter) ->
	    F = Filter,
	    A = 0
	;   functor(Filter,F,A)
	),
	Arity is A + 2,
	is_option('filter:path',Path),
	is_option('filter:extensions',Extensions),
	( filter_module(F) ->
	    true
	; load_module([F/Arity],F,filter,Path,Extensions,[]) ->
	    assert(filter_module(F))
	;   fail
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate read_matrix_with_filter/2(+Filters, +File).

\PL*/
read_matrix_with_filter(Filter,File) :-
	( empty_option(Filter) ->
	    msg("% No input filter requested"),
	    read_matrix(File)
	; functor(Filter,'.',2) ->
	    is_option(input_path,Path),
	    is_option(input_extensions,Extensions),
	    ( find_file(File,['.'|Path],[''|Extensions],InFile) ->
		true
	    ;	err("*** Input file not found: ",File),
		fail
	    ),
	    open(InFile,read,InStream),
	    run_filter(Filter,InStream),
	    close(InStream)
	;
	    read_matrix_with_filter([Filter],File)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate run_filter/2(+FilterList, +Stream).

\PL*/
run_filter([],InStream):-
	read_matrix_from_stream(InStream).
run_filter([Filter|Rest],InStream) :-
	( string(Filter) ->
	    atom_string(F,Filter),
	    Args = []
	; atom(Filter) ->
	    F = Filter,
	    Args = []
	;   Filter =.. [F|Args]
	),
	( \+ filter_module(F) ->
	    declare_filter(F)
	;   true
	),

	( \+ filter_module(F) ->
	    err("*** The filter ",F," could not be loaded. Filter ignored."),
	    run_filter(Rest,InStream)
	;
	    concat_atom(["% Input filter ",Filter,": "],Pre),
	    pipe(In,Out),
	    Command =.. [F,InStream,Out|Args],
	    reset_time,
	    ( call(Command,F) ->
		true
	    ;	err("*** Filter ",F," aborted abnormally.")
	    ),
	    reset_time_and_print(Pre,"\n"),
	    close(Out),
	    run_filter(Rest,In),
	    close(In)
	),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
run_filter([Filter|Rest],InStream,OutStream) :-
	( string(Filter) ->
	    atom_string(F,Filter),
	    Args = []
	; atom(Filter) ->
	    F = Filter,
	    Args = []
	;   Filter =.. [F|Args]
	),
	( \+ filter_module(F) ->
	    declare_filter(F)
	;   true
	),

	( \+ filter_module(F) ->
	    err("*** The filter ",F," could not be loaded. Filter ignored."),
	    run_filter(Rest,InStream)
	;
	    concat_atom(["% Input filter ",Filter,": "],Pre),
	    pipe(In,Out),
	    Command =.. [F,InStream,Out|Args],
	    reset_time,
	    ( call(Command,F) ->
		true
	    ;	err("*** Filter ",F," aborted abnormally.")
	    ),
	    reset_time_and_print(Pre,"\n"),
	    close(Out),
	    ( Rest == [] ->
		OutStream = In
	    ;   run_filter(Rest,In,OutStream),
		close(In)
	    )
	),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
