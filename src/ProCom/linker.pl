%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% $Id: linker.pl,v 1.22 1995/03/20 21:24:47 gerd Exp $
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

:- module_interface(linker). /*%-----------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.22 $}

The linker has to perform several actions to  complete a compiled program. The
main task is to provide missing predicates.  Those predicates are taken from a
library.  Thus the linker has  to know	which libraries	 are  available and in
which files they can be found.

Finally the linker  can provide	  the  optimizer with definitions  which   are
expanded in-line, i.e. unfolded.

\PL*/
:- export initialize_linker/0,
	  initialize_linker/1,
	  link_runtime_system/0,
	  add_link_path/1,
	  add_link_files/1,
	  add_link_files_from_options/1,
	  immediate_link_from_option/1,
	  require_predicate/1,
	  require_predicates_for/1,
	  expand_predicate/1,
	  provide_predicate/1,
	  provide_predicate/2,
	  provide_definition/2.
:- begin_module(linker).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The linker uses	  options, writes   out	 predicates  and interacts with	   the
optimizer.  Thus the appropriate libraries are loaded.

\PL*/
:-	lib(options),
	lib(find_file),
	lib(p_put),
	lib(optimize),
	lib(message).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:-	make_local_array(linker_error).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Certain informations are stored in the Prolog data base. Predicates for those
informations are declared dynamic.

\Predicate Link Path/1(+Directory).

This predicate	contains a single directory  which is  taken into account when
the linker searches for files.

\PL*/
:- dynamic 'Link Path'/1.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate Link File/1(+File).

This fact  is stored  in  the Prolog data base	 if the file |File|   has been
analyzed already. In this case the file has not to be analyzed any more.

\PL*/
:- dynamic 'Link File'/1.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate Provide Predicate/3(+Functor, +Arity, +File).

Facts for this predicate  indicate   that the predicate |Functor|/|Arity|   is
provided by the for the file |File|. The value of |$$|	for the file indicates
that it is provided by a program.

\PL*/
:- dynamic 'Provide Predicate'/3.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate Require Predicate/3(+Functor, +Arity, +File).

Facts  for this	 predicate  indicate that  the predicate |Functor|/|Arity|  is
provided by the for the file |File|. The value of |$$|	for the file indicates
that it is required by a program.

\PL*/
:- dynamic 'Require Predicate'/3.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate Expand Predicate/1(+Spec).

Facts for  this predicate      indicate	 that the  predicate given     by  the
specification |Spec|  can be expanded. |Spec|  is a predicate specification of
the form {\it Functor|/|Arity}.

\PL*/
:- dynamic 'Expand Predicate'/1.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate Def/3(+Head, +Body, +File).

This predicate is used to store a clause |Head| :- |Body|.


\PL*/
:- dynamic 'Def'/3.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate initialize_linker/0().

The linker is  initialized by  this predicate. This  predicate should  be used
before any compilation/link attempt to ensure a sane state.

This  predicate removes all   stored definitions, all informations on  library
files, and predicates. The  search path for libraries  is initialized from the
options |ProCom:link_path| and	|prolog|.   Consider  for example the	option
|ProCom:link_path|   with  the	 value	 |['.',	 '/usr/local/lib/ProCom']| and
|prolog| with the value |eclipse| then the  following directories are searched
for library files (in this order):

{\tt\obeylines
 .
 /usr/local/lib/ProCom
 ./eclipse
 /usr/local/lib/ProCom/eclipse
 ./default
 /usr/local/lib/ProCom/default }

Since the subdirectory |default| is searched in any case this directory can be
used to	 store	 libraries which  do  not use  features of  a  specific Prolog
dialect. The specific libraries	 should go in  the subdirectory named like the
dialect.

\PL*/
initialize_linker :-
	setval(linker_error,0),
	clear_definitions,
	is_option('ProCom:link_path',Path),
	is_option(prolog,Prolog),
	concat_atom(['/',Prolog],Dir),
	retract_all('Link Path'(_)),
	add_link_path(Path,''),
	add_link_path(Path,Dir),
	add_link_path(Path,'/default'),
	retract_all('Link File'(_)),
	retract_all('Provide Predicate'(_,_,_)),
	retract_all('Require Predicate'(_,_,_)),
	retract_all('Expand Predicate'(_)),
	retract_all('Def'(_,_,_)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate initialize_linker/1(+FileList).

This predicate is an abbreviation which initializes the linker and immediately
adds the link files given in |FileList|.

\PL*/
initialize_linker(Files) :-
	initialize_linker,
	add_link_files(Files).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate add_link_path/1().

\PL*/
add_link_path(Path) :-
	add_link_path(Path,'').
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate add_link_path/2(+PathList,+Dir).

The linker searches for libraries in certain directories. This predicate takes
a  list of initial parts  and a suffix and  constructs a  directory name. This
directory name is stored as fact in the local predicate |Link Path/1|.

Note that it  is assumed that  the directory name is  terminated by a trailing
slash (/).

\PL*/
add_link_path([],_).
add_link_path([Path|OtherPaths],Dir) :-
	concat_atoms(Path,Dir,LP),
	assert('Link Path'(LP)),
	add_link_path(OtherPaths,Dir).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate find_file/2(+ShortName, ?FullName).

This predicate searches for  an existing file.	For this purpose the predicate
|Link Path/1|  is   used  as prefix. This   path   is used only	 if   the file
|ShortName| is	 not already an absolute  reference.  Absolute	references are
characterized  by the  leading	character  |~|,	 i.e. a	  reference to a  home
directory, or the leading character |/|.

This  predicate is not resatisfiable,  i.e.  only the first  existing file  is
returned in |FullName|.

\PL*/
find_file(Name,FullName) :-
	findall(Dir, 'Link Path'(Dir), Path),
	find_file(Name,Path,['','.pl'],FullName),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate add_link_files/1(+FileList).

This predicate is   intended to make the   files  in |FileList|	 known to  the
linker. The main task is performed by |add_link_file/1| which does the job for
a single file. The remaining code simply recurses through the list structure.

\PL*/
add_link_files([]) :-
	!.
add_link_files([File|Files]) :-
	!,
	add_link_files(File),
	add_link_files(Files).
add_link_files(File) :-
	add_link_file(File).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate add_link_files_from_options/1(+OptionList).

The  same  functionality   as  in |add_link_files/1|   is   performed  with an
additional level of indirection. The argument is  a list of options containing
file names instead of the file names them self.

\PL*/
add_link_files_from_options([]) :- !.
add_link_files_from_options([Option|Options]) :-
	!,
	add_link_files_from_options(Option),
	add_link_files_from_options(Options).
add_link_files_from_options(Option) :-
	( is_option(Option,File) ->
	    add_link_file(File)
	; true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate add_link_file/1(+FileName).

A single file |FileName|  is made known to the	linker. With the {\em current}
path the library  is  searched and analyzed.   The analysis is performed in  a
failure driven loop by the predicate |analyze_library/2|. This analysis can be
omitted if the file has	 already  been analyzed. This  fact  is stored in  the
Prolog data base as facts for the predicate |Link File/1|.

\PL*/
add_link_file(Name) :-
	( find_file(Name,File) ->
	    ( 'Link File'(File) ->
		true
	    ;
		assert('Link File'(File)),
		open(File,read,Stream),
		repeat,
		read(Stream,Term),
		( Term == end_of_file ->
		    true
		;   analyze_library(Term,File),
		    fail
		),
		close(Stream),
		!
	    )
	;   puts("*** Library ",Name," could not be found."),
	    err("*** Library ",Name," could not be found."),
	    setval(linker_error,1)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate analyze_library/2(+Token, +File).

A term	 |Token| read  from a library	 file |File| is analyzed  and  certain
informations are stored	 to control the further	 actions of the linker and the
optimizer.

Pseudo instructions for the linker are evaluated at once. They	can be used to
specify	  which	  predicates  are  provided  and  required    by  the library.
Additionally (non-recursive) predicates can  be	 declared as expandable.   The
definition of  expandable   predicates has to be    stored to be   used by the
optimizer.

\PL*/
analyze_library((:- provide_predicate(Spec)),File) :-
	provide_predicate(Spec,File).
analyze_library((:- require_predicate(Spec)),File) :-
	require_predicate(Spec,File).
analyze_library((:- expand_predicate(Spec)),_) :-
	expand_predicate(Spec).
analyze_library((:- _),_).
analyze_library((?- _),_).
analyze_library((Head :- Body),File) :-
	functor(Head,F,A),
	provide_predicate(F/A,File),
	( 'Expand Predicate'(F/A) ->
	    store_definition(Head,Body,File)
	;   true
	).
analyze_library(Head,File) :-
	functor(Head,F,A),
	provide_predicate(F/A,File),
	( 'Expand Predicate'(F/A) ->
	    store_definition(Head,true,File)
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate provide_definition/2(+Clauses, +File).

\PL*/
provide_definition([],_) :- 
	!.
provide_definition([H|T],Id) :-
	!,
	provide_definition(H,Id),
	provide_definition(T,Id).
provide_definition((Head:-Body),$$(Id)) :-
	!,
	functor(Head,F,A),
	provide_predicate(F/A,$$(Id)),
	( (is_option('ProCom:expand'), 'Expand Predicate'(F/A)) ->
	    store_definition(Head,Body,-(Id))
	;   assert('Def'(Head,Body,Id))
	).
provide_definition((Head:-Body),Id) :-
	!,
	functor(Head,F,A),
	provide_predicate(F/A,Id),
	( 'Expand Predicate'(F/A) ->
	    store_definition(Head,Body,-(Id))
	;   assert('Def'(Head,Body,Id))
	).
provide_definition(Head,Id) :-
	provide_definition((Head:-true),Id).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate require_predicate/1(+Spec).

This predicate is used in the program to specify that a certain predicate or a
list of predicates are needed. |$$| indicates that the requirement is made for
the program in contrast to the requirements of a library.

\PL*/
require_predicate(Spec) :-
	require_predicate(Spec,'$$').
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate require_predicate/2(+Spec, +File).

\PL*/
require_predicate(F/A,File) :-
	assert('Require Predicate'(File,F,A)).
require_predicate([],_) .
require_predicate([Spec|Rest],File) :-
	require_predicate(Spec,File),
	require_predicate(Rest,File).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
require_predicates_for((A,B)) :-
	!,
	require_predicates_for(A),
	require_predicates_for(B).
require_predicates_for((A;B)) :-
	!,
	require_predicates_for(A),
	require_predicates_for(B).
require_predicates_for((A->B)) :-
	!,
	require_predicates_for(A),
	require_predicates_for(B).
require_predicates_for(once(A)) :-
	!,
	require_predicates_for(A).
require_predicates_for(not(A)) :-
	!,
	require_predicates_for(A).
require_predicates_for(\+(A)) :-
	!,
	require_predicates_for(A).
require_predicates_for(bagof(_,A,_)) :-
	!,
	require_predicates_for(A).
require_predicates_for(setof(_,A,_)) :-
	!,
	require_predicates_for(A).
require_predicates_for(findall(_,A,_)) :-
	!,
	require_predicates_for(A).
require_predicates_for(P) :-
	functor(P,F,A),
	( current_built_in(F/A) ->
	    true
	;   require_predicate(F/A,'$$')
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate provide_predicate/1(+Spec).

\PL*/
provide_predicate(Spec) :-
	provide_predicate(Spec,'$$').
provide_predicate([],_).
provide_predicate(F/A,File) :-
	( 'Provide Predicate'(F,A,File) ->
	    true
	;   assert('Provide Predicate'(F,A,File))
	).
provide_predicate([Spec|Rest],File) :-
	provide_predicate(Spec,File),
	provide_predicate(Rest,File).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate expand_predicate/1(+Spec).

This predicate can be used to tell the linker that the predicate given by the
specification |Spec| can be expanded. |Spec| is a predicate specification of
the form {\it Functor|/|Arity}\/ or a list of such specifications.

\PL*/
expand_predicate(F/A) :-
	( 'Expand Predicate'(F/A) ->
	    true
	;   assert('Expand Predicate'(F/A))
	).
expand_predicate([]).
expand_predicate([Spec|Rest]) :-
	expand_predicate(Spec),
	expand_predicate(Rest).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate determine_files/3(+Functors, +FilesIn, -FilesOut).

This predicate tries to find a minimal set of files which provide the required
predicates. |Functors| is a list of predicate  specifications of the form {\it
  Functor|/|Arity}\/ which have to be  found.  |FilesIn| is  the list of files
already marked to be linked. |FilesOut| are the additional files found by this
predicate.

If a predicate could not be found an error message is displayed.

\PL*/
determine_files([],Files,Files).
determine_files([F/A|Preds],FilesIn,FilesOut) :-
	'Provide Predicate'(F,A,File),
	!,
	( \+ \+ member(File,FilesIn) ->
	    NewPreds = Preds,
	    Files = FilesIn
	; setof(F1/A1,'Require Predicate'(File,F1,A1),P) ->
	    append(Preds,P,NewPreds),
	    Files = [File|FilesIn]
	;   NewPreds = Preds,
	    Files = [File|FilesIn]
	),
	determine_files(NewPreds,Files,FilesOut).
determine_files([Spec|Preds],Files,FilesOut) :-
	puts("*** Predicate ",Spec," could not be found."),
	err("*** Predicate ",Spec," could not be found."),
	setval(linker_error,1),
	determine_files(Preds,Files,FilesOut).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate link_runtime_system/0().

This predicate performs	 all actions gathered so  far in the Prolog data base.
It determines	which files need  to  be linked and  calls  the	 static or the
dynamic linker according to the option |ProCom:link|.

\PL*/
link_runtime_system :-
	(   'Def'(H,T,_) ,
	    put_clause((H:-T)),
	    fail
	;   true
	),
	( setof(F/A,'Require Predicate'('$$',F,A),Preds) ->
	    determine_files(Preds,['$$'(_)],Files),
	    ( is_option('ProCom:link' = static) ->
		static_linker(Files)
	    ;	
		put_clause(provide_predicate(_)), 
		put_clause(require_predicate(_)),
		put_clause(expand_predicate(_)),
		dynamic_linker(Files)
	    )
	;   true
	),
	!,
	is_option('ProCom:ignore_link_errors',Ignore),
	( empty_option(Ignore) ->
	    getval(linker_error,0)
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate dynamic_linker/1(+FileList).

The dynamic linking strategy  just leaves a reference  in the generated Prolog
code where to find the required libraries. This	 strategy has the advantage of
using less space for the program and the possibility  to improve the libraries
without recompilation/linking. The disadvantage is obvious. The libraries have
to be present in the place where the linker found them. The code can be run on
a different host only if this severe restriction is fulfilled.

This predicate takes all  library specifications in   the list |FileList|  and
generates appropriate code to load the associated libraries.

\PL*/
dynamic_linker([]).
dynamic_linker([File|Files]) :-
	( File == '$$' ->
	    true
	; File = '$$'(_) ->
	    true
	;   put_nl,
	    ( find_file(File,F) ->
		put_report('ProCom:verbose',"--- LIBRARY ",F),
		put_clause((:- [F]))
	    ;	puts("*** Library ",File," could not be found."),
		err("*** Library ",File," could not be found."),
		setval(linker_error,1)
	    )
	),
	dynamic_linker(Files).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate static_linker/1(+FileList).

The  static  linking strategy  tries to	 cope  with the	 disadvantages	of the
dynamic linking strategy. Thus relocatable code is generated by copying in the
needed libraries.  The	disadvantage is the  space needed,  but	 this is maybe
negligible when considering the size of the modules currently used.

The files given in the list |FileList| are searched on	the library path using
the predicate |find_file/2|. In a failure driven loop the relevant part of the
contents is written to the output stream by the predicate |static_link/1|.

\PL*/
static_linker([]).
static_linker([File|OtherFiles]) :-
	( File == '$$' ->
	    true
	; File = '$$'(_) ->
	    true
	;   put_nl,
	    ( find_file(File,F) ->
		put_report('ProCom:verbose',"--- LIBRARY ",F),
		open(F,read,Stream),
		repeat,
		read(Stream,Term),
		( Term == end_of_file ->
		    true
		;   static_link(Term),
		    fail
		),
		close(Stream),
		!
	    ;	puts("*** Library ",File," could not be found."),
		err("*** Library ",File," could not be found."),
		setval(linker_error,1)
	    )
	),
	static_linker(OtherFiles).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate static_link/1(+Term).

This predicate	takes the term	 read from a  library	file and performs  the
appropriate action to  write out some code.   Certain instructions are	simply
ignored. Thus we don't have to include them in	the output. Those instructions
are used to control the linking process.

Another	 thing which has to  be taken into account is	the fact that expanded
predicates do not  occur in the generated  Prolog code any  more.  Thus we can
omit them as well.

\PL*/
static_link((:- provide_predicate(_))) :- !.
static_link((:- require_predicate(_))) :- !.
static_link((:- expand_predicate(_)))  :- !.
static_link((:- true))		       :- !.
static_link((:- Body)) :- 
	!,
	put_clause((:- Body)).
static_link((Head :- Body)) :-
	!,
	functor(Head,F,A),
	( ( 'Expand Predicate'(F/A),
	    is_option('ProCom:expand') ) ->
	    true
	;   put_clause((Head :- Body))
	).
static_link(Head) :-
	functor(Head,F,A),
	( ( 'Expand Predicate'(F/A),
	    is_option('ProCom:expand') ) ->
	    true
	;   put_clause(Head)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate immediate_link_from_option/1(+OptionList).

This predicate instructs  the	linker to perform  it's	 actions  immediately.
Usually the linker keeps  it's instructions in the Prolog  data base  and acts
only when |link_runtime_system/0| is  called.  This predicate  can be  used to
statically  link  the	libraries specified    in  the	options	 |OptionsList|
immediately.

\PL*/
immediate_link_from_option([Option|Options]) :-
	immediate_link_from_option(Option),
	immediate_link_from_option(Options).
immediate_link_from_option(Option):-
	( Option == [] ->
	    true
	; is_option(Option,File) ->
	    ( File = [] ->
		true
	    ; functor(File,'.',2) ->
		static_linker(File)
	    ;	static_linker([File])
	    )
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
