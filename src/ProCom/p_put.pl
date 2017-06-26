%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: p_put.pl,v 1.10 1995/02/06 08:57:39 gerd Exp $
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

:- module_interface(p_put). /*
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.10 $}

This module contains various output routines. It provides a uniform output
interface. Some of the predicates are simply mapped to corresponding build-ins
or library predicates.

\PL*/
:- export put_open/1,
	  put_close/0,
	  put_nl/0,
	  puts/1,
	  puts/2,
	  puts/3,
	  put_report/2,
	  put_report/3,
	  put_report/4,
	  put_report/5,
	  put_boxed/2,
	  put_matrix/0,
	  put_clause/1,
	  put_clause/2. 
:- begin_module(p_put).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This module is based on some other modules.

\PL*/
:-	lib(strings),
	lib(numbervars),
	lib(message),
	lib(options),
	lib(matrix).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

One internal global variable is used. Thus it is possible to avoid passing
around a output stream argument.

\PL*/
:- setval('put:stream',output).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate put_open/1(+Filename).

This predicate opens a file for subsequent writing operations by the {\em
  put\_} predicates. Empty symbol, empty string and empty list are interpreted
as noop requests, i.e. no operation is performed if those arguments are
encountered.

The stream is saved in the global variable |put:stream| for further use.

\PL*/
put_open('') :- !.
put_open("") :- !.
put_open([]) :- !.
put_open(File) :-
	open(File,write,Stream),
	setval('put:stream',Stream).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate put_close/0().

This predicate is the dual to |put_open/1|. All actions required to clean up
things are performed. The stream is closed --- if it is not the normal output
stream! The global variable is reset. Any output requests through this module
are written to the default output stream subsequently. Thus the semantics of
|tell/1| and |told/0| are adopted.

\PL*/
put_close :-
	getval('put:stream',Stream),
	( Stream = output ->
	    true
	;   close(Stream)
	),
	setval('put:stream',output).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate put_matrix/0().

This predicate writes the current matrix to the output stream. The main work
is performed in the module {\sf matrix}. Here we have to provide a stream and
a prefix token only.

\PL*/
put_matrix :-
	getval('put:stream',Stream),
	show_matrix('%',Stream).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate put_nl/0().

Just produce a newline on the put:stream.

\PL*/
put_nl :- 
	getval('put:stream',Stream),
	nl(Stream).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate puts/3(+Term1,+Term2,+Term3).

Write three terms onto the |put:stream|. The arguments are separated by spaces
and started by a |%|.

\PL*/
puts(S1,S2,S3) :- 
	puts([S1,S2,S3]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate puts/2(+Term1,+Term2).

Write two terms onto the put:stream. The arguments are separated
by spaces and started by a |%|.

\PL*/
puts(S1,S2) :- 
	puts([S1,S2]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate puts/1(+Term).

Write a term onto the put:stream. If the term is a list then the elements are
written separated by spaces and started by a |%|. Otherwise |write\1| is used
for writing.

\PL*/
puts(Arg) :-
	getval('put:stream',Stream),
	write(Stream,'% '),
	'Put List'(Arg,Stream),
	nl(Stream).
'Put List'(A,Stream) :-
	( A = []    -> true
	; A = [$|T] ->
	    nl(Stream),
	    write(Stream,'% '),
	    'Put List'(T,Stream)
	; A = [H|T] ->
	    write(Stream,H),
	    write(Stream,' '),
	    'Put List'(T,Stream)
	; A = $ ->
	    nl(Stream)
	;   write(Stream,A)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate put_report/2(+Option, +String).

\PL*/
put_report(Option,String1) :-
	( is_option(Option) ->
	    getval('put:stream',Stream),
	    printf(Stream,'%%%w\n',[String1]),
	    msg("%",String1)
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate put_report/3(+Option, +String1, +String2).

\PL*/
put_report(Option,String1,String2) :-
	( is_option(Option) ->
	    getval('put:stream',Stream),
	    printf(Stream,'%%%w%w%n',[String1,String2]),
	    msg("%",String1,String2)
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate put_report/4(+Option, +String1, +String2, +String3).

\PL*/
put_report(Option,String1,String2,String3) :-
	( is_option(Option) ->
	    getval('put:stream',Stream),
	    printf(Stream,'%%%w%w%w%n',[String1,String2,String3]),
	    msg("%",String1,String2,String3)
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate put_report/5(+Option, +String1, +String2, +String3, +String4).

\PL*/
put_report(Option,String1,String2,String3,String4) :-
	( is_option(Option) ->
	    getval('put:stream',Stream),
	    printf(Stream,"%%%w%w%w%w%n",[String1,String2,String3,String4]),
	    msg("%",String1,String2,String3,String4)
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate put_boxed/2(+Option, +String).

\batchmode % TeX chokes on special characters, so I disable warnings.
\PL*/
put_boxed(Option,String) :-
	( is_option(Option) ->
	    getval('put:stream',Stream),
	    printf(Stream,"\n\n%%%78c\n%%---  %w\n%%%78c\n\n",[45,String,45])
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\errorstopmode

\Predicate put_clause/1(+Clause).

Write a clause to the put:stream. No simplifications are appied.

\PL*/
:- mode put_clause(+).
put_clause(Clause) :-
	getval('put:stream',Stream),
	( is_option('ProCom:put_pretty') ->
	    setval('p_put:no',0),
	    put_clause_to_stream(Clause,Stream)
	;   printf(Stream,"%vQDMw.%n",[Clause])
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate put_clause2/(+Clause, +Vars).

\PL*/
:- mode put_clause(+,+).
put_clause(Clause,Vars) :-
	apply_vars(Vars),
	getval('put:stream',Stream),
	put_clause_to_stream(Clause,Stream).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate apply_vars/1(VarList).

\PL*/
apply_vars([]).
apply_vars([[A|A]|Rest]) :-
	apply_vars(Rest).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate put_clause_to_stream/2(+Clause, +Stream).

\PL*/
:- mode put_clause_to_stream(+,+).
put_clause_to_stream((?- Goal),Stream) :- 
	!,
	printf(Stream,"?-\t",[]),
	put_body_to_stream(Goal,8,Stream,[],_),
	printf(Stream,".\n",[]).
put_clause_to_stream((:- Goal),Stream) :- 
	!,
	printf(Stream,":-\t",[]),
	put_body_to_stream(Goal,8,Stream,[],_),
	printf(Stream,".\n",[]).
put_clause_to_stream((Head :- Body),Stream) :-
	!,
	get_meta_goals(Head,[],Vars,Meta),
	printf(Stream,"%QVDw :-\n",[Head]),
	indent_to(8,Stream),
	( Meta == true ->
	    Goals = Body
	;   Goals = (Meta,Body)
	),
	put_body_to_stream(Goals,8,Stream,Vars,_),
	printf(Stream,".\n",[]).
put_clause_to_stream(Head,Stream) :-
	get_meta_goals(Head,[],Vars,Meta),
	( Meta == true ->
	    printf(Stream,"%QVDw.\n",[Head])
	;   printf(Stream,"%QVDw :-",[Head]),
	    put_body_to_stream(Meta,8,Stream,Vars,_),
	    printf(Stream,".\n",[])
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate put_body_to_stream/5(+Body, +Indent, +Stream).

\PL*/
:- mode put_body_to_stream(+,+,+,?,?).
put_body_to_stream((A,B),In,Stream,Omit,NewOmit) :-
	!,
	put_body_to_stream(A,In,Stream,Omit,Omit2),
	writeln(Stream,','),
	indent_to(In,Stream),
	put_body_to_stream(B,In,Stream,Omit2,NewOmit).
put_body_to_stream((A;B),In,Stream,Omit,NewOmit) :-
	!,
	printf(Stream,"(   ",[]),
	N is In + 4,
	put_d_i_on_stream(A,N,Stream,Omit,Omit2),
	nl(Stream),
	indent_to(In,Stream),
	write(Stream,';	  '),
	put_disjunction_to_stream(B,N,Stream,Omit,Omit3),
	nl(Stream),
	indent_to(In,Stream),
	write(Stream,')'),
	append(Omit2,Omit3,NewOmit).
put_body_to_stream(Literal,In,Stream,Omit,NewOmit) :-
	get_meta_goals(Literal,Omit,Vars,Meta),
	( Meta == true ->
	    printf(Stream,"%QVDw",[Literal]),
	    NewOmit = Omit
	;   append(Vars,Omit,NewOmit),
	    put_body_to_stream((Meta,Literal),In,Stream,NewOmit,_)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate put_d_i_on_stream/3(+Term, +Indent, +Stream).

\PL*/
:- mode put_d_i_on_stream(+,+,+).
put_d_i_on_stream((A->B),In,Stream,Omit,NewOmit) :-
	!,
	put_body_to_stream(A,In,Stream,Omit,Omit2),
	writeln(Stream,' ->'),
	indent_to(In,Stream),
	put_body_to_stream(B,In,Stream,Omit2,NewOmit).
put_d_i_on_stream(X,In,Stream,Omit,NewOmit) :-
	put_body_to_stream(X,In,Stream,Omit,NewOmit).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate put_disjunction_to_stream/3(+Term, +Indent, +Stream).

\PL*/
:- mode put_disjunction_to_stream(+,+,+).
put_disjunction_to_stream((A;B),In,Stream,Omit,NewOmit) :-
	!,
	put_d_i_on_stream(A,In,Stream,Omit,Omit2),
	In4 is In - 4,
	nl(Stream),
	indent_to(In4,Stream),
	write(Stream,';	  '),
	put_disjunction_to_stream(B,In,Stream,Omit,Omit3),
	append(Omit2,Omit3,NewOmit).
put_disjunction_to_stream(A,In,Stream,Omit,NewOmit) :-
	put_body_to_stream(A,In,Stream,Omit,NewOmit).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate indent_to/2(+Level, +Stream).

\PL*/
:- mode indent_to(++,++).
indent_to(0,_).
indent_to(1,Stream) :- write(Stream," ").
indent_to(2,Stream) :- write(Stream,"  ").
indent_to(3,Stream) :- write(Stream,"   ").
indent_to(4,Stream) :- write(Stream,"    ").
indent_to(5,Stream) :- write(Stream,"  	  ").
indent_to(6,Stream) :- write(Stream," 	   ").
indent_to(7,Stream) :- write(Stream,"       ").
indent_to(N,Stream) :- N >= 8,
	printf(Stream,"\t",[]),
	N1 is N - 8,
	indent_to(N1,Stream).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^




\Predicate get_meta_goals/4().

\PL*/
get_meta_goals(Term,Omit,Vars,Goals) :-
	term_variables(Term,V),
	get_meta_vars_from_list(V,Omit,Vars),
	get_meta_goals(Vars,Goals).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate get_meta_goals/2().

\PL*/
get_meta_goals([],true).
get_meta_goals([H],add_constraints(H,Attr)) :-
	!,
	get_attribute(H,Attr).
get_meta_goals([H|T],(add_constraints(H,Attr),R)) :-
	!,
	get_attribute(H,Attr),
	get_meta_goals(T,R).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate get_meta_vars/3().

\PL*/
get_meta_vars(Term,Omit,Vars) :-
	term_variables(Term,V),
	get_meta_vars_from_list(V,Omit,Vars).
get_meta_vars_from_list([],_,[]).
get_meta_vars_from_list([H|T],Omit,Vars) :-
	(   meta(H) ->
	    ( \+ ( member(X,Omit), X == H ) ->
		Vars = [H|VT]
	    ;	Vars = VT
	    )
	;   Vars = VT,
	    getval('p_put:no',No),
	    incval('p_put:no'),
	    H =	 '$VAR'(No)
	),
	get_meta_vars_from_list(T,Omit,VT).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate get_attribute/2(Metaterm, Attribute).

\PL*/
get_attribute(_{constraints:Attr},A) :-
	-?->
	A = Attr.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
