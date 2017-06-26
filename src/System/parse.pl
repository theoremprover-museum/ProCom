%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: parse.pl,v 1.22 1995/07/03 11:35:12 gerd Exp gerd $
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

:- module_interface(parse). /*%------------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.22 $}
\def\SYNTAX#1.{\begin{itemize}\item{}\(#1\) \end{itemize}}

This module provides predicates to read formulae from a file and store
them as a matrix in normal form. Basically pure Prolog syntax is used.


The following constructs are supported:
%
{\newcommand\T[1]{\mbox{\tt #1}}
\begin{eqnarray}
  &&  p \,\T{:-}\, q_1\T,\ldots\T,q_n\T.
\\&&  \,\T{:-}\, q_1\T,\ldots\T,q_n\T.
\\&&  p_1\T;\ldots\T;p_n \,\T{:-}\, q_1\T,\ldots\T,q_m\T.
\\&&  \T[q_1\T,\ldots\T,q_n\T{].}
\\&&  \T{?-} \T[q_1\T,\ldots\T,q_n\T{.]}
\\&&  \T{?-} q_1\T,\ldots\T,q_n\T.
\\&&  label \T{::} clause.
\end{eqnarray}}




Now we are ready for the implementation issues.

\PL*/
:- export read_matrix/1,
	  read_matrix_from_stream/1. 
:- begin_module(parse).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

First of all we need some externally defined predicates.

\PL*/
:-	lib(matrix),
	lib(literal),
	lib(options),
	lib(message),
	lib(find_file),
	lib(log),
	lib(time),
	lib(op_def).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- define_option input_path	  = ['Samples'].
:- define_option input_extensions = ['.mat'].
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate read_matrix/1(+File).

This predicate tries to open the file |File| and reads the information into
the Prolog data base. If |File| doen not exist or is not readable an message
is issued and the predicate fails.  When successful the predicates |Clause/3|,
|HashClause/3|, |GoalClause/2|, and |ClauseLength/2| are set appropriately.

The predicate acts verbose. I.e.\ When entering and leaving the predicate
messages are printed.

\PL*/
:- mode read_matrix(++).
read_matrix(File) :-
	is_option(input_path,Path),
	is_option(input_extensions,Extensions),
	( ( find_file(File,['.'|Path],[''|Extensions],FullName), 
	    open(FullName,read,Stream) ) ->
	    msg("% Reading matrix file ",FullName)
	;   err("*** File ",File," could not be opened."),
	    fail
	),
	read_matrix_from_stream(Stream),
	close(Stream),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate read_matrix_from_stream/1(+Stream).

This predicate reads a matrix from a stream |Stream|. In a failure driven loop
clauses are read until the end of file is found. |#| constructs are ignored
during the reading. All other clauses are given to |parse_clause/2| to be
processed. 

The time used and the number of clauses read is determined and printed at the
end.

The predicate |readvar/3| is used to read the clauses to get the original
variable names. This predicate from the \eclipse{} kernel returnes a list of
pairs of the form {\em |[|PrintName {\tt\char"7C} Variable|]|}.%"

\PL*/
read_matrix_from_stream(Stream) :-
	reset_time,
	delete_matrix,
	repeat,
	readvar(Stream,Form,Vars),
	( Form = end_of_file ->
	    true
	; Form = (# _) ->
	    fail
	;   parse_clause(Form,Vars),
	    fail
	),
	!,
	findall(x,'Clause'(_),Clauses),
	length(Clauses,NumberOfClauses),
	concat_string([NumberOfClauses," clauses read in"],Prefix),
	log_reset_time_and_print(Prefix,"\n").
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate parse_clause/2(+Formula, +Vars).

This predicate	takes  a formula  |Formula|  and an clause   index |Index| and
stores it in  the Prolog  database. Several  variants of  clauses are allowed.
They are translated into the list  representation.  |parse_clause/2| is called
to perform the store operation.

\PL*/
:- mode parse_clause(+,?).
parse_clause(Clause,_) :-
	parse_clause(Clause),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate parse_clause/1(+Formula).

This  predicate is used to map	the various variants of clause representations
into   the   internal form.  Internally a   list   of literals is   used. Thus
|parse_clause/2| mainly translates the syntactic sugar into a list of literals
and calls |add_clause/1| to perform the storing operation.  \bigskip

\SYNTAX Label |::| Clause.
%
Any clause can have arbitrary information assigned to  it. This is done in two
forms. One form is to mark a clause as goal clause with the |?-| operator. The
more general form implemented here is to write a  label in fromt of the clause
seperated by  the  |::| operator. The  label  {\em Label}\/  is stored	in the
Prolog database as |'Label'(|{\em Label}|,|{\em Index}|)|.

\PL*/
:- mode parse_clause(+).
parse_clause((Label::Clause)) :-
	!,
	parse_clause(Clause),
	add_label(Label).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\SYNTAX [L_1,\ldots,L_n].
%
The simplest --- but hard to read --- form of a clause consists of a
(nonempty) list of literals. They are stored as they are.

\PL*/
parse_clause([Head|Tail]) :-
	!,
	add_clause([Head|Tail]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\SYNTAX H_1|;|\ldots|;|H_n \,|:-|\, T_1|,|\ldots|,|T_m.
%
This is the general form of a clause in extended Prolog notation. The head
literals $H_1,\ldots H_n$ denote the negative literals. Thus they are
implicitly negated when the linternal list representation is constructed.
Nevertheless explicit negation using the |-| operator is permitted.

Other degenerate variants --- the head or the tail are emtpy ---  are provided
as well.
\PL*/
parse_clause((Head:-Tail)) :-
	!,
	add_to_clause(Tail,[],'-',TailList),
	add_to_clause(Head,TailList,'+',List),
	add_clause(List).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\SYNTAX |:-|\, T_1|,|\ldots|,|T_m.
%
This rule parses the degenerate form of the extended Prolog notation where the
head is empty. Note that in contrast to Prolog there is a difference to the
|?-| operator.
\PL*/
parse_clause((:-Tail)) :-
	!,
	add_to_clause(Tail,[],'-',List),
	add_clause(List).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\SYNTAX	 H_1|;|\ldots|;|H_n \,|<-|\, T_1|,|\ldots|,|T_m.
%
The forms incorporating the |<-| operator are simply syntactic variants of the
same forms using the |:-| operator.
\PL*/
parse_clause((Head<-Tail)) :-
	!,
	parse_clause((Head:-Tail)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\SYNTAX |<-|\, T_1|,|\ldots|,|T_m.
%
This from is simply mapped to the corresponding |:-| variant.
\PL*/
parse_clause((<-Tail)) :-
	!,
	parse_clause((:-Tail)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\SYNTAX |?-| Goal.
%


\PL*/
parse_clause((?-Goal)) :-
	!,
	(functor(Goal,'.',2) ->
	    add_clause((?-Goal))
	;   add_to_clause(Goal,[],'-',List),
	    add_clause((?-List))
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\SYNTAX L_1 |;|\ldots|;| L_n.
\SYNTAX L_1 |,|\ldots|,| L_n.
%
A disjunction is interpreted as a degenerated left hand side of an
extended Prolog clause. Thus it is negated while translated into a list.
This case acts as a catch all clause. 
\PL*/
parse_clause(Lit) :-
	add_to_clause(Lit,[],'+',List),
	add_clause(List).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate add_to_clause/4(+Junction, +List, +Mode, ?Literals).

\PL*/
%:- mode add_to_clause(+,+,++,?).
add_to_clause((A;B),List,Mode,NewList) :-
	!,
	add_to_clause(B,List,Mode,List_B),
	add_to_clause(A,List_B,Mode,NewList).
add_to_clause((A,B),List,Mode,NewList) :-
	!,
	add_to_clause(B,List,Mode,List_B),
	add_to_clause(A,List_B,Mode,NewList).
add_to_clause(Literal,List,Mode,NewList) :-
	( Mode == '+' ->
	    merge_literal_signs(Literal,NewLit)
	;   merge_literal_signs(--Literal,NewLit)
	),
	NewList = [NewLit|List].
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- skipped read_matrix/1,
	   read_matrix_from_stream/1. 
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog*/
