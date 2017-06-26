%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: tptp.pl,v 1.3 1995/01/27 13:45:38 gerd Exp $
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

:- module_interface(tptp). /*%-------------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.3 $}

This module provides a filter to read formulae in the TPTP syntax. This is
usually the first filter in the filter pipe. The output produced is in
\ProTop{} syntax.

This module exports the filter predicate |tptp/2|.

\PL*/
:- export tptp/2.
:- begin_module(tptp).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
info(filter,"$Revision: 1.3 $","Filter to parse files in TPTP format.").
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following modules are required from the \ProTop{} system:

\PL*/
:-	lib(op_def),
	lib(options),
	lib(literal).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This filter uses the following option:

\PL*/
:- define_option 'tptp:home' = '/home/system/tptp/TPTP-v1.1.3/'.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The option |tptp:home| contains the absolute path to the TPTP library. This is
used to implement the |include| instruction of the TPTP file.



\Predicate tptp/2(+Stream, +OutStream).

This predicate provides the filter functionality. It reads from the stream
|Stream| and writes the result to the stream |OutStream|. This is done in a
failure driven loop until the token |end_of_file| is seen. 

\PL*/
tptp(Stream,OutStream) :-
	repeat,
	read(Stream,Term),
	( Term = end_of_file ->
	    true
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The |#| instruction is honored even so it is not part of the TPTP syntax.

\PL*/
	;   Term = (# _) ->
	    writeclause(OutStream,Term)
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Input clauses are translated using the predicate |translate/2| and written as
list of literals. The name is stored as the label. Those input clauses marked
as theorems are marked as goal clause.

\PL*/
	;   Term = input_clause(Name,theorem,Literals) ->
	    translate(Literals,Clause),
	    writeclause(OutStream,(Name:: ?- Clause)),
	    fail
	;   Term = input_clause(Name,_,Literals) ->
	    translate(Literals,Clause),
	    writeclause(OutStream,Name::Clause),
	    fail
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The |include| instruction is evaluated in the predicate |tptp_include/2|.

\PL*/
	;   Term = include(File) ->
	    tptp_include(File,OutStream),
	    fail
	),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate translate/1(+TPTP_Literals,?ProTop_Literals).

This predicate translates a list of literals |TPTP_Literals| in TPTP syntax
into a corresponding list of literals in \ProTop{} syntax. The only
translation affects the |equal/2| predicate which is translated into the |=/2|
predicate.

\PL*/
translate([],[]).
translate([++ equal(A,B)| Tail],[++(A=B)|TTail]) :-
	!,
	translate(Tail,TTail).
translate([-- equal(A,B)| Tail],[--(A=B)|TTail]) :-
	!,
	translate(Tail,TTail).
translate([Head | Tail],[Head | TTail]) :-
	translate(Tail,TTail).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate tptp_include/2(FileName,OutputStream).

This predicate implements a |include| instruction compatible to the TPTP
tools. This means the included file is searched in the directory given in the
option |tptp:home| only.

\PL*/
tptp_include(File,OutStream) :-
	is_option('tptp:home',Path),
	concat_atom([Path,File],Full),
	exists(Full),
	open(Full,read,Stream),
	tptp(Stream,OutStream),
	close(Stream),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate tptp_file_to_file/2(+Infile, +Outfile).

For testing purposes only.

\PL*/
tptp_file_to_file(Infile,Outfile) :-
	open(Infile,read,Stream),
	( Outfile = '' ->
	    tptp(Stream,output)
	;   open(Outfile,write,OutStream),
	    tptp(Stream,OutStream),
	    close(OutStream)
	),
	close(Stream).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\EndProlog */
