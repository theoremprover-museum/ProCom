%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: proof.pl,v 1.5 1995/02/13 20:05:14 gerd Exp $
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

:- module_interface(proof). /*%-------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.5 $}

This is essentially the library {\em ProCom/eclipse/proof.pl}. It is meant for
those provers which do not create stand alone executables.


\PL*/
:- export show_proof/2,
	  save_bindings/4,
	  save_proof/2.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- begin_module(proof).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate save_bindings/4(+Index, +Clause, +Bindings, +File).

This predicate append the information about a solution found to the report
file |File|. The informations stored are the clause index |Index|, the clause
(as list of literals with indices) |Clause| and the variable bindings
|Bindings| as a list of terms {\it Variable |=| Value}.

\PL*/
save_bindings(Index,Clause,Bindings,File) :-
	( (File == []; \+ atom(File)) ->
	    true
	;   open(File,append,Stream),
	    printf(Stream,"bindings(%Qw,(%QMDw),%QMDw).\n",
		   [Index,Clause,Bindings]),
	    close(Stream)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate save_proof/2(+Proof, +File).

This predicate appends the given proof term |Proof| to the file |File|. If
file is not a proper file name then nothing is done. The predicate succeeds
always and leaves no choice point.

\PL*/
save_proof(Proof,File) :-
	( (File == []; \+ atom(File)) ->
	    true
	;   open(File,append,Stream),
	    printf(Stream,"%QMDw.\n\n",[Proof]),
	    close(Stream)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate show_proof/2(+Stream, +Proof).

This predicate pretty prints a proof term |Proof| onto the given stream
|Stream|. 

\PL*/
show_proof(Stream,proof(Goal,Proof)) :-
	printf(Stream,"%n%%| Proof with goal clause %w%n",[Goal]),
	show_proof_list(Stream,Proof,"").
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate show_proof_list/3(+Stream, +Proof, +Level).

This predicate pretty prints a proof term |Proof| onto the given stream
|Stream|.  |Level| is the prefix denoting the level of indentation.

\PL*/
show_proof_list(Stream,Proof,Level) :-
	( Proof == [] -> 
	    true
	; var(Proof) -> 
	    printf(Stream,"%%|	%s%q%n",[Level,Proof])
	; Proof = [P|Proofs] ->
	    P =.. [H|T],
	    printf(Stream,"%%|	%s%q%n",[Level,H]),
	    show_proof_term(Stream,T,Level),
	    show_proof_list(Stream,Proofs,Level)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate show_proof_term/3(+Stream, +List, +Level).

\PL*/
show_proof_term(_,[],_).
show_proof_term(Stream,[H|T],Level) :-
	( H == [] ->
	    printf(Stream,"%%|	%s.%n",[Level])
	; (nonvar(H), functor(H,'.',2)) -> 
	    concat_atom(["|  ",Level],NewLevel),
	    show_proof_list(Stream,H,NewLevel)
	;   printf(Stream,"%%|	%s%q%n",[Level,H])
	),
	show_proof_term(Stream,T,Level).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- skipped show_proof/2,
	   save_bindings/4,
	   save_proof/2.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
