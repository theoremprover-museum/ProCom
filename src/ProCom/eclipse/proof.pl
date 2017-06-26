%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: proof.pl,v 1.10 1995/04/24 21:29:11 gerd Exp $
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%% This file is part of ProCom.
%%% It is distributed under the GNU General Public License.
%%% See the file COPYING for details.
%%% 
%%% (c) Copyright 1994 Gerd Neugebauer
%%% 
%%% Net: gerd@imn.th-leipzig.de
%%% 
%%%****************************************************************************
/*\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.10 $}

 Proof presentation

\Predicate save_bindings/4().

\PL*/
:- expand_predicate(save_bindings/4).
save_bindings(Index,Clause,Bindings,File) :-
	( File == [] -> true
	;   open(File,append,Stream),
	    printf(Stream,"bindings(%Qw,(%QMDw),%QMDw).\n",
		   [Index,Clause,Bindings]),
	    close(Stream)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate save_proof/2().

\PL*/
:- expand_predicate(save_proof/2).
save_proof(Proof,File) :-
	( File == [] -> true
	;   open(File,append,Stream),
	    printf(Stream,"%QMDw.\n\n",[Proof]),
	    close(Stream)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate show_proof/1().

\PL*/
:- expand_predicate(show_proof/1).
show_proof(proof(Goal,Proof)) :-
	printf("%n%%| Proof with goal clause %w%n",[Goal]),
	show_proof_list(Proof,"").
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate show_proof_list/2().

\PL*/
show_proof_list(Proof,Level) :-
	( Proof == [] -> 
	    true
	; var(Proof) -> 
	    printf("%%|	 %s%q%n",[Level,Proof])
	; Proof = [P|Proofs] ->
	    P =.. [H|T],
	    printf("%%|	 %s%q%n",[Level,H]),
	    show_proof_term(T,Level),
	    show_proof_list(Proofs,Level)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate show_proof_term/2().

\PL*/
show_proof_term([],Level).
show_proof_term([H|T],Level) :-
	( H == [] ->
	    printf("%%|	 %s.%n",[Level])
	; (nonvar(H), functor(H,'.',2)) -> 
	    concat_atom(["|  ",Level],NewLevel),
	    show_proof_list(H,NewLevel)
	;   printf("%%|	 %s%q%n",[Level,H])
	),
	show_proof_term(T,Level).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog*/
