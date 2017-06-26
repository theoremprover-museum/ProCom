%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: proof.pl,v 1.2 1994/12/12 17:07:16 gerd Exp $
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
/*\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.2 $}

 Proof presentation

\Predicate save_bindings/4().

\PL*/
:- expand_predicate(save_bindings/4).
save_bindings(Index,Clause,Bindings,File).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate save_proof/2().

\PL*/
:- expand_predicate(save_proof/2).
save_proof(Proof,File).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate show_proof/1().

\PL*/
:- expand_predicate(show_proof/1).
show_proof(proof(Goal,Proof)) :-
	nl,
	write('%| Proof with goal clause '),
	write(Goal),
	nl,
        show_proof_list(Proof,"").
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate show_proof_list/2().

\PL*/
show_proof_list(Proof,Level) :-
	( Proof == [] -> 
	    true
	; var(Proof) ->
	    write('%|  '),
	    write(Level),
	    write(Proof),
	    nl
	; Proof = [P|Proofs] ->
	    P =.. [H|T],
	    write('%|  '),
	    write(Level),
	    write(H),
	    nl,
            show_proof_term(T,Level),
            show_proof_list(Proofs,Level)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate show_proof_term/2().

\PL*/
show_proof_term([],Level).
show_proof_term([H|T],Level) :-
	( H == [] ->
	    write('%|  '),
	    write(Level),
	    write('.'),
	    nl
	; (nonvar(H), functor(H,'.',2)) -> 
	    name(Level,L_Level),
	    name(NewLevel,[124,32,32|L_Level]),
	    show_proof_list(H,NewLevel)
	;   
	    write('%|  '),
	    write(Level),
	    write(H),
	    nl
        ),
	show_proof_term(T,Level).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog*/
