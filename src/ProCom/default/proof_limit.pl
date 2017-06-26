%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: proof_limit.pl,v 1.1 1994/12/19 22:03:38 gerd Exp $
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
/*\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.1 $}

\Predicate init_proof_limit/0().

\PL*/
:- dynamic proof_limit/1.
:- expand_predicate(init_proof_limit/0).
init_proof_limit :-
	retractall(proof_limit(_)),
	assert(proof_limit(0)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate check_proof_limit/1().

\PL*/
:- expand_predicate(check_proof_limit/1).
check_proof_limit(Limit) :-
	retract(proof_limit(N)),
	NO is N + 1,
	NO >= Limit,
	assert(proof_limit(NO)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate get_proof_limit/1().

\PL*/
:- expand_predicate(get_proof_limit/1).
get_proof_limit(Limit) :-
	proof_limit(Limit).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog*/
