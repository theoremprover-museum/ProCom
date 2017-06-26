%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: member.pl,v 1.3 1995/01/11 09:15:45 gerd Exp $
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
/*\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.3 $}

Reduction needs a sound implementation of the member predicate.

\Predicate sound_member/2(Element,List).

This implementation uses unify/2 declared in the library |unify.pl|.

\PL*/
sound_member(Element,[Head|Tail]) :-
	(   unify(Element,Head)
	;   sound_member(Element,Tail)
	).
:- require_predicate(unify/2).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog*/
