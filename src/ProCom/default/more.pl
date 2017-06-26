%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: more.pl,v 1.2 1995/01/11 09:15:45 gerd Exp $
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



\Predicate more/1().

To allow several solutions to be found this predicate asks if the search
should be continued.

\PL*/
:- expand_predicate(more/1).
more(Depth) :- 
	( var(Depth) -> 
	    write('Continue? ')
	;   
	    write('Current depth is '),
	    write(Depth),
	    write('Continue? ')
	),
	more_get_answer(110).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate more_get_answer/1(Character).

This predicate reads characters from standard input until the character |y|
(=121) or the character |n| (=110) is read. This predicate can not be expanded
since the cut may interfere with the control in the calling clause.

\PL*/
more_get_answer(C) :-
	repeat,
	get(A),
	(A = 121 ; A = 110),
	!,
	C = A.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog*/
