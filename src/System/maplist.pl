%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% $Id: maplist.pl,v 1.8 1995/01/27 13:45:38 gerd Exp $
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

:- module_interface(maplist). /*%--- Just to make pcode happy. ----------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.8 $}

\PL*/
:- export 'Maplist'/3.
:- tool(maplist/2,'Maplist'/3).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- begin_module(maplist).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate maplist/2(+Predicate, +List).

Apply |Predicate| to all elements of |List|. In general |Predicate| is
any Prolog term. The elements of |List| constitute an additional ---
last --- argument. The resulting goal is called. This predicate is
implemented as a replacement for a predicate in the Quintus library
with the same name.

\Predicate Maplist/3(+Predicate, +List, +Module).

|maplist/2| is implemented as a {\em tool}. |Maplist/3| performs the
actions necessary but automatically gets the current module as third
argument. The module is needed to call the goal in the appropriate
module.

\PL*/
'Maplist'(_,[],_).
'Maplist'(Pred,[A|List],Module) :-
	Pred =.. PL,
	append(PL,[A],NewPL),
	NewPred =.. NewPL,
	call(NewPred,Module),
	'Maplist'(Pred,List,Module).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
