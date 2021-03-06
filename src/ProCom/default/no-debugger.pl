%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: no-debugger.pl,v 1.3 1995/01/11 09:15:45 gerd Exp $
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

This library provides a dummy debugger for the compiled prove procedure. Even
so all interface predicates are provided they are only noops.

\Predicate Debugger/4(Literal,Candidates,Depth,Path).

This is the main predicate provided by this library.

\PL*/
:- expand_predicate('Debugger'/4).
'Debugger'(_,_,_,_).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- dynamic 'Debugger Spypoint'/1.
'Trace'.
'Notrace'.
'Spy'(_).
'Unspy'(_).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog*/
