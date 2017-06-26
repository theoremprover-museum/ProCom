/******************************************************************************
%% $Id: main.pl,v 1.40 1995/02/06 08:57:39 gerd Exp $
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

\section{The Top Level Routines}
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.40 $}

The main purpose of this  file is to  load the whole system. Before  \ProTop{}
has been created  this has  been the place   to add predicates	for the Prolog
interface. Anyone still working	 with the Prolog interface  can take this file
as an starting point.

The main task performed now is to load the prover toplevel \ProTop{} and
provide the exported predicates in the current module.

\PL*/
:-	ensure_loaded('ProTop/protop.pl'),
	ensure_loaded('Prepare/complete_goals.pl'),
	ensure_loaded('Prepare/connection_graph.pl'),
	use_module(protop).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
