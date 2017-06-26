%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: p__depth_first.pl,v 1.6 1995/01/27 13:45:38 gerd Exp $
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

:- module_interface(p__depth_first). /*%------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.6 $}

This module provides a predicate to initialize the search strategy to use
depth first search.

\PL*/
:- begin_module(p__depth_first).

:-	lib(options),
	lib(linker),
	lib(message).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate init_search/0().

\PL*/
init_search :-
	setval('ProCom:add_depth_argument',off),
	setval('ProCom:depth_pass_through',off),
	setval('ProCom:need_weight',off),
	is_option(search,Search),
	( Search = depth_first ->
	    true
	;   err("*** Wrong number or arguments: ",Search)
	),
	is_option('ProCom:literal_selection',Sel),
	( Sel = random ->
	    add_link_files(['search_r_df.pl'])
	;   add_link_files(['search_df.pl'])
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
