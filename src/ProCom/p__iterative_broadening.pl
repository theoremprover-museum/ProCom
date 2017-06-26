%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: p__iterative_broadening.pl,v 1.8 1995/01/27 13:45:38 gerd Exp $
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

:- module_interface(p__iterative_broadening). /*%------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.8 $}


\PL*/
:- begin_module(p__iterative_broadening).

:-	lib(options),
	lib(linker),
	lib(message).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate init_search/0().

\PL*/
init_search :-
	setval('ProCom:add_depth_argument',on),
	setval('ProCom:depth_pass_through',off),
	setval('ProCom:need_weight',on),
	is_option(search,iterative_broadening(Start,A,B)),
	expand_predicate(set_depth_bound/1),
	provide_definition((set_depth_bound(Bound) :-
			set_depth_bound(Start*1,A,B,Bound) ),
			'$$'(iterative_broadening)),
	require_predicate(set_depth_bound/4),
	is_option('ProCom:literal_selection',Sel),
	( Sel = random ->
	    add_link_files(['search_r_ib.pl'])
	;   add_link_files(['search_ib.pl'])
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
