%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: p__iterative_widening.pl,v 1.8 1995/01/27 13:45:38 gerd Exp $
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

:- module_interface(p__iterative_widening). /*%------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.8 $}


\PL*/
:- begin_module(p__iterative_widening).

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
	is_option(search,Search),
	( Search = iterative_widening(Start,A,B) ->
	    true
	; Search = iterative_widening(Start,B) ->
	    A	  = 1
	; Search = iterative_widening(Start) ->
	    A	  = 1,
	    B	  = 1
	; Search = iterative_widening ->
	    Start = 1,
	    A	  = 1,
	    B	  = 1
	;   err("*** Wrong number or arguments: ",Search),
	    Start = 1,
	    A	  = 1,
	    B	  = 1
	),
	expand_predicate(set_depth_bound/1),
	provide_definition((set_depth_bound(Bound) :-
			set_depth_bound(Start,A,B,Bound) ),
			'$$'(iterative_widening)),
	require_predicate(set_depth_bound/4),
	is_option('ProCom:literal_selection',Sel),
	( Sel = random ->
	    add_link_files(['search_r_iw.pl'])
	;   add_link_files(['search_iw.pl'])
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\EndProlog */
