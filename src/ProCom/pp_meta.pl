%%*****************************************************************************
%% $Id: pp_meta.pl,v 1.2 1995/01/11 09:15:45 gerd Exp $
%%*****************************************************************************
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

:- module(pp_meta). /*%--- -------------------------------------

\PL*/

:- global pp/2.

:-	lib(strings).

pp_meta(Clause,Stream) :-
	copy_term(Clause,CleanClause),
	term_variables(Clause,Vars),
	term_variables(CleanClause,PureVars),
	( CleanClause = (H:-T) ->
	    printf(Stream,"%VQDMw :-",[H]),
	    pp_meta_goals(Vars,PureVars,T,"",Stream)
	;   printf(Stream,"%VQDMw",[CleanClause]),
	    pp_meta_goals(Vars,PureVars,true," :-",Stream)
	).

pp_meta_goals([],[],G,Prefix,Stream) :-
	( G == true ->
	    true
	;   printf(Stream,"%w",[Prefix]),
	    pp_meta_body(G,"\t",Stream)
	),
	printf(Stream,".\n",[]).
pp_meta_goals([H|T],[PH|PT],G,Prefix,Stream) :-
	( meta(H) ->
	    printf(Stream,"%w\n\t%VQDMw",[Prefix,(PH=H)]),
	    pp_meta_goals(T,PT,G,",",Stream)
	;   pp_meta_goals(T,PT,G,Prefix,Stream)
	).

pp_meta_body((A,B),In,Stream) :-
	!,
	pp_meta_body(A,In,Stream),
	printf(Stream,",",[]),
	pp_meta_body(B,In,Stream).
pp_meta_body((A->B;C),In,Stream) :-
	!,
	printf(Stream,"\n%w( ( ",[In]),
	concat_strings(In,"    ",In2),
	pp_meta_body(A,In2,Stream),
	printf(Stream," ) ->",[In]),
	pp_meta_body(B,In2,Stream),
	printf(Stream,"\n%w;",[In]),
	pp_meta_body(C,In2,Stream),
	printf(Stream,"\n%w)",[In]).
pp_meta_body((A->B),In,Stream) :-
	!,
	printf(Stream,"\n%w( ( ",[In]),
	concat_strings(In,"    ",In2),
	pp_meta_body(A,In2,Stream),
	printf(Stream," ) ->",[In]),
	pp_meta_body(B,In2,Stream),
	printf(Stream,"\n%w)",[In]).
pp_meta_body((A;B),In,Stream) :-
	!,
	printf(Stream,"\n%w(",[In]),
	concat_strings(In,"    ",In2),
	pp_meta_body(A,In2,Stream),
	printf(Stream,"\n%w;",[In]),
	pp_meta_body(B,In2,Stream),
	printf(Stream,"\n%w)",[In]).
pp_meta_body(A,In,Stream) :-
	!,
	printf(Stream,"\n%w%VQDMw",[In,A]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\EndProlog */
