%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: report_summary.pl,v 1.5 1995/05/01 19:47:03 gerd Exp $
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

:- module_interface(report_summary). /*%---------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.5 $}


\PL*/
:- export summary_clear/0,
          summary_store/3,
          make_summary_table/2.
:- begin_module(report_summary).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- dynamic info_db/3,
           info_label/1.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate summary_clear/0().

This predicate removes all information stroed in the summary data base.

\PL*/
summary_clear :-
        retract_all(info_db(_,_,_)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate summary_store/3(Label,Type,Value).

\PL*/
summary_store(Label,Type,Value) :-
        assert(info_db(Label,Type,Value)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate summary_assemble_format/2().

\PL*/
summary_assemble_format([],["|"]).
summary_assemble_format([l|T],["|l"|FT]) :- summary_assemble_format(T,FT).
summary_assemble_format([r|T],["|r"|FT]) :- summary_assemble_format(T,FT).
summary_assemble_format([c|T],["|c"|FT]) :- summary_assemble_format(T,FT).
summary_assemble_format([p(W)|T],[P|FT]) :-
        concat_string(["|p{",W,"}"],P),
        summary_assemble_format(T,FT).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate summary_guess_format/2().

\PL*/
summary_guess_format([],["|"]).
summary_guess_format([time(_,_)|T],["|r"|FT])  :- summary_guess_format(T,FT).
summary_guess_format([label(_)|T],["|l"|FT])   :- summary_guess_format(T,FT).
summary_guess_format([section(_)|T],["|l"|FT]) :- summary_guess_format(T,FT).
summary_guess_format([@(_)|T],["|l"|FT])       :- summary_guess_format(T,FT).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate summary_format/3().

\PL*/
summary_format(Layout,Format,Width) :-
        member(Row,Layout),
        functor(Row,row,Width),
        !,
        Row =.. [row|R],
        ( member(F,Layout),
          F =.. [format|Args] ->
            summary_assemble_format(Args,Fmt),
            ( Width =:= length(Args) ->
                true
            ;   writeln("*** ERROR: Inconsistent length."),
                fail
            )
        ;   summary_guess_format(R,Fmt)
        ),
        concat_atom(Fmt,Format).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate make_summary_table/2().

\PL*/
make_summary_table(Layout,Stream) :-
        retract_all(info_label(_)),
        (   info_db(Label,_,_),
            \+ info_label(Label),
            assert(info_label(Label)),
            fail
        ;   true
        ),
        summary_format(Layout,FMT,Width),
        printf(Stream,
	       "\n\\begin{Summary}{%w}\n\\begin{tabular}{%w}\\hline\n",
	       [Width,FMT]),
        (   member(head(Head),Layout),
            printf(Stream,
		   "  \\multicolumn{%w}{|c|}{\\PRSummaryHeadFont %w}\\\\\\hline\n",
		   [Width,Head]),
            fail
        ;   true
        ),
        (   member(subhead(Head),Layout),
            printf(Stream,
		   "  \\multicolumn{%w}{|c|}{\\PRSummarySubHeadFont %w}\\\\\\hline\n",
		   [Width,Head]),
            fail
        ;   true
        ),
        (   member(Titles,Layout),
            functor(Titles,titles,_),
            Titles =.. [titles,T0|Args],
	    typeset_one_title(T0,Stream),
            (   member(T,Args),
		printf(Stream," & ",[]),
                typeset_one_title(T,Stream)
            ;   printf(Stream,"\\\\\n",[])
            ),
            fail
        ;   true
        ),
        member(Row,Layout),
        Row =.. [row|RowSpec],
        (   member(X,Layout),
            do(X,RowSpec,Stream),
            fail
        ;   true
        ),
        printf(Stream,"  \\hline\n\\end{tabular}\\end{Summary}\n",[]),
        !.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate typeset_one_title/2().

\PL*/
typeset_one_title(T,Stream) :-
        string(T),
        !,
        printf(Stream,"{%w}",[T]).
typeset_one_title(T,Stream) :-
        atom(T),
        !,
        printf(Stream,"{%w}",[T]).
typeset_one_title(multi(N,T),Stream) :-
        integer(N),
        ( string(T) 
        ;   atom(T)
        ),
        !,
        printf(Stream,"[%w]{%w}",[N,T]).
typeset_one_title(_,Stream) :-
        printf(Stream,"{}",[]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate typeset_one_row/2().

\PL*/
typeset_one_row([],_).
typeset_one_row([H],Stream) :-
	!,
        write(Stream,"{"),
        ( typeset_one_row_item(H,Stream) -> true; true ),
        write(Stream,"}\\\\").
typeset_one_row([H|T],Stream) :-
        write(Stream,"{"),
        ( typeset_one_row_item(H,Stream) -> true; true ),
        write(Stream,"}&"),
        typeset_one_row(T,Stream).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate typeset_one_row_item/2().

\PL*/
typeset_one_row_item(label(Label),Stream) :- 
        printf(Stream,"\\Label{%w}",[Label]).
typeset_one_row_item(time(Type,Label),Stream) :-
        ( info_db(Label,time(T),Value),
          match_time(T,Type) ->
            printf(Stream,"%w{%w}",["\\Time",Value])
        ;   write(Stream,"\\NoTime")
        ),
        !.
typeset_one_row_item(section(Label),Stream) :-
        typeset_one_row_item_generic(Label,section,"\\Section","",Stream).
typeset_one_row_item(sum(Spec),Stream) :-
        do_sum(Spec,Sum),
        write(Stream,Sum).
typeset_one_row_item_generic(Label,Type,Macro,Default,Stream) :-
        ( info_db(Label,Type,Value) ->
            printf(Stream,"%w{%w}",[Macro,Value])
        ;   printf(Stream,"%w",[Default])
        ),
        !.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate do_sum/2().

\PL*/
do_sum(Spec,Sum) :-
        do_sum(Spec,S,U,I),
        concat_string(["\\No{",S,"}{",U,"}{",I,"}"],Sum).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate do_sum/4(+List, ?Sum, ?Sum_Undef, ?Sum_Infty).

\PL*/
do_sum([],0,0,0).
do_sum([H|T],Sum,Undef,Infty) :-
        do_sum(H,S1,U1,I1),
        do_sum(T,S2,U2,I2),
        Sum   is S1+S2,
        Undef is U1+U2,
        Infty is I1+I2.
do_sum(N,N,0,0) :-
        number(N).
%do_sum(time(Type,Label),S,U,I).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate match_time/2(String,Symbol).

\PL*/
match_time(S,T)            :- atom(S), !, S=T.
match_time(S,T)            :- string(S), string(T), !, substring(S,T,_).
match_time(S,compile_time) :- string(S), !, substring(S,"ompile time",_).
match_time(S,completion)   :- string(S), !, substring(S,"oal completion",_).
match_time(S,connections)  :- string(S), !, substring(S,"onnection graph",_).
match_time(S,read)         :- string(S), !, substring(S,"clauses read",_).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate do/3().

\PL*/
do(in(Label,Labels),Spec,Stream) :-
        (   member(Label,Labels),
%            printf(Stream,"  \\Row",[]),
            printf(Stream,"  ",[]),
            typeset_one_row(Spec,Stream),
            nl(Stream),
            fail
        ;   true
        ),
        !.
do(each(X),Spec,Stream) :-
        !,
        ( setof(L,A^B^info_db(L,A,B),Labels) ->
            do(in(X,Labels),Spec,Stream)
        ;   true
        ).
do(separator ,_,Stream) :- !, printf(Stream,"  \\Separator\n",[]).
do(   head(_),_,_)      :- !.
do(subhead(_),_,_)      :- !.
do(Spec,_,_) :- functor(Spec,titles,_),!.
do(Spec,_,_) :- functor(Spec,format,_),!.
do(Spec,_,_) :- functor(Spec,row,_),!.
do(Spec,_,_) :- printf("*** Unknown Spec ignored: %w\n",[Spec]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^



The following predicates are for testing purposes only.

\PL*/
go :-
        make_summary_table(
        [head("A Headline"),
         subhead("Sometimes you need a subheading"),
         titles("","Read","Goal","Conn","Compile","Run"),
%        format(c,l,r),
         row(section(X),
                time("clauses read ",X),
                time("Goal completion",X),
                time("Finding connections",X),
                time("Compile time",X),
                time(runtime,X)),
         each(X),
         separator,
         in(X,[1,2,3])
        ],
        output).
go1 :-
        make_summary_table(
        [head("Times in ms"),
         subhead("Sometimes you need a subheading"),
         titles("Label","Name","Time"),
         format(c,l,r),
         row(label(X),section(X),time(runtime,X)),
         each(X),
         in(X,[1,2,3])
        ],
        output).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
