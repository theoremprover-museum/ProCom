%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: matrix-save.pl,v 1.1 1995/03/06 23:04:17 gerd Exp $
%%%============================================================================
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

:- module_interface(matrix). /*%-----------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.1 $}

The purpose of this module is to provide predicates to store matrices.


\PL*/
:- export 'CheckPath'/1,
	  'Clause'/3,
	  'ClauseLength'/2,
	  'Connection'/4,
	  'Contrapositive'/4,
	  'CP_by_index'/4,
	  'GoalClause'/1,
	  'Label'/2,
	  'MaxClause'/1,
	  'NegativeClause'/1,
	  'PositiveClause'/1,
	  add_goal_mark/1,
	  add_connection/2,
	  add_connection/4,
	  add_contrapositive/3,
	  delete_clause/1,
	  delete_clauses/1,
	  delete_contrapositive/2,
	  delete_literal/1,
	  delete_literals/1,
	  make_index/3,
	  remove_clause/1,
	  reset_matrix/0,
	  show_matrix/1,
	  show_matrix/2,
	  store_clause/1,
	  store_clause_vars/3,
	  store_label/1.

:- export 'Matrix#Macro'/2.
:-	define_macro('Connection'/2,'Matrix#Macro'/2,[]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- begin_module(matrix).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:-	lib(hook),
	lib(message),
	lib(literal),
	lib(numbervars).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
'Matrix#Macro'(no_macro_expansion('Connection'(A-B,C-D)),
	       'Connection'(A,B,C,D)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\subsection{Internal Representation of the Matrix}

Internally the matrix is stored in the Prolog data base. Various informations
can be accessed through the following predicates which are declared dynamic.

\PL*/
:- set_flag(occur_check,on).

:- dynamic 'CheckPath'/1,
	   'ClauseVars'/3,
	   'ClauseLength'/2,
	   'Connection'/4,
	   'GoalClause'/1,
	   'Label'/2,
	   'LiteralDegree'/2,
	   'NegativeClause'/1,
	   'PositiveClause'/1.

:- setval('Matrix:Max Clause',0).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\subsection{Working on the Matrix}

Next we declare the hooks provided by this module.

\PL*/
:- define_hook add_contrapositive_hook/3.
:- define_hook reset_matrix/0.
:- define_hook delete_clause_hook/1.
:- define_hook delete_contrapositive_hook/1.
:- define_hook delete_literal_hook/1.
:- define_hook store_HashClause/3.
:- define_hook store_clause/2.
:- define_hook store_empty_clause/1.
:- define_hook store_goal_clause/2.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate MaxClause/1(?Index).

This predicate provides access to the internal clause counter. It is
mainly defined for backward compatibility since nobody should really
need this information.

\PL*/
'MaxClause'(Index) :-
	getval('Matrix:Max Clause',Index).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate Clause/3(Literal, Body, Index).

\PL*/
'Clause'(++Lit,Body,Index) :-
	'Contrapositive'(Lit,'++',Body,Index).
'Clause'(--Lit,Body,Index) :-
	'Contrapositive'(Lit,'--',Body,Index).

'CP_by_index'(C,L,Literal,Body) :-
	nonvar(Literal),
	( Literal = (++Lit) -> Sign = '++'
	; Literal = (--Lit) -> Sign = '--'
	),
	'Contrapositive'(Lit,Sign,Body,C-L).
'CP_by_index'(C,L,Literal,Body) :-
	var(Literal),
	'Contrapositive'(Lit,Sign,Body,C-L),
	Literal =.. [Sign,Lit].

'Contrapositive'(Lit,Sign,Body,C-L) :-
	nonvar(C),
	nonvar(L),
	!,
	concat_atom(['#idx',C,'-',L],Key),
	recorded(Key,cp(Lit,Sign,Body,C-L)).
'Contrapositive'(Lit,Sign,Body,C-L) :-
	nonvar(Lit),
	!,
	( Sign = (++) ; Sign = (--) ),
	functor(Lit,F,A),
	concat_atom(['#cp',Sign,F,'/',A],Key),
	recorded(Key,cp(Lit,Sign,Body,C-L)).
'Contrapositive'(Lit,Sign,Body,C-L) :-
	err("*** Instantiation fault in ",'Contrapositive'(Lit,Sign,Body,C-L)),
	fail.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate reset_matrix/0().

This predicate removes all dynamical predicates representing information about
the matrix from the Prolog data base.

\PL*/
reset_matrix :-
	run_hooks(reset_matrix),
	retract_all('CheckPath'(_)),
	retract_all('Label'(_,_)),
	setval('Matrix:Max Clause',0),
	(   delete_clause(_),
	    fail
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate delete_clause/1(?Index).

Delete a single clause with the index |Index|. Any information is
removed from this module refering to this clause. Before the hook
|delete_clause_hook| is called to allow other modules to modyfy their
informations accordingly. 

{\bf Note:} At the time when this hook is called the system part of
the clause has not been removed yet.
\PL*/
delete_clause(Index) :-
	erase('#clause',Index),
	run_hooks(delete_clause_hook,[Index]),
	concat_atom(['#lit',Index],XKey),
	recorded_list(XKey,List),
	(   member(L,List),
	    erase(XKey),
	    concat_atom(['#idx',Index,'-',L],IKey),
	    erase(IKey,cp(Lit,Sign,_,_)),
	    functor(Lit,F,A),
	    concat_atom(['#cp',Sign,F,'/',A],Key),
	    erase(Key),
	    fail
	;   true
	),
	retract_all('GoalClause'(Index)),
	retract_all('PositiveClause'(Index)),
	retract_all('NegativeClause'(Index)),
	retract_all('ClauseLength'(Index,_)),
	retract_all('Connection'(Index,_,_,_)),
	retract_all('Connection'(_,_,Index,_)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate delete_clauses/1(+ClauseList).

Delete all clauses in the list |ClauseList|. This list contains the clause
indices of the clauses to be deleted. This predicate fails only if called with
a argument of wrong type.
\PL*/
delete_clauses([]).
delete_clauses([H|T]) :-
	delete_clause(H),
	delete_clauses(T).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate add_connection/2().

\PL*/
add_connection(FromC-FromI,ToC-ToI) :-
	assert('Connection'(FromC,FromI,ToC,ToI)).

add_connection(FromC,FromI,ToC,ToI) :-
	assert('Connection'(FromC,FromI,ToC,ToI)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate remove_connection/2().

\PL*/
remove_connection(FromC-FromI,ToC-ToI) :-
	retract('Connection'(FromC,FromI,ToC,ToI)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate make_index/3().

Not used yet.

\PL*/
make_index(Clause,Literal,Index) :-
	( ( nonvar(Clause), nonvar(Literal) ) ->
	    concat_atom([Clause,'/',Literal],Index)
	; nonvar(Index) ->
	    name(Index,Name),
	    append(Nclause,[0'/|NLit],Name),
	    name(Clause,Nclause),
	    name(Literal,NLit)
	;   err("\n*** Instantiation error: make_index/3"),
	    fail
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate store_label/1(+Label).

\PL*/
store_label(Label) :-
	getval('Matrix:Max Clause',Index),
	assert('Label'(Label,Index)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate store_clause/2(+Clause, +ClauseIndex).

A given clause is stored in the Prolog data base. The clause |Clause|
is either a list or a list preceeded by |:-|. |ClauseIndex| is the
index of the clause to be stored, i.e.\ an integer.

If the empty clause is encountered the special fact |'Clause'([],[],
ClauseIndex-0)| is stored. The |?-| indicates goal clauses. In
addition to the contrapositives the fact

	|'GoalClause'(ClauseIndex)| 

\noindent is inserted into the Prolog data base. The main work of
storing the contrapositives is done by the predicate |store_clause/4|.

\PL*/
:- mode store_clause(+).
store_clause([]) :-
	!,
	incval('Matrix:Max Clause'),
	getval('Matrix:Max Clause',ClauseIndex),
	assert('ClauseLength'(ClauseIndex,0)),
	record('#clause',ClauseIndex),
	run_hooks(store_empty_clause,[ClauseIndex]).
store_clause((?-Clause)) :-
	!,
	incval('Matrix:Max Clause'),
	getval('Matrix:Max Clause',ClauseIndex),
	record('#clause',ClauseIndex),
	add_goal_mark(ClauseIndex),
	prepare_clause(Clause,ClauseIndex,0,0,NewClause),
	get_flag(occur_check,OC),
	set_flag(occur_check,on),
	store_contrapositives(NewClause,X-X),
	set_flag(occur_check,OC),
	run_hooks(store_clause,[Clause,ClauseIndex]),
	run_hooks(store_goal_clause,[Clause,ClauseIndex]).
store_clause(Clause) :- 
	incval('Matrix:Max Clause'),
	getval('Matrix:Max Clause',ClauseIndex),
	record('#clause',ClauseIndex),
	prepare_clause(Clause,ClauseIndex,0,0,NewClause),
	get_flag(occur_check,OC),
	set_flag(occur_check,on),
	store_contrapositives(NewClause,X-X),
	set_flag(occur_check,OC),
	run_hooks(store_clause,[Clause,ClauseIndex]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate store_clause_vars/3().

\PL*/
store_clause_vars(_,_,_).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


\Predicate add_goal_mark/1(+Clauses).

Mark the given clause or clauses as goals. The argument |Clause| can
either be an index of a single clause or a list of such indices.

\PL*/
:- mode add_goal_mark(++).
add_goal_mark([]) :-
	!.
add_goal_mark([Clause|Clauses]) :-
	!,
	add_goal_mark(Clause),
	add_goal_mark(Clauses).
add_goal_mark(Clause) :-
	( 'GoalClause'(Clause) ->
	    true
	;   assert('GoalClause'(Clause))
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate prepare_clause/5(+Clause, +ClauseIndex, +NextLiteralIndex,
			    +PosLen, -NewClause).

Given a clause |Clause| as a list of simple literals and the index
|ClauseIndex| of the clause compute the associated list of literals of
the form {\tt literal({\em SimpleLiteral},{\em Index})}.

As a side effect the clause length is computed. Additionally it is
checked if the clause is entirely positive or negative. Those
information is stored in the Prolog database.

\PL*/
:- mode prepare_clause(+,++,++,++,-).
prepare_clause([],ClauseIndex,Length,PosLength,[]) :-
	assert('ClauseLength'(ClauseIndex,Length)),
	(PosLength =:= Length -> assert('PositiveClause'(ClauseIndex)); true),
	(PosLength =:= 0      -> assert('NegativeClause'(ClauseIndex)); true).
prepare_clause([Literal|Tail],
	       ClauseIndex,
	       LiteralIndex,
	       PosLen,
	       [literal(Literal,ClauseIndex-NewLiteralIndex)|NewTail]) :-
	(functor(Literal,(--),1) ->
	    NewPosLen = PosLen
	;   NewPosLen is PosLen + 1
	),
	NewLiteralIndex is LiteralIndex + 1,
	prepare_clause(Tail,ClauseIndex,NewLiteralIndex,NewPosLen,NewTail).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate store_contrapositives/2(+Lit_Postfix, +Prefix).

Store a single clause in the Prolog data base. A clause consists of a
list of literals. For each literal a contrapositive is stored in the
predicates |'Clause'/3| and |'HashClause'/3|.

\noindent\begin{minipage}{80mm}
The two arguments represent a single list together with a current
element |Literal| in the list. This is illustrated by the picture on the
right side.

The predicate |store_contrapositives/2| works recursive, moving
elements from the first list into the second one. It terminates as
soon as the first argument is empty.
\end{minipage}\hfill
\begin{minipage}{65mm}
 {\unitlength=1mm\small\tt
 \begin{picture}(62,16)(0,-8)
   \multiput(0,0)(5,0){12}{\framebox(5,5){}}
   {\thicklines\put(22.5,-4){\vector(0,1){4}}}
   \put(22.5,-6){\makebox(0,0){Lit}}
   \put(0,8){\makebox(0,0)[l]{\(\overbrace{\rule{20mm}{.1pt}}^{\mbox{\tt
   Prefix}}\)}} 
   \put(25,8){\makebox(0,0)[l]{\(\overbrace{\rule{35mm}{.1pt}}^{\mbox{\tt
   Postfix}}\)}} 
 \end{picture}}
\end{minipage}

A difference list type approach is used to avoid explicit use of
|append/3|. The |fail| branch is required to undo the unification for
appending.

\PL*/
:- mode store_contrapositives(+,?).
store_contrapositives([],_).
store_contrapositives([literal(Literal,LitInd)|Postfix],Prefix-PE) :-
	(   PE = Postfix,
	    add_contrapositive(Literal,LitInd,Prefix),
	;   PE = [literal(Literal,LitInd)|PE1]
	),
	store_contrapositives(Postfix,Prefix-PE1).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate add_contrapositive/3(+Literal, +Index, +Body).

\PL*/
add_contrapositive(Literal,Index,Body) :-
	( Literal = (++Lit) ->
	    Sign = '++'
	; Literal = (--Lit) ->
	    Sign ='--'
	;   err(unexpected_error),
	    fail
	),
	Index = C-L,
	concat_atom(['#idx',C,'-',L],IKey),
	record(IKey,cp(Lit,Sign,Body,Index)),
	functor(Lit,F,A),
	concat_atom(['#cp',Sign,F,'/',A],Key),
	record(Key,cp(Lit,Sign,Body,Index)),
	concat_atom(['#lit',C],XKey),
	record(XKey,L),
	run_hooks(add_contrapositive_hook,[Index,Literal,Body]),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate delete_contrapositive/2(+ClauseIndex, +LiteralIndex).

\PL*/
delete_contrapositive(C,L) :-
	concat_atom(['#idx',C,'-',L],IKey),
	erase(IKey,cp(Lit,Sign,Prefix,LitInd)),
	functor(Lit,F,A),
	concat_atom(['#cp',Sign,F,'/',A],Key),
	erase(Key,cp(Lit,Sign,Prefix,LitInd)),

	retract_all('Connection'(C,L,_,_)),
	retract_all('Connection'(_,_,C,L)),
	run_hooks(delete_contrapositive_hook,[C-L]).
%	( \+ 'CP_by_index'(_,_,_,C-_) ->
%	    retract_all('Connection'(C,_,_,_)),
%	    retract_all('Connection'(_,_,C,_)),
%	    retract_all('GoalClause'(C)),
%	    retract_all('PositiveClause'(C)),
%	    retract_all('NegativeClause'(C)),
%	    retract_all('ClauseLength'(C,_))
%	;
%	    true
%	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate delete_literal/1(+Index).

This predicate deletes the literal |Index|. This means that the contrapositive
and all connections are no longer accessible. Various other informations are
also updated.

\PL*/
delete_literal(Index) :-
	Index = Clause-LI,
	run_hooks(delete_literal_hook,[Index]),
	retract_all('Connection'(Clause,LI,_,_)),
%	retract_all('CP_by_index'(Clause,LI,_,_)),
	retract_all('Contrapositive'(_,_,_,Index)),

	retract('ClauseLength'(Clause,Len)),
	Len1 is Len-1,
	assert('ClauseLength'(Clause,Len1)),
	(  % retract('CP_by_index'(Clause,I,Literal,Body)),
	   % delete(literal(_,Index),Body,NewBody),
	   % assert('CP_by_index'(Clause,I,Literal,NewBody)),
	    fail
	;   retract('Contrapositive'(Literal,Sign,Body,Clause-I)),
	    delete(literal(_,Index),Body,NewBody),
	    assert('Contrapositive'(Literal,Sign,NewBody,Clause-I)),
	    fail
	;   true
	),
	( 'PositiveClause'(Clause) ->
	    ( Len1 = 0 ->
		assert('NegativeClause'(Clause))
	    ;	true
	    )
	; 'NegativeClause'(Clause) ->
	    ( Len1 = 0 ->
		assert('PositiveClause'(Clause))
	    ;	true
	    )
	; 'CP_by_index'(Clause,_,Literal,Body) ->
	    ( ( functor(Literal,'++',1),
		\+ member(-- _,Body) ) ->
		assert('PositiveClause'(Clause))
	    ; ( functor(Literal,'--',1),
		\+ member(++ _,Body) ) ->
		assert('NegativeClause'(Clause))
	    ;	true
	    )
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate delete_literals/1(+LitList).

Delete all literals given in the list |LitList| as index.
\PL*/
delete_literals([]).
delete_literals([H|T]) :-
	delete_literal(H),
	delete_literals(T).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate show_matrix/1(+Prefix).

Print a matrix to the output stream. Each line is preceeded by |Prefix|.

\PL*/
:- mode show_matrix(++).
show_matrix(Prefix) :-
	show_matrix(Prefix,output).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate show_matrix/2(+Prefix, +Stream).

Print a matrix to the stream |Stream|. Each line is preceeded by |Prefix|.

\PL*/
:- mode show_matrix(++,++).
show_matrix(Prefix,Stream) :-
	(   'CP_by_index'(Idx,1,Lit,Rest),
	    LitList = [literal(Lit,Idx-1)|Rest],
	    ( 'GoalClause'(Idx) ->
		printf(Stream,"%w ?-[ %% Clause %w",[Prefix,Idx])
	    ;	printf(Stream,"%w   [ %% Clause %w",[Prefix,Idx])
	    ),
	    (	'Label'(Label,Idx),
		printf(Stream," :: %w",[Label]),
		fail
	    ;	true
	    ),
	    nl(Stream),
	    numbervars(LitList,0,_),
	    show_clause(LitList,Prefix,Stream),
	    printf(Stream,"%w	].%n",[Prefix]),
	    fail
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate show_clause/3(+LiteralList, +Prefix, +Stream).

\PL*/
:- mode show_clause(+,++,++).
show_clause([],_,_).
show_clause([literal(Lit,_)],Prefix,Stream) :-
	!,
	printf(Stream,"%w     %Mw%n",[Prefix,Lit]).
show_clause([literal(Lit,_)|Rest],Prefix,Stream) :-
	printf(Stream,"%w     %Mw,%n",[Prefix,Lit]),
	show_clause(Rest,Prefix,Stream).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- skipped show_matrix/1,
	   show_matrix/2.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog*/
