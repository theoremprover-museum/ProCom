/******************************************************************************
%% $Id: filter_test.pl,v 1.1 1995/02/02 09:59:31 gerd Exp $
%%*****************************************************************************
%% Author: Gerd Neugebauer
%%=============================================================================

\PL*/
:- ['../main'].
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
test_filter(Name,File) :-
	test_filter(Name,File,output).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
test_filter(Name,File,Outfile) :-
	atom(Name),
	ensure_loaded(Name),
	open(File,read,Stream),
	( Outfile == output ->
	    OutStream = output
	;   open(Outfile,write,OutStream)
        ),
	Call =.. [Name,Stream,OutStream],
	call(Call,Name),
	close(Stream),
	( Outfile == output ->
	    true
	;   close(OutStream)
        ).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
