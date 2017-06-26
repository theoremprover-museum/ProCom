%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: tee.pl,v 1.4 1995/03/20 21:24:47 gerd Exp $
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

:- module_interface(tee). /*%-------------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.4 $}

This filter provides a means to trace or log the data flow in a filter chain.
If this filter is part of a chain then it acts like the empty filter. I.e. the
input is passed through. As a side effect it stores everything passed through
also in the file specified by the option |tee:file|.

The value of the option |tee:file| is an atom or string denoting a file name.
Some atoms have a special meaning. |output| and |stdout| denote the output
stream.  |stderr| denotes the standart error stream. If such an reeserved
value is encountered then the associated stream is used instead of opening a
file.  If a file is used then the output is appended to this file.

The option defaults to the value |output|.

Instead of specifying the output file in the option |tee:file| it is possible
to give the name as additional argument to the filter. Thus it is possible to
redirect the output of several incarnations of tee to different files:

The following example logs the input and the output of the filter
|mult_taut_filter| in the two files |file1| and |file2| respectively.

\begin{BoxedSample}
  input\_filter = [tee(file1),mult\_taut\_filter,tee(file2)]
\end{BoxedSample}

\PL*/
:- export tee/2,
	  tee/3.
:- begin_module(tee).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Our identification as a filter for \ProTop.

\PL*/
info(filter,"$Revision: 1.4 $","T filter").
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We need some modules from the system.

\PL*/
:-	lib(op_def),
	lib(options).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The value of the option |tee:file| is an atom or string denoting a file
name. Some atoms have a special meaning. |output| and |stdout| denote the
output stream. |stderr| denotes the standart error stream.

\PL*/
:- define_option 'tee:file' = output.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate tee/2(+Stream, +OutStream).

This is an empty filter. No action is performed except that terms are
transfered from the input stream |Stream| to the output stream
|OutStream|. Additionally the Term is echoed to the standart output stream.

\PL*/
tee(Stream,OutStream) :-
	is_option('tee:file',File),
	tee(Stream,OutStream,File).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate tee/3(+Stream, +OutStream, +OutFile).

\PL*/
tee(Stream,OutStream,File) :-
	( File = output ->
	    LogStream = output,
	    Close = no
	; File == [] ->
	    LogStream = output,
	    Close = no
	; File = stdout ->
	    LogStream = output,
	    Close = no
	; File = stderr ->
	    LogStream = stderr,
	    Close = no
	; open(File,append,LogStream) ->
	    Close = yes
	;   LogStream = output,
	    Close = no
	),
	repeat,
	read(Stream,Term),
	( Term = end_of_file ->
	    true
	;   writeclause(OutStream,Term),
	    writeclause(LogStream,Term),
	    fail
	),
	( Close = yes ->
	    close(LogStream)
	;   true
	),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
