#******************************************************************************
#* $Id: Makefile,v 1.69 1995/07/03 11:35:12 gerd Exp gerd $
#============================================================================
# 
# This file is part of ProCom.
# It is distributed under the GNU General Public License.
# See the file COPYING for details.
# 
# (c) Copyright 1995 Gerd Neugebauer
# 
# Net: gerd@imn.th-leipzig.de
# 
#============================================================================
# This makefile tries to automize the reconfiguration and installation
# of the ProCom system. Additionally some development tools are included
# which are not intended for general use.
#
# This makefile uses some additions introduced by GNUmake.
# It is not intended to work with any other variant of make!
#
#\
#\\
# >> Consult the file INSTALL for instructions on the installation of ProCom.
#// 
#/ 
#*=============================================================================
#* Configuration Section
#*=============================================================================
# These lines are intended for the installer of the ProCom system to specify
# the needs. The adaptable line should be fairly good documented. In case of
# doubt consult the documentation (or the sources:-).
# 

#------------------------------------------------------------------------------
# INSTALLDIR is the directory where `make install' puts the system.
# If you intend to install ProCom for using it only (in contrast to
# using it for prover development) this should be the only thing for
# you to change.
#
INSTALLDIR	= /u/home/procom

#------------------------------------------------------------------------------
# HERE is the place where the system is located.
# Change it only if the shell command  pwd  brings some undesired result
# e.g. when the current directory is automounted and the prefix (/auto)
# should be avoided.
#
HERE		= `pwd`

#------------------------------------------------------------------------------
# SUBDIRS is a list of directories which are considered when searching
# for Prolog libraries/files.
# This list can be used to add new directories containg alternate modules
# or descriptor files.
#
SUBDIRS		= ProTop				\
		  System				\
		  Prepare				\
		  Reductions				\
		  Normalform				\
		  Filter				\
		  ProCom				\
		  Capri					\
		  Otter					\
		  Setheo				\
		  Protein				\
		  Pool
#		  SelBa

#------------------------------------------------------------------------------
# ETCDIRS is a list of directories which are installed in addition to those
# defined in SUBDIRS. They are not considered to contain Prolog sources.
#
ETCDIRS		= Doc					\
		  inputs				\
		  Scripts

#------------------------------------------------------------------------------
# PROVERS is a list of provers to be preloaded by the main routine.
#
PROVERS		= procom				\
		  pool					\
		  otter					\
		  setheo
#		  selba

#------------------------------------------------------------------------------
# FILTERS is a list of filter modules to be loaded.
# The empty filter named 'none' is loaded additionally.
#
FILTERS		= tptp					\
		  mult_taut_filter			\
		  mpp					\
		  tee					\
		  equality_axioms			\
		  E_flatten				\
		  constraints

#------------------------------------------------------------------------------
# Some options of ProTop and friends need to be set to the installation
# directory. This operation is performed by ProTop on startup and when
# reset_options is executed.
# DEFAULT_OPTIONS is a list of strings containing option=value pairs where the
# value can contain Makefile variables. It is important to note that the
# construct $(HERE) expands to the current working directory when the creating
# the executables. I.e. this is the installation directory when make install
# is run.
#
DEFAULT_OPTIONS	= "input_path		= ['Samples']"			     \
		  "'report:style_path'	= \"$(HERE)/inputs\""		     \
		  "'ProCom:link_path'	= ['.','ProCom','$(HERE)/ProCom']"   \
		  "'ProCom:capri_path'	= ['.','Capri','$(HERE)/Capri']"     \
		  "'tptp:home'          = '/home/system/tptp/TPTP-v1.1.3/'"  \
		  "'prove:red_path'	= ['.','Red','$(HERE)/Prepare']"     \
       "'ProTop:path' = [\".\",\"Scripts\",\"$(HERE)/Scripts\",\"\~/Scripts\"]"

#------------------------------------------------------------------------------
# PROCOM_PROVERS contains the names of all prover modules which are loaded
# into ProCom.
# This information is stored in the file ProCom/procom.cfg as facts for the
# Prolog predicate define_prover/1.
#
PROCOM_PROVERS	= extension_procedure			\
		  me_para

#------------------------------------------------------------------------------
# PROCOM_SEARCH contains the names of all search modules which are loaded
# into ProCom. The mandatory p__ prefix is automatically prepended by the
# system.
# This information is stored in the file ProCom/procom.cfg as facts for the
# Prolog predicate define_search/1.
#
PROCOM_SEARCH	= depth_first				\
		  iterative_deepening			\
		  iterative_inferences			\
		  iterative_widening			\
		  iterative_broadening
 
#------------------------------------------------------------------------------
# PROCOM_REORDER contains the names of all reorder modules which are loaded
# into ProCom. The mandatory p-- prefix is automatically prepended by the
# system.
# This information is stored in the file ProCom/procom.cfg as facts for the
# Prolog predicate define_reorder/1.
#
PROCOM_REORDER  = simple
 
#------------------------------------------------------------------------------
# PROCOM_LIBS contains the subdirectories of libraries used by the ProCom
# linker. This information is used by RCS.
#
PROCOM_LIBS	= ProCom/default			\
		  ProCom/eclipse			\
		  ProCom/quintus

#------------------------------------------------------------------------------
# PROTOP is the name of the executable prover.
#
PROTOP		= protop

#------------------------------------------------------------------------------
# OTTER is the command which calles otter. A path may be required.
#
OTTER 		= otter
# OTTER 	= /home/system/otter/bin/otter

#------------------------------------------------------------------------------
# SETHEOHOME is the directory where the Setheo executables are located.
#
SETHEOHOME	= /home/system/setheo/bin


#------------------------------------------------------------------------------
# Name the Prolog executable
#
PROLOG		= eclipse

#*=============================================================================
#* End of Configuration Section
#*=============================================================================

###############################################################################
####
#### BEWARE:	You are not supposed to change anything below this line.
####		If you really do so it is on your own RISK.
####
###############################################################################

#------------------------------------------------------------------------------
# The shell to use (make internal)
#
SHELL		= /bin/sh

FILES		= main.pl				\
		  $(patsubst %,%/*.pl,$(SUBDIRS))	

ETC_FILES	= README				\
		  INSTALL				\
		  COPYING				\
		  Changes				\
		  Makefile				\
		  Doc/*.tex Doc/*.sty Doc/*.bib		\
		  Scripts/*.pt				\
		  Listing/*.pl Listing/*.tex		\
		  inputs/protop.sty			\
		  Emacs/pt-mode.el

#------------------------------------------------------------------------------
# The files under the control of RCS
#
RCSFILES	= $(FILES)				\
		  $(patsubst %,%/*.pl,$(PROCOM_LIBS))	\
		  $(ETC_FILES)

#------------------------------------------------------------------------------
# The files and directories used by make install
#
INSTALL_FILES	= $(SUBDIRS)				\
		  $(ETCDIRS)				\
		  main.pl				\
		  $(ETC_FILES)

#------------------------------------------------------------------------------
# Targets triggered by make configure
#
CONFIG_TARGETS	= ProTop/protop.cfg			\
		  ProCom/procom.cfg			\
		  Prepare/prove.cfg			\
		  Otter/otter.cfg			\
		  Setheo/setheo.cfg			\
		  Doc/doc.cfg				\
		  .emacs

#------------------------------------------------------------------------------
# Files removed by make clean
#
CLEAN_PATTERN	  = *~ *.bak *.log *.ilg *.blg pce.log core \\#*
DISTCLEAN_PATTERN = ${CLEAN_PATTERN} *.toc *.ind *.idx *.dvi ...* */*.cfg
CLEAN_DIRS 	  = . ${SUBDIRS} $(ETCDIRS)


#------------------------------------------------------------------------------
# Name of the directory to pack with make dist
#
DIST_PREFIX	= ProCom/

DIST		= $(DIST_PREFIX)

VERSION		= `echo '$Revision: 1.69 $ '| sed -e 's/[^0-9.]//g'`

#*=============================================================================

all: $(PROTOP)

configure: $(CONFIG_TARGETS)
config:    $(CONFIG_TARGETS)

#*=============================================================================

ProTop/protop.cfg: Makefile
	@echo Creating $@
	@echo '%% ATTENTION: This file has been generated automatically.'   >$@
	@echo '%%            Changes to this file may be overwritten.'	   >>$@
	@echo '%%            Consult INSTALL and Makefile for details.'	   >>$@
	@echo '%%'							   >>$@
	@echo '%% Last created:' `date`					   >>$@
	@echo '%%      by user:' $(USER)				   >>$@
	@echo '%%      on host:' `hostname`				   >>$@
	@echo ''							   >>$@
	@echo '%----------------------------------------------------------'>>$@
	@echo '%- Store the version number from the Makefile.'		   >>$@
	@echo '%-'							   >>$@
	@echo "protop_version(\"$(VERSION)\")."				   >>$@
	@echo ''							   >>$@
	@echo '%----------------------------------------------------------'>>$@
	@echo '%- Declare where ProTop (main.pl) lives.'		   >>$@
	@echo '%-'							   >>$@
	@echo 'protop_home("'${HERE}'").'				   >>$@
	@echo ''							   >>$@
	@echo '%----------------------------------------------------------'>>$@
	@echo '%- Declare the path where Prolog files can be found.'	   >>$@
	@echo '%-'							   >>$@
	@for i in ${SUBDIRS}; \
	 do echo 'prover_system_path("'${HERE}'/'$$i'").' >> $@; done
	@echo ''							   >>$@
	@echo '%----------------------------------------------------------'>>$@
	@echo '%- Declare the default options (search paths).'		   >>$@
	@echo '%- Those are reset when ProTop is started.'		   >>$@
	@echo '%-'							   >>$@
	@for i in ${DEFAULT_OPTIONS} none; \
	 do echo "default_option("$$i")." >> $@; done
	@echo ''							   >>$@
	@echo '%----------------------------------------------------------'>>$@
	@echo '%-  A list of ProTop commands is required for help.'	   >>$@
	@echo '%-' 							   >>$@
	@sed	-e '/^$$/d'			\
		-e '/^[^p]/d'			\
		-e '/^.[^t]/d'			\
		-e '/^pt(/s/)[ 	]*:-.*$$//'	\
		-e 's/^pt(//'			\
		-e '/^(/s/)$$//'		\
		-e 's/^(//'			\
		-e 's/^/pt_command("/'		\
		-e 's/$$/")./'			\
		ProTop/protop.pl					   >>$@

ProCom/procom.cfg: Makefile
	@echo Creating $@
	@echo '%% ATTENTION: This file has been generated automatically.'   >$@
	@echo '%%            Changes to this file may be overwritten.'	   >>$@
	@echo '%%            Consult INSTALL and Makefile for details.'	   >>$@
	@echo '%%'							   >>$@
	@echo '%% Last created:' `date`					   >>$@
	@echo '%%      by user:' $(USER)				   >>$@
	@echo '%%      on host:' `hostname`				   >>$@
	@echo '%%'							   >>$@
	@echo '%----------------------------------------------------------'>>$@
	@echo '%- This file contains declaration for provers which will'   >>$@
	@echo '%- be loaded under the control of ProCom/Capri.'		   >>$@
	@echo '%-'							   >>$@
	@echo '%- The order is relevant when automatically selecting an'   >>$@
	@echo '%- appropriate prover.'					   >>$@
	@echo '%-'							   >>$@
	@for i in ${PROCOM_PROVERS}; \
	 do echo 'define_prover('$$i').' >> $@; done
	@echo ''							   >>$@
	@echo '%----------------------------------------------------------'>>$@
	@echo '%- Declare the search strategies to be loaded.'		   >>$@
	@echo '%-'							   >>$@
	@for i in ${PROCOM_SEARCH}; \
	 do echo 'define_search('$$i').' >> $@; done
	@echo ''							   >>$@
	@echo '%----------------------------------------------------------'>>$@
	@echo '%- Declare the reorder strategies to be loaded.'		   >>$@
	@echo '%-'							   >>$@
	@for i in ${PROCOM_REORDER}; \
	 do echo 'define_reorder('$$i').' >> $@; done
	@echo ''							   >>$@

Prepare/prove.cfg: Makefile
	@echo Creating $@
	@echo '%% ATTENTION: This file has been generated automatically.'   >$@
	@echo '%%            Changes to this file may be overwritten.'	   >>$@
	@echo '%%            Consult INSTALL and Makefile for details.'	   >>$@
	@echo '%%'							   >>$@
	@echo '%% Last created:' `date`					   >>$@
	@echo '%%      by user:' $(USER)				   >>$@
	@echo '%%      on host:' `hostname`				   >>$@
	@echo '%%'							   >>$@
	@echo '%----------------------------------------------------------'>>$@
	@echo '%- This file contains declaration for modules which will'   >>$@
	@echo '%- be used for preparation.'				   >>$@
	@echo '%-'							   >>$@
	@echo '%----------------------------------------------------------'>>$@
	@echo '%- Declare the prover modules to be loaded.'		   >>$@
	@echo '%-'							   >>$@
	@for i in ${PROVERS}; \
	 do echo "prover_module('"$$i"')." >> $@; done
	@echo ''							   >>$@
	@echo '%----------------------------------------------------------'>>$@
	@echo '%- Declare the input filters to be loaded.'		   >>$@
	@echo '%-'							   >>$@
	@for i in ${FILTERS} none; \
	 do echo "filter_module('"$$i"')." >> $@; done
	@echo ''							   >>$@

Otter/otter.cfg: Makefile
	@echo Creating $@
	@echo '%% ATTENTION: This file has been generated automatically.'   >$@
	@echo '%%            Changes to this file may be overwritten.'	   >>$@
	@echo '%%            Consult INSTALL and Makefile for details.'	   >>$@
	@echo '%%'							   >>$@
	@echo '%% Last created:' `date`					   >>$@
	@echo '%%      by user:' $(USER)				   >>$@
	@echo '%%      on host:' `hostname`				   >>$@
	@echo '%%'							   >>$@
	@echo '%----------------------------------------------------------'>>$@
	@echo '%- This file contains the declaration for otter.' 	   >>$@
	@echo '%- We have to specify the place where otter lives.'	   >>$@
	@echo '%-'							   >>$@
	@echo 'define_otter("'$(OTTER)'").'				   >>$@
	@echo ''							   >>$@

Setheo/setheo.cfg: Makefile
	@echo Creating $@
	@echo '%% ATTENTION: This file has been generated automatically.'   >$@
	@echo '%%            Changes to this file may be overwritten.'	   >>$@
	@echo '%%            Consult INSTALL and Makefile for details.'	   >>$@
	@echo '%%'							   >>$@
	@echo '%% Last created:' `date`					   >>$@
	@echo '%%      by user:' $(USER)				   >>$@
	@echo '%%      on host:' `hostname`				   >>$@
	@echo '%%'							   >>$@
	@echo '%----------------------------------------------------------'>>$@
	@echo '%- This file contains the declaration for setheo.' 	   >>$@
	@echo '%- We have to specify the place where setheo lives.'	   >>$@
	@echo '%-'							   >>$@
	@echo 'define_setheo("'$(SETHEOHOME)'").'			   >>$@
	@echo ''							   >>$@

#*=============================================================================

Doc/doc.cfg: Makefile
	@echo Creating $@
	@echo '%% ATTENTION: This file has been generated automatically.'   >$@
	@echo '%%            Changes to this file may be overwritten.'	   >>$@
	@echo '%%            Consult INSTALL and Makefile for details.'	   >>$@
	@echo '%%'							   >>$@
	@echo '%% Last created:' `date`					   >>$@
	@echo '%%      by user:' $(USER)				   >>$@
	@echo '%%      on host:' `hostname`				   >>$@
	@echo '%%'							   >>$@
	@echo '%----------------------------------------------------------'>>$@
	@echo "\gdef\Version{$(VERSION)}"				   >>$@
	@echo ''							   >>$@
	@echo '\gdef\ProTopFilters{%{\catcode`\_=12%'			   >>$@
	@for i in ${FILTERS}; do echo "  \Filter{$$i}%" >> $@; done
	@echo "}%}"							   >>$@
	@echo ''							   >>$@
	@echo '\gdef\ProTopProvers{%{\catcode`\_=12%'			   >>$@
	@for i in ${PROVERS}; do echo "  \Prover{$$i}%" >> $@; done
	@echo "}%}"							   >>$@
	@echo ''							   >>$@

#*=============================================================================

.emacs:	Makefile
	@echo Creating $@
	@echo ';;; ATTENTION: This file has been generated automatically.'  >$@
	@echo ';;;            Changes to this file may be overwritten.'	   >>$@
	@echo ';;;            Consult INSTALL and Makefile for details.'   >>$@
	@echo ';;;'							   >>$@
	@echo ';;; Last created:' `date`				   >>$@
	@echo ';;;      by user:' $(USER)				   >>$@
	@echo ';;;      on host:' `hostname`				   >>$@
	@echo ';;;'							   >>$@
	@echo ';;;--------------------------------------------------------'>>$@
	@echo '	; Expand the load path if required'			   >>$@
	@echo "(require 'cl)"						   >>$@
	@echo "(setq load-path (adjoin \"$(HERE)/Emacs\" load-path))"	   >>$@
	@echo ''							   >>$@
	@echo '	; get the pt-mode loaded automatically when used'	   >>$@
	@echo "(autoload 'pt-mode \"$(HERE)/Emacs/pt-mode.el\" nil t)"	   >>$@
	@echo ''							   >>$@
	@echo '	; use pt-mode for files with the extension .pt'		   >>$@
	@echo "(setq auto-mode-alist (adjoin (cons \"\\.pt$$\" 'pt-mode) \
					 auto-mode-alist))"		   >>$@
	@echo ''							   >>$@
	@echo '	; set the correct path for the ProTop excutable'	   >>$@
	@echo "(setq protop-program \"$(HERE)/$(PROTOP)\")"		   >>$@
	@echo ''							   >>$@

#*=============================================================================

$(PROTOP): config $(FILES) Makefile
	@echo Creating $@ executable ...
	@unset DISPLAY;				\
	 ( echo "compile(main).";		\
	   echo "save('$@'), protop, exit(0).";	\
	   echo "halt."				\
	 ) | $(PROLOG) > $@.log
	@echo See $@.log for further informations.

#*=============================================================================

$(PROTOP).st: config $(FILES) Makefile
	@echo Creating $@ saved state ...
	@unset DISPLAY;				\
	 ( echo "compile(main).";		\
	   echo "save('$@').";			\
	   echo "halt."				\
	 ) | $(PROLOG) > $@.log
	@echo See $@.log for further informations.

#*=============================================================================

manual Doc/manual.dvi: Doc/doc.cfg Doc/manual.tex
	(cd Doc; $(MAKE) manual.dvi)

#*=============================================================================

listing xref changelog log:
	(cd Listing; $(MAKE) $@)

#*=============================================================================
#* 
#*	Version Control.
#* 
ci:
	ci -l -mok $(RCSFILES)
	$(MAKE) change_LOG
co:
	co -r $(RCSFILES)

change_LOG LOG:
	@echo Updating Changes ...
	@head -16 Changes > .changes_head
	@tail +17 Changes > .changes
	@mv .changes_head Changes
	@echo ''						>> Changes
	@echo '\begin{Changes}	'				>> Changes
	@echo '\item none'					>> Changes
	@echo '\end{Changes}'					>> Changes
	@echo ''						>> Changes
	@echo "\\LogDate{Changes in Version $(VERSION)  }{`date`}" >> Changes
	@echo '%----------------------------------'		>> Changes
	@cat .changes						>> Changes
	@$(RM) .changes
#*=============================================================================
#* 
#*	Installation.
#* 
install:
	@if test "$(INSTALLDIR)" = "." ;			\
	then	echo 'Installdir is here. Nothing moved.';	\
		$(MAKE) $(PROTOP) $(PROTOP).st;			\
	else							\
		echo 'Installing in $(INSTALLDIR)';		\
		install -d $(INSTALLDIR);			\
		tar -cf - $(INSTALL_FILES) | (cd $(INSTALLDIR); tar -xf -);  \
		cd $(INSTALLDIR);				\
		/bin/rm -f RCS */RCS */*/RCS;			\
		$(MAKE) distclean $(PROTOP) $(PROTOP).st;	\
	fi
#*=============================================================================

clean:
	@echo -n 'Clearing '
	@for i in ${CLEAN_DIRS} ; do				\
		echo -n "$$i ";					\
		if test -f $$i/Makefile -a "$$i" != "." ;	\
		then (cd $$i; $(MAKE) clean);			\
		else (cd $$i; rm -f ${CLEAN_PATTERN});		\
		fi						\
	 done
	@echo ''

distclean:
	@echo -n 'Clearing '
	@for i in ${CLEAN_DIRS} ; do				\
		echo -n "$$i ";					\
		if test -f $$i/Makefile -a "$$i" != "." ;	\
		then (cd $$i; $(MAKE) distclean);		\
		else (cd $$i; rm -f ${DISTCLEAN_PATTERN});	\
		fi						\
	done
	@echo ''

#*=============================================================================

Makefile: $(FILES)
	echo -n '#' >> Makefile

dist: Makefile
	cd ..; tar -cvf - $(DIST) | gzip > ProCom-$(VERSION).tar.gz

#*=============================================================================
# The next line is automatically enlarged to tell RCS to increment the
# version number of the executable. Thsi version number is identical
# to the RCS version number of this Makefile.
#
#################################################