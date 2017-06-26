#******************************************************************************
#* $Id: Makefile,v 0.00 1995/03/16 09:13:20 gerd Exp $
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

SUBDIRS           = default eclipse quintus
CLEAN_PATTERN	  = *~ *.bak *.log *.ilg *.blg pce.log core \\#*
DISTCLEAN_PATTERN = ${CLEAN_PATTERN} *.toc *.ind *.idx *.dvi ...* */*.cfg

all:
	@echo What\?\?

clean:
	@for i in ${SUBDIRS} ; do				\
		echo -n "$$i ";					\
		if test -f $$i/Makefile -a "$$i" != "." ;	\
		then (cd $$i; $(MAKE) clean);			\
		else (cd $$i; rm -f ${CLEAN_PATTERN});		\
		fi						\
	 done

veryclean distclean:
	@for i in ${SUBDIRS} ; do				\
		echo -n "$$i ";					\
		if test -f $$i/Makefile -a "$$i" != "." ;	\
		then (cd $$i; $(MAKE) distclean);		\
		else (cd $$i; rm -f ${DISTCLEAN_PATTERN});	\
		fi						\
	 done

