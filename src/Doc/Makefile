#******************************************************************************
#* $Id: Makefile,v 0.00 1995/01/25 08:47:08 gerd Exp $
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

MAN_FILES     = manual.tex		\
		protop.tex		\
		syntax.tex		\
		options.tex		\
		filters.tex		\
		capri.tex		\
		filter.tex		\
		install.tex		\
		matrix.tex		\
		../Otter/otter.tex	\
		../Setheo/setheo.tex	\
		Makefile
LATEX	      = latex
MAKEINDEX     = makeindex -s dotted.ist
BIBTEX        = bibtex

CLEAN_PATTERN	  = *~ *.bak *.log *.ilg *.blg pce.log core \\#*
DISTCLEAN_PATTERN = ${CLEAN_PATTERN} *.toc *.ind *.idx *.dvi ...* */*.cfg

manual.dvi: $(MAN_FILES)
	$(LATEX)     manual
	$(BIBTEX)    manual
	$(MAKEINDEX) manual
	$(LATEX)     manual

clean:
	@/bin/rm -f $(CLEAN_PATTERN)

veryclean distclean:
	@/bin/rm -f $(DISTCLEAN_PATTERN)
