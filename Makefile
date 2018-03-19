##############################################################################
# This specifies what must be installed. 
# fxt is the transformation tool program. 
# fxtlib will copy in the installation directory all files needed for using fxt
# from within SML code
##############################################################################
TO_INSTALL = fxt fxtlib

##############################################################################
# These are the locations for executables, heap images and library files
##############################################################################
PREFIX         = /home/psi/aberlea/install
FXGREP_LIBDIR  = ${PREFIX}/fxgrep
FXT_BINDIR     = ${PREFIX}/bin
FXT_LIBDIR     = ${PREFIX}/fxt

##############################################################################
# The path where the SML-NJ binaries are located, and the name of the 
# SML-NJ executable with the Compilation manager built-in. If sml is in 
# your PATH at execution time, you fon't need the full path here.  
##############################################################################
SML_BINDIR = /local/sml/bin
SML_EXEC   = ${SML_BINDIR}/sml
##############################################################################
# No need to change this for SML-NJ 110.0.6. For earlier or working versions  
# 110.19 you might have to use the second or third line. This is the
# compilation manager function for making with a named description file. 
##############################################################################
#SML_MAKEDEF= val make = CM.make'
SML_MAKEDEF= val make = CM.make
#SML_MAKEDEF= fun make x = CM.make'{force_relink=true, group=x}

##############################################################################
# These should be fine on most unix machines
##############################################################################
SED       = sed
RM        = rm -f
RMDIR     = rmdir
COPY      = cp -f
MOVE	  = mv -f
CHMOD     = chmod
FIND      = find
#MKDIRHIER = mkdirhier
MKDIRHIER = mkdir -p

##############################################################################
# nothing to change below this line
##############################################################################

##############################################################################
# This is a find pattern for pruning paths when copying the source tree
##############################################################################
PRUNE = \( -path '*/CM*' -o -ipath '*/test*' -o -path '*.y.sml' \)
FXTLIB_PRUNE = \( -path '*/CM*' -o -path '*/CVS*' -o -path '*.y.sml' \)

SRC         = src

INSTALL_PARAMS =  ${SRC}/Transform/Params/installParams.sml


image.fxt: fxgrep.cm inst.params
	@echo "Creating the ${PROG_NAME} heap image..."
	echo "${SML_MAKEDEF}; make \"${SRC}/fxt.cm\"; \
	      SMLofNJ.exportFn(\"${SRC}/_fxt\",Transform.transform)" | ${SML_EXEC}

arch.os:
	${SML_BINDIR}/.arch-n-opsys | \
	${SED} -e 's/^.*HEAP_SUFFIX=\(.*\)$$/\1/' > .arch-opsys

fxgrep.cm: Makefile 
	echo Alias ${FXGREP_LIBDIR}/fxgrep.cm > ${SRC}/fxgrep.cm

fxt.sh: Makefile arch.os
	${RM} fxt.sh
	echo "#!/bin/sh -f" > fxt.sh
	echo >> fxt.sh
	echo "SML_BINDIR=${SML_BINDIR}" >> fxt.sh
	echo "FXT_LIBDIR=${FXT_LIBDIR}" >> fxt.sh
	cat fxt.sh.in >> fxt.sh

inst.params:
	x=`echo ${SML_BINDIR} | sed -e "s/\//\\\\\\\\\\\\//g"`; \
	${SED} -e "s/^\(.*SML_BINDIR = \"\).*\(\".*\)/\1$${x}\2/g"\
	            ${INSTALL_PARAMS} > ${INSTALL_PARAMS}.tmp
	${MOVE} ${INSTALL_PARAMS}.tmp ${INSTALL_PARAMS}

inst.dirs:
	test -d ${FXT_BINDIR} || ${MKDIRHIER} ${FXT_BINDIR}	
	test -d ${FXT_LIBDIR} || ${MKDIRHIER} ${FXT_LIBDIR}	

inst.fxtlib: inst.dirs fxgrep.cm
	for dir in `${FIND} ${SRC} ${FXTLIB_PRUNE} -prune -o -type d -print`; do \
	    ${MKDIRHIER} ${FXT_LIBDIR}/$${dir}; \
	done;
	for file in `${FIND} ${SRC} ${FXTLIB_PRUNE} -prune -o -name "*.cm" -print -o -name "*.y" -print -o -name "*.sml"  -print -o -name "*.sig" -print`; do \
	    ${COPY} $${file} ${FXT_LIBDIR}/$${file}; \
	done
	rm -f ${FXT_LIBDIR}/fxgrep.cm
	echo Group is > ${FXT_LIBDIR}/fxt.cm
	echo "  "${SRC}/fxt.cm >> ${FXT_LIBDIR}/fxt.cm


inst.fxt: inst.dirs fxt.sh arch.os
	${RM} ${FXT_BINDIR}/fxt ${FXT_BINDIR}/fxt.sh \
	      ${FXT_LIBDIR}/_fxt.`cat .arch-opsys`
	${COPY} fxt.sh ${FXT_BINDIR}/fxt
	${CHMOD} 755 ${FXT_BINDIR}/fxt
	${COPY} ${SRC}/_fxt.`cat .arch-opsys` ${FXT_LIBDIR}
	${CHMOD} 644 ${FXT_LIBDIR}/_fxt.`cat .arch-opsys`

install:
	for prog in ${TO_INSTALL}; do \
	    make inst.$${prog}; \
	done


uninstall: arch.os
	-${RM} ${FXT_BINDIR}/fxt; ${FXT_BINDIR}/fxt.sh
	-${RM} ${FXT_LIBDIR}/_fxt.`cat .arch-opsys`; \
	-${RMDIR} ${FXT_BINDIR} ${FXT_LIBDIR} 

clean:
	-${RM} -f ${SRC}/_fx.* fxt.sh .arch-opsys ${SRC}/fxgrep.cm 
	-find ${SRC} -type d -name CM -print | xargs ${RM} -r 
