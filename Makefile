CC=ghc

OUTDIR=build
OUTBIN=shortener

MAIN_SRC=Shortener.hs

all: create_output_dir
	${CC} --make -o ${OUTDIR}/${OUTBIN} -outputdir ${OUTDIR} ${MAIN_SRC}

clean: 
	rm -r ${OUTDIR}/*

create_output_dir: 
	-mkdir build
