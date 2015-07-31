# The haskell compiler in this case
CC=ghc

# Directory to output all intermediate files and the final binary to
OUTDIR=build
# The name of the binary to be emitted
OUTBIN=shortener

# The main haskell file
MAIN_SRC=Shortener.hs

# Compile all
all: create_output_dir
	${CC} --make -o ${OUTDIR}/${OUTBIN} -outputdir ${OUTDIR} ${MAIN_SRC}

# Remove all build files
clean: 
	rm -r ${OUTDIR}/*

# Create the output directory
create_output_dir: 
	-mkdir ${OUTDIR}
