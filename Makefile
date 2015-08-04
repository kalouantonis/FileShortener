# The haskell compiler in this case
CC=ghc

DEP_INSTALL_CMD=cabal install
DEPS="directory-tree"

# Directory to output all intermediate files and the final binary to
OUTDIR=build
# The name of the binary to be emitted
OUTBIN=shortener

# The main haskell file
MAIN_SRC=Shortener.hs


# Compile all
all: compile

# Compile and install dependencies
install: deps compile move_to_usr

compile: create_output_dir
	${CC} --make -o ${OUTDIR}/${OUTBIN} -outputdir ${OUTDIR} ${MAIN_SRC}

# Remove all build files
clean: 
	rm -r ${OUTDIR}/*

# Create the output directory
create_output_dir: 
	-mkdir ${OUTDIR}

deps:
	${DEP_INSTALL_CMD} ${DEPS}

move_to_usr:
	cp ${OUTDIR}/${OUTBIN} /usr/bin/${OUTBIN}
