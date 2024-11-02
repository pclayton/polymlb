url='https://github.com/melsman/aplcompile'
smlpkg=1

build () {
	make -C "$REPO" MLCOMP="$POLYMLB" clean all
}
