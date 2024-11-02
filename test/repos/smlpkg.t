url='https://github.com/diku-dk/smlpkg'

build () {
	make -C "$REPO" MLCOMP="$POLYMLB" clean all
}
