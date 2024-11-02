url='https://github.com/melsman/apltail'
smlpkg=1

build () {
	make -C "$REPO" MLCOMP="$POLYMLB" clean all
}
