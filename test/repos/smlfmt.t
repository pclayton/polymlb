url='https://github.com/shwestrick/smlfmt'

build () {
	make -C "$REPO" MLCOMP="$POLYMLB" clean smlfmt
}
