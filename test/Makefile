POLYMLB   != realpath ../polymlb

SML       != ls sml/*.sml
REPOS     != ls repos/*.t
REPOS_DIR := repos/r

all: sml

sml: $(SML)

$(SML):
	@ echo 'PolyML.print_depth ~1; use "test.sml"; use "$@";' | poly -q --error-exit
	@ echo $@: OK

repos: $(REPOS)

# requires gmake
$(REPOS):
	@ [ -d "$(REPOS_DIR)/$(@F:.t=)" ] || { \
		. $@ && \
		git clone --depth=1 --revision=$$sha $$url $(REPOS_DIR)/$(@F:.t=) && \
		cd $(REPOS_DIR)/$(@F:.t=) && \
		smlpkg sync && \
		git apply ../../$(@F:.t=.patch); \
	} > /dev/null
	$(MAKE) -sj1 -C $(REPOS_DIR)/$(@F:.t=) MLCOMP="$(POLYMLB) -q" clean all

clean:
	rm -rf $(REPOS_DIR)

.PHONY: $(SML) $(REPOS) all clean
