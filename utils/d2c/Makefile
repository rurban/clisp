# Makefile for doing various steps of .d -> .c conversion.
# Probably only works with GNU make.

here           := $(shell pwd)
comment5       := $(here)/comment5
ansidecl       := $(here)/ansidecl
join-comment   := perl $(here)/join-c-comments.pl
utf8-to-latin  := perl $(here)/utf8-to-latin.pl
latin-to-ascii := perl $(here)/latin-to-ascii.pl
conversions    := src utils

all:
	@echo "Pick a phase."

clean:
	rm comment5 ansidecl ansidecl.c

convert: phase-0 phase-1 phase-2 phase-3 phase-4 phase-5 phase-omega

# Phase zero: small tweaks to some .d files to prepare them for
# subsequent conversion. These changes aren't actually required until
# phase-3 but need to be applied now before we start converting the
# source.

phase-0:
	cd ../..; patch -p1 < $(here)/phase-0-patch

undo-phase-0:
	cd ../..; patch -R -p1 < $(here)/phase-0-patch

# Phase One: get rid of the #-style comments and the infrastructure
# that supports them. And convert all .d files to 7-bit ascii.

phase-1: $(addprefix convert-,$(conversions))

apply-patch-1:
	cd ../..; patch -p1 < $(here)/phase-1-patch

convert-%: comment5 apply-patch-1
	cd ../../$*; \
	for f in *.d; do \
	  cp $$f $$f.ORIG; \
	  $(comment5) $$f | $(join-comment) | $(utf8-to-latin) | $(latin-to-ascii) > $$f.TMP; \
	  mv $$f.TMP $$f; \
	done

comment5: ../comment5.c
	gcc -o $@ $<

undo-phase-1:
	cd ../../utils; for f in *.d; do mv $$f.ORIG $$f; done
	cd ../../src; for f in *.d; do mv $$f.ORIG $$f; done
	cd ../..; patch -R -p1 < $(here)/phase-1-patch

# Phase Two: get rid of the idiosyncratic control-structure macros,
# elif, until, and loop.

phase-2: $(addprefix elif-until-and-loop-,$(conversions))

elif-until-and-loop-%:
	cd ../../$*; \
	for f in *.d; do \
	  cp $$f $$f.PHASE-1; \
	  perl $(here)/fix-elif-until.pl $$f.PHASE-1 > $$f; \
	done

undo-phase-2:
	cd ../../src; for f in *.d; do mv $$f.PHASE-1 $$f; done
	cd ../../utils; for f in *.d; do mv $$f.PHASE-1 $$f; done

# Phase Three: get rid of preprocessor macros global, local, and var.
# Requires a small patch to some .d files (done in phase-0) to put
# back some blocks that used to be inserted by varbrace.

phase-3: $(addprefix var-global-and-local-,$(conversions))

apply-patch-3:
	cd ../..; patch -p1 < $(here)/phase-3-patch

var-global-and-local-%: apply-patch-3
	cd ../../$*; \
	for f in *.d; do \
	  cp $$f $$f.PHASE-2; \
	  perl $(here)/fix-var-global-local.pl $$f.PHASE-2 > $$f; \
	done

undo-phase-3:
	cd ../../src; for f in *.d; do mv $$f.PHASE-2 $$f; done
	cd ../../utils; for f in *.d; do mv $$f.PHASE-2 $$f; done
	cd ../..; patch -R -p1 < $(here)/phase-3-patch


# Phase Four: run ansidecl to finally make .c files.

phase-4: ansidecl apply-patch-4
	cd ../../src; \
	for f in *.d; do \
	  cp $$f $$f.PHASE-3; \
	  cat $$f.PHASE-3 | $(ansidecl) > `basename $$f .d`.c; \
	  rm -f $$f; \
	done

apply-patch-4:
	cd ../../; patch -p1 < $(here)/phase-4-patch

ansidecl: ansidecl.c
	gcc -o $@ $<

ansidecl.c: ../ansidecl.d
	$(comment5) $< | $(join-comment) > $@



# We don't want to run the .c files that are really assembly code
# through indent because nothing good happens to 'em.

assembly := ari68000.c
assembly += ari68020.c
assembly += ari80386.c
assembly += ariarm.c
assembly += arihppa.c
assembly += arimips.c
assembly += arimips64.c
assembly += arisparc.c
assembly += arisparc64.c
assembly += sp68000.c
assembly += sp80386.c
assembly += sp80386.msvc.c
assembly += spmips.c
assembly += spsparc.c
assembly += spsparc64.c

empty   :=
space   := $(empty) # don't delete this comment
to-skip := $(subst $(space), | ,$(assembly))

phase-5:
	cd ../../src; \
	for f in *.d.ORIG; do \
	  name=`basename $$f .d.ORIG`; \
	  case $$name.c in \
	    $(to-skip) ) \
	      echo "Skipping $$name.c"; \
	      ;; \
	    *) \
	      cp $$name.c $$name.UNINDENTED; \
	      indent -kr -i2 -ppi2 $$name.c 2>$$name.indent-errors; \
	      ;; \
	  esac; \
	done; \
	find . -name '*.indent-errors' -a -size 0b -exec rm {} \; ; \
	for f in *.indent-errors; do \
	  name=`basename $$f .indent-errors`; \
	  mv $$name.UNINDENTED $$name.c; \
	done

# Eventually we need to convert the generated makefiles. But we don't
# need it during builds and it's slow so we don't do it all the time.
# This should be run after all the patches have been applied.

phase-omega: convert-makefiles

convert-makefiles:
	cd ../..; $(MAKE) -f Makefile.devel makefiles
