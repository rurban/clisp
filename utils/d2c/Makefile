# Makefile for doing various steps of .d -> .c conversion.
# Probably only works with GNU make.

here         := $(shell pwd)
comment5     := $(here)/comment5
join-comment := perl $(here)/join-c-comments.pl

conversions := makefiles src utils

all: apply-patch $(addprefix convert-,$(conversions))

clean:
	rm comment5

apply-patch:
	cd ../..; patch -p1 < $(here)/conversions-patch-phase-01

convert-%: comment5
	cd ../../$*; \
	for f in *.d; do \
	  cp $$f $$f.ORIG; \
	  $(comment5) $$f | $(join-comment) > $$f.TMP; \
	  mv $$f.TMP $$f; \
	done

convert-makefiles: apply-patch
	cd ../..; $(MAKE) -f Makefile.devel makefiles

comment5: ../comment5.c
	gcc -o $@ $<
