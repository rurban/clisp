#!/bin/sh

comment5='/usr2/cvs/clisp-conversion/src/comment5'
joincomment='perl ~/javamonkey/clisp-d-conversion/utils/join-c-comments.pl'

for f in *.d; do
    cp $f $f.ORIG;
    $comment5 $f | $joincomment > $f.TMP;
    mv $f.TMP $f;
done
