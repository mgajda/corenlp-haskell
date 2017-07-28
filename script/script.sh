#!/bin/bash
CORENLP=~/Downloads/stanford-corenlp-full-2017-06-09

$CORENLP/corenlp.sh -annotators tokenize,cleanxml,ssplit,pos,ner,depparse,lemma,mention,coref,natlog,quote -ner.useSUTime false -ner.applyNumericClassifiers false $*
# -outputFormat conll -file inputs.txt -outputDirectory .
