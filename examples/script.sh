#!/bin/bash
CORENLP=/Users/m/exp/corenlp/stanford-corenlp-full-2017-06-09

$CORENLP/corenlp.sh -annotators tokenize,cleanxml,ssplit,pos,lemma,ner,parse,dcoref,relation,natlog,quote $*
#-outputFormat conll -file inputs.txt -outputDirectory .
