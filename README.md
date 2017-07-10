# Stanford CoreNLP output parser and API binding

This is a Haskell library for working with Stanford CoreNLP.

## Planned features

It should allow parsing of Stanford CoreNLP output from file,
and use of Stanford CoreNLP in web service mode.

The API should allow for easy access to dependency parser annotations, along with co-references,
natural logic quantifiers, and parts of speech.

## Installation

[Stanford CoreNLP](https://stanfordnlp.github.io) is easiest to install as a download.
Of course, for CI integration it is preferred to use Maven installation.

It should be possible to exercise project on [TravisCI](http://travis-ci.org).

## Output examples

[Examples](examples/) contain:
* input as either `.txt`, or `.html` file,
* output in `.conll` format similar SyntaxNet,
* output in Stanford CoreNLP native `.xml` serialization,
* output in Stanford CoreNLP web server targetted `.json` serialization.
