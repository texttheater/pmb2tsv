pmb2tsv
=======

pmb2tsv is a collection of scripts to convert data from the [Parallel Meaning
Bank](https://pmb.let.rug.nl) (PMB) into column-based files including CCG
supertags, dependency structure, constituent structure, semantic tags, and
semantic roles.

The primary target audience is people wanting to do semantic role labeling
(SRL) experiments on the PMB.

**Note**: `pmb2tsv` is experimental and some of its output may be erroneous.

Input Data
----------

Please download the [PMB](https://pmb.let.rug.nl) 3.0.0 and extract the
directory `pmb-3.0.0` into the root directory of this repository.

Software Dependencies
---------------------

Scripts to convert the files are mostly found in this repository; however, the
following software needs to be present on the system:

* [Python 3](https://www.python.org) – the `python3` executable should be on
  your `$PATH`.
* [Produce](https://github.com/texttheater/produce) – the `produce` executable
  should be on your `$PATH`.
* [SWI-Prolog](https://www.swi-prolog.org) 7 or higher – the `swipl` executable
  should be on your `$PATH`.
* [GNU Parallel](https://www.gnu.org/software/parallel/) – the `parallel`
  executable should be on your `$PATH`.

Conversion
----------

If everything is in place, run the following command:

    produce

This creates files named `pmb-3.0.0-{en,de,it,nl}-{train,dev,test}-gold.tsv`.
They contain the converted sentences, separated by empty lines, one token per
line with the following tab-separated columns:

1. Token number within sentence
2. Token form
3. PMB semantic tag
4. Symbol (English lemma)
5. Dependency head token number or 0 if root
6. CCG supertag
7. CCG constituent structure

For every frame in the sentence, there is an additional column that marks each
token as being the head of the predicate (in which case it contains its WordNet
sense, e.g. `attack.v.01`, or `pred` if the word sense is not annotated), as
being part of the role filler (in which case it contains a VerbNet Role such as
`Agent` or `Patient`), or as neither (in which case it is `O`).

The conversion of CCG derivations to dependency trees tries to follow the Basic
[UD](https://universaldependencies.org) guidelines. For details, see

    Kilian Evang (2020): Configurable Dependency Tree Extraction from CCG
    Derivations. Proceedings of the Universal Dependencies Workshop.

To reproduce the experiments from that paper, run:

    produce pmb-3.0.0-{en,de,it,nl}-{p00,p01}-gold.eval
