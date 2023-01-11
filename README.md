pmb2tsv
=======

pmb2tsv is a collection of scripts to convert data from the [Parallel Meaning
Bank](https://pmb.let.rug.nl) (PMB) into column-based files.

Input Data
----------

Please download the [PMB](https://pmb.let.rug.nl) 3.0.0 and extract the
directory `pmb-3.0.0` into a `data` directory in the root directory of this
repository (or symlink it).

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

Now use the `produce` command to convert the desired portions of the PMB to TSV
files. For example, to get all training data, run:

    produce out/pmb-3.0.0-{en,de}-{bronze,silver,gold}-train.tsv
    produce out/pmb-3.0.0-{it,nl}-{bronze,silver}-train.tsv

And to get all gold development data, run:

    produce out/pmb-3.0.0-{en,de,it,nl}-gold-dev.tsv

This example will generate 8 TSV files, one per language and part. They contain
the converted sentences, separated by empty lines, one token per line with the
following tab-separated columns:

1. Token number within sentence
2. Token form
3. Lemma
4. Semantic tag
3. WordNet sense
4. CCG dependency head
5. DRS fragment

Warning: for a small number of CCG derivations, especially some that are not
fully corrected, dependency and role extraction will fail. The corresponding
columns will be empty/missing. In extremely rare cases a dependency non-tree (a
cyclic graph) may be extracted.

For details on the conversion from CCG derivations to dependency trees, see

    Kilian Evang (2020): Configurable Dependency Tree Extraction from CCG
    Derivations. Proceedings of the Universal Dependencies Workshop.

To reproduce the experiments from that paper, checkout out the
`evang-2020-configurable` tag and run:

    produce pmb-3.0.0-{en,de,it,nl}-gold-{p00,p01}.eval
