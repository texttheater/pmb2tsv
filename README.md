pmb2tsv
=======

pmb2tsv is a collection of scripts to convert data from the [Parallel Meaning
Bank](https://pmb.let.rug.nl) (PMB) into column-based (`.tsv`) files. It also
extracts the raw and tokenized data in the form of `.tok.iob` files and the
DRSs in the form of `.drs.clf` files.

Input Data
----------

Please download the [PMB](https://pmb.let.rug.nl) 3.0.0/4.0.0 and extract the
directory `pmb-3.0.0`/`pmb-4.0.0` into a `data` directory in the root directory
of this repository (or symlink it).

Software Dependencies
---------------------

Scripts to convert the files are mostly found in this repository; however, the
following software needs to be present on the system:

* [Python 3](https://www.python.org) – the `python3` executable should be on
  your `$PATH`.
* [Produce](https://github.com/texttheater/produce) – the `produce` executable
  should be on your `$PATH`.
* [SWI-Prolog](https://www.swi-prolog.org) 7 or higher – the `swipl` executable
  should be on your `$PATH`. Tested with version 8.4.2.
* [GNU Parallel](https://www.gnu.org/software/parallel/) – the `parallel`
  executable should be on your `$PATH`.

Outputs
-------

The data is extracted in four different formats:

* `.tok.iob`: Tokenization information in character-level BIO format
* `.parse.tags`: CCG derivations with tags in Prolog syntax
* `.drs.clf`: DRSs in clause format

Individual per-token annotation layers are extracted to files with these
extensions:

* `.toknum` (token number)
* `.tok` (token form)
* `.lemma` (lemma)
* `.sem` (semantic tag)
* `.wordnet` (WordNet sense)
* `.dep` (dependency head)
* `.frag` (DRS fragment)

All of these are integrated into TSV files (the columns are in the above
order):

* `.tsv`

The output file names before the extension contain the following information,
indicating which data is in each file:

* PMB version (`3.0.0` or `4.0.0`)
* language (`en`, `de`, `it`, or `nl`)
* status (`gold`, `silver`, or `bronze`)
* part (`p00`, `p01`, ..., or `p99`) or portion (`train`, `dev`, `test`, or
  `eval`). The latter are concatenated together from the former according to
  the division in the PMB README (different for 3.0.0 and 4.0.0).

Usage
-----

Use the `produce` command to produce the file(s) with the information you need.
For example, to extract the `.drs.clf` file for the PMB 4.0.0 English gold part
00:

    produce out/pmb-4.0.0-en-gold-p00.drs.clf

To produce train, dev, and test data in TSV format for all languages:

    produce out/pmb-4.0.0-{en,de,it,nl}-gold-{train,dev,test}.tsv

There are also shorthands to extract all data for a given version and status.
For example:

    produce gold-4.0.0
    produce silver-4.0.0
    produce bronze-4.0.0

Limitations
-----------

For a small number of CCG derivations, especially some that are not fully
corrected, dependency extraction will fail. The corresponding columns will be
empty/missing. In extremely rare cases a dependency non-tree (a cyclic graph)
may be extracted.

Publications and Experiments
----------------------------

For details on the conversion from CCG derivations to dependency trees, see

    Kilian Evang (2020): Configurable Dependency Tree Extraction from CCG
    Derivations. Proceedings of the Universal Dependencies Workshop.

To reproduce the experiments from that paper, checkout out the
`evang-2020-configurable` tag and run:

    produce pmb-3.0.0-{en,de,it,nl}-gold-{p00,p01}.eval
