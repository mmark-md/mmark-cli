# MMark CLI

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/mmark-cli.svg?style=flat)](https://hackage.haskell.org/package/mmark-cli)
[![Stackage Nightly](http://stackage.org/package/mmark-cli/badge/nightly)](http://stackage.org/nightly/package/mmark-cli)
[![Stackage LTS](http://stackage.org/package/mmark-cli/badge/lts)](http://stackage.org/lts/package/mmark-cli)
[![Build Status](https://travis-ci.org/mmark-md/mmark-cli.svg?branch=master)](https://travis-ci.org/mmark-md/mmark-cli)

This is a command line application serving as an interface to MMark markdown
processor.

```
mmark—command line interface to MMark markdown processor

Usage: mmark [-v|--version] [-i|--ifile IFILE] [-o|--ofile OFILE] [-j|--json]
             [-t|--template FILE] [--ext-comment PREFIX] [--ext-font-awesome]
             [--ext-kbd] [--ext-link-target] [--ext-obfuscate-email CLASS]
             [--ext-punctuation] [--ext-skylighting] [--ext-toc RANGE]
  Command line interface to MMark markdown processor

Available options:
  -h,--help                Show this help text
  -v,--version             Print version of the program
  -i,--ifile IFILE         Read markdown source from this file (otherwise read
                           from stdin)
  -o,--ofile OFILE         Save rendered HTML document to this file (otherwise
                           write to stdout)
  -j,--json                Output parse errors and result in JSON format
  -t,--template FILE       Use the template located at this path
  --ext-comment PREFIX     Remove paragraphs that start with the given prefix
  --ext-font-awesome       Enable support for inserting font awesome icons
  --ext-kbd                Enable support for wrapping things in kbd tags
  --ext-link-target        Enable support for specifying link targets
  --ext-obfuscate-email CLASS
                           Obfuscate email addresses assigning the specified
                           class
  --ext-punctuation        Enable punctuation prettifier
  --ext-skylighting        Enable syntax highlighting of code snippets with
                           Skylighting
  --ext-toc RANGE          Enable generation of table of contents using the
                           supplied range of headers to include, e.g. "1-6" or
                           "2-4"
```

## Contribution

Issues, bugs, and questions may be reported in [the GitHub issue tracker for
this project](https://github.com/mmark-md/mmark-cli/issues).

Pull requests are also welcome and will be reviewed quickly.

## License

Copyright © 2018 Mark Karpov

Distributed under BSD 3 clause license.
