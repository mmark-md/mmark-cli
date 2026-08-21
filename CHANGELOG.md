## MMark 0.1.0.0

* The package now requires `mmark-0.1` and `mmark-ext-0.3` or later.

* `--ext-footnotes` now checks the footnotes of a document: a reference that
  leads nowhere, a footnote that nothing refers to, a footnote that is
  defined twice, and more than one footnote section are all reported as
  errors.

* `--ext-toc` reports a document that asks for a table of contents but has
  no headings to put in one. The table of contents is also inserted before
  the other extensions run now, so `--ext-punctuation` applies to it too.

* Added the following extensions, all new in `mmark-ext-0.3`:

    * `--ext-emoji`, which replaces `:shortcode:` with the emoji it names
      and reports a name it does not recognize.
    * `--ext-lazy-images`, which gives every image `loading="lazy"` and
      `decoding="async"`.
    * `--ext-line-highlight`, which points at the lines of a code block
      named by its info string, as in ```` ```haskell {2,4-6} ````. Both
      syntax highlighters read the same specification, so this renders the
      blocks they do not take.
    * `--ext-mermaid`, which renders a `mermaid` code block as `<pre
      class="mermaid">` for the mermaid script to pick up in the browser.
    * `--ext-permalinks`, which appends to every heading a link to its own
      id.

* Removed `--ext-font-awesome` and `--ext-obfuscate-email`, because the
  extensions behind them are gone from `mmark-ext-0.3`.

## MMark CLI 0.0.5.2

* Maintenance release with more minimal dependencies.

## MMark CLI 0.0.5.1

* Dropped support for GHC 8.6 and older.
* Works with aeson 2.

## MMark CLI 0.0.5.0

* Added support for the `ghcSyntaxHighlighter` extension. Only available
  when compiled with GHC >= 8.4.1.

## MMark CLI 0.0.4.0

* This version works with `mmark-0.0.6.0` and `megaparsec-7.0.0`.

## MMark CLI 0.0.3.0

* This version works with `mmark-ext-0.2.0.0` and later.

## MMark CLI 0.0.2.0

* Added two new extensions: footnotes (`--ext-footnotes`) and MathJax
  support (`--ext-mathjax`).

## MMark CLI 0.0.1.0

* Initial release.
