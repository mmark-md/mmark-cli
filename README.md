# MMark CLI

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/mmark-cli.svg?style=flat)](https://hackage.haskell.org/package/mmark-cli)
[![Stackage Nightly](http://stackage.org/package/mmark-cli/badge/nightly)](http://stackage.org/nightly/package/mmark-cli)
[![Stackage LTS](http://stackage.org/package/mmark-cli/badge/lts)](http://stackage.org/lts/package/mmark-cli)
[![Build Status](https://travis-ci.org/mmark-md/mmark-cli.svg?branch=master)](https://travis-ci.org/mmark-md/mmark-cli)

* [Templates](#templates)
* [Extensions](#extensions)
  * [Comment paragraph](#comment-paragraph)
  * [Font Awesome](#font-awesome)
  * [Footnotes](#footnotes)
  * [Kbd tags](#kbd-tags)
  * [Link targets](#link-targets)
  * [MathJax](#mathjax)
  * [Email obfuscation](#email-obfuscation)
  * [Punctuation prettifier](#punctuation-prettifier)
  * [Skylighting](#skylighting)
  * [Table of contents](#table-of-contents)
* [Contribution](#contribution)
* [License](#license)

This is a command line application serving as an interface to the MMark
markdown processor.

```
mmark—command line interface to MMark markdown processor

Usage: mmark [-v|--version] [-i|--ifile IFILE] [-o|--ofile OFILE] [-j|--json]
             [-t|--template FILE] [--ext-comment PREFIX] [--ext-font-awesome]
             [--ext-footnotes] [--ext-kbd] [--ext-link-target] [--ext-mathjax]
             [--ext-obfuscate-email CLASS] [--ext-punctuation]
             [--ext-skylighting] [--ext-toc RANGE]
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
  --ext-footnotes          Enable support for footnotes
  --ext-kbd                Enable support for wrapping things in kbd tags
  --ext-link-target        Enable support for specifying link targets
  --ext-mathjax            Enable support for MathJax formulas
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

## Templates

By using the `--template` argument, it's possible to create a standalone
HTML page. The templating system we use is
[Mustache](https://mustache.github.io/mustache.5.html), as implemented by
the [stache](https://hackage.haskell.org/package/stache) library. The
library conforms to the version 1.1.3 of the official [Mustache
specification](https://github.com/mustache/spec), but does not implement
lambdas (which is an optional feature is the specification) for simplify and
other technical reasons we won't touch here.

If markdown source file has a YAML section, its contents will be provided as
context for rendering of the template. In addition to that, a new top-level
value bound to the variable named `output` will be available. That variable
contains the HTML rendition of markdown document. It's best to interpolate
it without HTML escaping, like so: `{{& output }}`.

## Extensions

Here we list how to use the available extensions. The extensions come from
the [`mmark-ext`](https://hackage.haskell.org/package/mmark-ext) package.

### Comment paragraph

* Option: `--ext-comment PREFIX`

This extension removes paragraphs that start with the given `PREFIX`. For
example:

```
$ mmark --ext-comment REM
First.

REM Second.

Third.
----------------------- Control-D
<p>First.</p>
<p>Third.</p>
```

### Font awesome

* Option: `--ext-font-awesome`

This allows to turn autolinks with `fa` scheme into font awesome icons:

```
$ mmark --ext-font-awesome
Here is the user icon: <fa:user>.

A more interesting example: <fa:quote-left/3x/pull-left/border>.
----------------------- Control-D
<p>Here is the user icon: <span class="fa fa-user"></span>.</p>
<p>A more interesting example:
  <span class="fa fa-quote-left fa-3x fa-pull-left fa-border"></span>.
</p>
```

In general, all path components in URIs that go after the name of icon will
be prefixed with `"fa-"` and added as classes, so you can do a lot of fancy
stuff, see http://fontawesome.io/examples/.

### Footnotes

* Option: `--ext-footnotes`

The extension performs two transformations:

* It turns links with URIs with `footnote` scheme and single path piece
  consisting of a number into links to footnote references.
* It turns block quotes with the `"footnotes"` label (see the example below)
  into a footnote section.

```
$ mmark --ext-footnotes
Here goes some text [1](footnote:1).

> footnotes

  1. Here we have the footnote.
----------------------- Control-D
<p>Here goes some text <a href="#fn1" id="fnref1"><sup>1</sup></a>.</p>
<ol>
<li id="fn1">
Here we have the footnote.
<a href="#fnref1">↩</a></li>
</ol>
```

The extension is not fully safe though in the sense that we can't check that
a footnote reference refers to an existing footnote and that footnotes have
corresponding references, or that they are present in the document in the
right order.

### Kbd tags

* Option: `--ext-kbd`

Introduce kbd tags into resulting HTML document by wrapping content in links
with URL with `kbd` scheme. For example:

```
$ mmark --ext-kbd
To enable that mode press [Ctrl+A][kbd].

[kbd]: kbd:
----------------------- Control-D
<p>To enable that mode press <kbd>Ctrl+A</kbd>.</p>
```

The use of reference-style links seems more aesthetically pleasant to the
author, but you can of course do something like this instead:

```
To enable that mode press [Ctrl+A](kbd:).
```

### Link targets

* Option: `--ext-link-target`

When title of a link starts with the word `"_blank"`, `"_self"`,
`"_parent"`, or `"_top"`, it's stripped from title (as well as all
whitespace after it) and added as the value of `target` attribute of the
resulting link. For example:

```
$ mmark --ext-kbd
This [link](/url '_blank My title') opens in new tab.
----------------------- Control-D
<p>This <a href="/url" title="My title" target="_blank">link</a>
opens in new tab.</p>
```

### MathJax

* Option: `--ext-mathjax`

The extension allows to transform inline code spans into MathJax inline
spans and code blocks with the info string `"mathjax"` (case-sensitive) into
MathJax display spans. Every line in such a code block will produce a
separate display span, i.e. a separate line with a formula (which is
probably what you want anyway).

Inline code spans must start and end with the dollar sign `$` to be
recognized as MathJax markup:

````
$ mmark --ext-mathjax
Let's talk about `$A$` and `$B$`.

```mathjax
A \xrightarrow{f} B
```
----------------------- Control-D
<p>Let&#39;s talk about
  <span class="math inline">\(A\)</span> and
  <span class="math inline">\(B\)</span>.
</p>
<p>
  <span class="math display">\[A \xrightarrow{f} B\]</span>
</p>
````

### Email obfuscation

* Option: `--obfuscate-email CLASS`

This extension makes email addresses in autolinks be rendered as something
like this:

```
[mark@arch ~]$ mmark --ext-obfuscate-email protected-email
Send all your spam to <someone@example.org>, if you can!
----------------------- Control-D
<p>Send all your spam to
  <a href="javascript:void(0)"
     class="protected-email"
     data-email="someone@example.org">
  Enable JavaScript to see this email</a>, if you can!
</p>
```

You'll also need to include jQuery and this bit of JS code for the magic to
work:

```java-script
$(document).ready(function () {
    $(".protected-email").each(function () {
        var item = $(this);
        var email = item.data('email');
        item.attr('href', 'mailto:' + email);
        item.html(email);
    });
});
```

### Punctuation prettifier

* Option: `--ext-punctuation`

This makes MMark prettify punctuation (only affects plain text in inlines),
the effect is the following:

* Replace `...` with ellipsis `…`
* Replace `---` with em-dash `—`
* Replace `--` with en-dash `–`
* Replace `"` with left double quote `“` when previous character was a space
  character, otherwise replace it with right double quote `”`
* Replace `'` with left single quote `‘` when previous character was a space
  character, otherwise replace it with right single quote `’` aka apostrophe

For example (not sure if this is the correct punctuation to use here, but it
demonstrates the effect):

```
[mark@arch ~]$ mmark --ext-punctuation
Something---we don't know what, happened...
----------------------- Control-D
<p>Something—we don’t know what, happened…</p>
```

### Skylighting

* Option: `--ext-skylighting`

Use the [skylighting](https://hackage.haskell.org/package/skylighting)
package to render code blocks with info strings that result in a successful
lookup from the syntax table that comes with the library.

The resulting HTML will be rendered as described
[here](https://hackage.haskell.org/package/mmark-ext-0.2.0.0/docs/Text-MMark-Extension-Skylighting.html#v:skylighting).

Example:

````
[mark@arch ~]$ mmark --ext-skylighting
Some Haskell:

```haskell
main :: IO ()
main = return ()
```
----------------------- Control-D
<p>Some Haskell:</p>
<div class="source-code"><pre><code class="language-haskell">
<span class="ot">main ::</span><span> </span><span class="dt">IO</span><span> ()</span>
<span>main </span><span class="fu">=</span><span> return ()</span>
</code></pre></div>
````

### Table of contents

* Option: `--ext-toc RANGE`

Replace the code block with info string `"toc"` by table of contents
assembled from headings with levels from `N` to `M`, where `N-M` is `RANGE`.

For example:

````
[mark@arch ~]$ mmark --ext-toc 2-4
# Story of my life

```toc
```

## Charpter 1

Foo.

## Chapter 2

Bar.

### Something

Baz.
----------------------- Control-D
<h1 id="story-of-my-life">Story of my life</h1>
<ul>
<li>
<a href="#charpter-1">Charpter 1</a>
</li>
<li>
<a href="#chapter-2">Chapter 2</a>
<ul>
<li>
<a href="#something">Something</a>
</li>
</ul>
</li>
</ul>
<h2 id="charpter-1">Charpter 1</h2>
<p>Foo.</p>
<h2 id="chapter-2">Chapter 2</h2>
<p>Bar.</p>
<h3 id="something">Something</h3>
<p>Baz.</p>
````

## Contribution

Issues, bugs, and questions may be reported in [the GitHub issue tracker for
this project](https://github.com/mmark-md/mmark-cli/issues).

Pull requests are also welcome and will be reviewed quickly.

## License

Copyright © 2018 Mark Karpov

Distributed under BSD 3 clause license.
