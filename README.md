# MMark CLI

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/mmark-cli.svg?style=flat)](https://hackage.haskell.org/package/mmark-cli)
[![Stackage Nightly](http://stackage.org/package/mmark-cli/badge/nightly)](http://stackage.org/nightly/package/mmark-cli)
[![Stackage LTS](http://stackage.org/package/mmark-cli/badge/lts)](http://stackage.org/lts/package/mmark-cli)
[![CI](https://github.com/mmark-md/mmark-cli/actions/workflows/ci.yaml/badge.svg)](https://github.com/mmark-md/mmark-cli/actions/workflows/ci.yaml)

* [Templates](#templates)
* [Extensions](#extensions)
  * [Comment paragraph](#comment-paragraph)
  * [Emoji](#emoji)
  * [Footnotes](#footnotes)
  * [Kbd tags](#kbd-tags)
  * [Lazy images](#lazy-images)
  * [Line highlighting](#line-highlighting)
  * [Link targets](#link-targets)
  * [MathJax](#mathjax)
  * [Mermaid](#mermaid)
  * [Permalinks](#permalinks)
  * [Punctuation prettifier](#punctuation-prettifier)
  * [GHC syntax highlighter](#ghc-syntax-highlighter)
  * [Skylighting](#skylighting)
  * [Table of contents](#table-of-contents)
* [Contribution](#contribution)
* [License](#license)

This is a command line application serving as an interface to the MMark
markdown processor.

```
mmark—command line interface to the MMark markdown processor

Usage: mmark [-v|--version] [-i|--ifile IFILE] [-o|--ofile OFILE] [-j|--json]
             [-t|--template FILE] [--ext-comment PREFIX] [--ext-emoji]
             [--ext-footnotes] [--ext-kbd] [--ext-lazy-images]
             [--ext-line-highlight] [--ext-link-target] [--ext-mathjax]
             [--ext-mermaid] [--ext-permalinks] [--ext-punctuation]
             [--ext-ghc-highlighter] [--ext-skylighting] [--ext-toc RANGE]

  Command line interface to the MMark markdown processor

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
  --ext-emoji              Replace :shortcode: with the emoji it names
  --ext-footnotes          Enable support for footnotes
  --ext-kbd                Enable support for wrapping things in kbd tags
  --ext-lazy-images        Let the browser decide when to fetch each image
  --ext-line-highlight     Point at the lines of a code block named by its info
                           string, e.g. "haskell {2,4-6}"
  --ext-link-target        Enable support for specifying link targets
  --ext-mathjax            Enable support for MathJax formulas
  --ext-mermaid            Render mermaid code blocks as diagrams in the browser
  --ext-permalinks         Append a link to its own id to every heading
  --ext-punctuation        Enable punctuation prettifier
  --ext-ghc-highlighter    Enable GHC syntax highlighter for Haskell code
  --ext-skylighting        Enable syntax highlighting of code snippets with
                           Skylighting
  --ext-toc RANGE          Enable generation of table of contents using the
                           supplied range of headers to include, e.g. "1-6" or
                           "2-4"
```

An extension may find something in the document that it cannot make sense
of, such as a footnote reference that leads nowhere. When that happens it is
reported the same way a parse error is, against the source of the document,
and nothing is written to the output.

## Templates

By using the `--template` argument, it's possible to create a standalone
HTML page. The templating system we use is
[Mustache](https://mustache.github.io/mustache.5.html), as implemented by
the [stache](https://hackage.haskell.org/package/stache) library. The
library conforms to version 1.1.3 of the official [Mustache
specification](https://github.com/mustache/spec), but does not implement
lambdas (which is an optional feature of the specification) for simplicity
and other technical reasons we won't touch here.

If the markdown source file has a YAML section, its contents will be
provided as context for rendering of the template. In addition to that, a
new top-level value bound to the variable named `output` will be available.
That variable contains the HTML rendition of the markdown document. It's
best to interpolate it without HTML escaping, like so: `{{& output }}`.

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

### Emoji

* Option: `--ext-emoji`

Replace every `:shortcode:` with the emoji it names:

```
$ mmark --ext-emoji
Ship it :rocket: and :tada:.
----------------------- Control-D
<p>Ship it 🚀 and 🎉.</p>
```

A shortcode is a run of letters, digits, `_`, `+`, and `-` between colons. A
name that is not one of the recognized ones is reported as an error rather
than left alone, on the grounds that it is far more likely to be a typo than
something you meant to keep:

```
$ mmark --ext-emoji
Ship it :nosuchthing:.
----------------------- Control-D
<stdin>:1:1:
  |
1 | Ship it :nosuchthing:.
  | ^
there is no emoji called "nosuchthing"
```

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
>
> 1. Here we have the footnote.
----------------------- Control-D
<p>Here goes some text <a href="#fn1" id="fnref1"><sup>1</sup></a>.</p>
<ol>
<li id="fn1">
Here we have the footnote.
<a href="#fnref1">↩</a></li>
</ol>
```

The footnotes of a document are also checked, so a reference that leads
nowhere, a footnote that nothing refers to, a footnote that is defined
twice, and more than one footnote section are all reported as errors:

```
$ mmark --ext-footnotes
Here goes some text [1](footnote:1).
----------------------- Control-D
<stdin>:1:21:
  |
1 | Here goes some text [1](footnote:1).
  |                     ^
there is no footnote 1
```

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

### Lazy images

* Option: `--ext-lazy-images`

Give every image `loading="lazy"` and `decoding="async"`, so that an image
far down the page does not hold up the ones the reader can already see:

```
$ mmark --ext-lazy-images
![a cat](cat.png)
----------------------- Control-D
<p><img loading="lazy" decoding="async" alt="a cat" src="cat.png"></p>
```

### Line highlighting

* Option: `--ext-line-highlight`

Point at the lines of a code block that the prose is about by naming them
after the language in the info string. The lines are given the class
`"highlighted-line"`, and the language, if there is one, still becomes the
`language-` class of the `<code>` element:

````
$ mmark --ext-line-highlight
```elixir {2}
alpha
beta
gamma
```
----------------------- Control-D
<pre><code class="language-elixir">alpha
<span class="highlighted-line">beta
</span>gamma
</code></pre>
````

The specification is a comma-separated list of line numbers and ranges, so
`{2,4-6}` points at lines 2, 4, 5, and 6.

Both syntax highlighters read the same specification and point at the lines
themselves, around the tokens they have coloured. This extension is what
renders the blocks they do not take, so combining it with one of them
highlights the languages that highlighter knows and still points at the
lines in every other block:

````
$ mmark --ext-skylighting --ext-line-highlight
```haskell {2}
main :: IO ()
main = return ()
```
----------------------- Control-D
<div class="source-code"><pre><code class="language-haskell"><span class="ot">main ::</span><span> </span><span class="dt">IO</span><span> ()</span>
<span class="highlighted-line"><span>main </span><span class="ot">=</span><span> </span><span class="fu">return</span><span> ()</span>
</span></code></pre></div>
````

### Link targets

* Option: `--ext-link-target`

When the title of a link starts with the word `"_blank"`, `"_self"`,
`"_parent"`, or `"_top"`, it's stripped from the title (as well as all
whitespace after it) and added as the value of the `target` attribute of the
resulting link. For example:

```
$ mmark --ext-link-target
This [link](/url '_blank My title') opens in new tab.
----------------------- Control-D
<p>This <a target="_blank" rel="noopener noreferrer" href="/url" title="My title">link</a>
opens in new tab.</p>
```

### MathJax

* Option: `--ext-mathjax`

The extension allows us to transform inline code spans into MathJax inline
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

### Mermaid

* Option: `--ext-mermaid`

Render a `mermaid` code block as `<pre class="mermaid">`, which is what the
[mermaid](https://mermaid.js.org/) script in the page looks for when it
turns the diagram into an SVG in the browser:

````
$ mmark --ext-mermaid
A diagram:

```mermaid
graph TD;
  A-->B;
```
----------------------- Control-D
<p>A diagram:</p>
<pre class="mermaid">graph TD;
  A--&gt;B;
</pre>
````

You will need to include the mermaid script for anything to happen; this
extension only marks the block for it. Rendering diagrams ahead of time
instead requires the mermaid command line tool, which this program does not
call — use the `mermaidScanner` and `mermaidSvg` functions of `mmark-ext`
for that.

### Permalinks

* Option: `--ext-permalinks`

Append to every heading a link to the id MMark gives that heading. The link
is labelled `"#"` and given the class `"permalink"`, so that a style sheet
can show it only when the heading is hovered:

```
$ mmark --ext-permalinks
# Story of my life
----------------------- Control-D
<h1 id="story-of-my-life">Story of my life<a href="#story-of-my-life" class="permalink" aria-hidden="true" tabindex="-1">#</a></h1>
```

The link says nothing a screen reader can use, so it is hidden from one and
taken out of the order the keyboard walks.

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
$ mmark --ext-punctuation
Something---we don't know what, happened...
----------------------- Control-D
<p>Something—we don’t know what, happened…</p>
```

### GHC syntax highlighter

* Option: `--ext-ghc-highlighter`

Use the [GHC syntax
highlighter](https://hackage.haskell.org/package/ghc-syntax-highlighter)
package to highlight code blocks with `"haskell"` info string using lexer of
GHC itself.

The resulting HTML will be rendered as described
[here](https://hackage.haskell.org/package/mmark-ext/docs/Text-MMark-Extension-GhcSyntaxHighlighter.html#v:ghcSyntaxHighlighter).

Example:

````
$ mmark --ext-ghc-highlighter
Some Haskell:

```haskell
main :: IO ()
main = return ()
```
----------------------- Control-D
<p>Some Haskell:</p>
<div class="source-code"><pre><code class="language-haskell"><span class="va">main</span><span> </span><span class="sy">::</span><span> </span><span class="cr">IO</span><span> </span><span class="sy">(</span><span class="sy">)</span><span>
</span><span class="va">main</span><span> </span><span class="sy">=</span><span> </span><span class="va">return</span><span> </span><span class="sy">(</span><span class="sy">)</span><span>
</span></code></pre></div>
````

### Skylighting

* Option: `--ext-skylighting`

Use the [skylighting](https://hackage.haskell.org/package/skylighting)
package to render code blocks with info strings that result in a successful
lookup from the syntax table that comes with the library.

The resulting HTML will be rendered as described
[here](https://hackage.haskell.org/package/mmark-ext/docs/Text-MMark-Extension-Skylighting.html#v:skylighting).

Example:

````
$ mmark --ext-skylighting
Some Haskell:

```haskell
main :: IO ()
main = return ()
```
----------------------- Control-D
<p>Some Haskell:</p>
<div class="source-code"><pre><code class="language-haskell"><span class="ot">main ::</span><span> </span><span class="dt">IO</span><span> ()</span>
<span>main </span><span class="ot">=</span><span> </span><span class="fu">return</span><span> ()</span>
</code></pre></div>
````

### Table of contents

* Option: `--ext-toc RANGE`

Replace the code block with info string `"toc"` by a table of contents
assembled from headings with levels from `N` to `M`, where `N-M` is `RANGE`.
A document that asks for a table of contents but has no headings to put in
one is reported as an error at the code block that asks.

The table of contents is inserted before the other extensions run, so
`--ext-punctuation` prettifies the headings in the table just as it does the
headings in the document.

For example:

````
$ mmark --ext-toc 2-4
# Story of my life

```toc
```

## Chapter 1

Foo.

## Chapter 2

Bar.

### Something

Baz.
----------------------- Control-D
<h1 id="story-of-my-life">Story of my life</h1>
<ul>
<li>
<a href="#chapter-1">Chapter 1</a>
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
<h2 id="chapter-1">Chapter 1</h2>
<p>Foo.</p>
<h2 id="chapter-2">Chapter 2</h2>
<p>Bar.</p>
<h3 id="something">Something</h3>
<p>Baz.</p>
````

## Contribution

Issues, bugs, and questions may be reported in [the GitHub issue tracker for
this project](https://github.com/mmark-md/mmark-cli/issues).

Pull requests are also welcome.

## License

Copyright © 2018–present Mark Karpov

Distributed under the BSD 3-clause license.
