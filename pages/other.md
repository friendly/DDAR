---
output: 
  html_fragment:
    self_contained: false
    smart: false
---

<!-- generator: rmarkdown::render("other.md") -->

<div class="contents">
## Updates

Updates to the code examples in the book will be posted here.

* __`ca`__ package: There are now several enhancements for graphics for correspondence analysis (Chapter 6)
  in the `ca` package, v. 0.64.  All examples in the book still work, but some can be simplified.

* __`ggplot2`__ changes: In the latest major release, `ggplot2_2.0.0`, calls to `stat_smooth(method="glm")` now
  require the `family` argument to be specified as, for example, `method.args = list(family = binomial)`
  rather than `family = binomial`.
  This affects numerous figures in Chapter 7, starting with Figure 7.2 in Example 7.3.

## Errata

Readers are invited to send an email related to typos or errors in the book to
`friendly AT yorku DOT ca`.  Please give specific section, page, figure or equation
references, and use `DDAR: errata` in the subject line.

* [Errata for the book, Jan. 11, 2016](extra/errata.pdf)

## Additional vignettes and case studies

Several examples and topics did not make it into the printed book

* [Visualizing GLMs for binary outcomes](extra/titanic-glm-ex.pdf)
* [Classification and regression trees](extra/titanic-tree-ex.pdf)


## Reviews

>This is an excellent book, nearly encyclopedic in its coverage. I personally find it very useful and expect that many other readers will as well. The book can certainly serve as a reference. It could also serve as a supplementary text in a course on categorical data analysis that uses R for computation or, because so much statistical detail is provided, even as the main text for a course on the topic that emphasizes graphical methods. ___John Fox, McMaster University___

## Citations

If you use this book or the materials contained on this web site in research papers, you can cite that use as follows
with BibTeX:

```
@Book{FriendlyMeyer:2016:DDAR,
   title	= {Discrete Data Analysis with R: Visualization and Modeling Techniques for Categorical and Count Data},
   year		= {2016},
   author	= {Friendly, Michael and Meyer, David},
   publisher	= {Chapman \& Hall/CRC},
   address	= {Boca Raton, FL},
   isbn		= {978-1-4987-2583-5},
}

```
A text version of this citation is:

Friendly, M. & Meyer, D. (2016). *Discrete Data Analysis with R: Visualization and Modeling Techniques for Categorical and Count Data*. Boca Raton, FL: Chapman & Hall/CRC.
</div>
