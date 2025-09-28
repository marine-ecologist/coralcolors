# coralcolors <img src="../man/figures/logo.png" style="float:right; height:125px; margin-left:20px;"/>

The `coralcolors` package provides color scales for plotting in R based
on nature’s most stunning and colorful organisms: scleractinian corals.

\#TeamCoral in its colorful glory.

## Installation

``` r
library(devtools)
devtools::install_github("marineecologist/coralcolors", force = TRUE)
```

### coralcolor splcales

To view the current list of coral taxa, use:

``` r
library(coralcolors)

names(coral_palettes)
```

    ##  [1] "acanthastrea"    "acropora1"       "acropora2"       "acropora3"       "acropora4"       "australophyllia" "cyphastrea"     
    ##  [8] "montipora"       "micromussa"      "porites"         "seriatopora"     "symbiodinium"

Use `show_colors()` to view color palettes:

``` r
show_colors(coral_palettes$acropora2)
```

![](/Users/rof011/coralcolors/pkgdown/index_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
