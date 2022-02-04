Supplementary materials for a paper on the SPACE-TIME mapping in
Indonesian based on linguistic and gestural data
================
*by* [Gede Primahadi Wijaya
Rajeg](https://udayananetworking.unud.ac.id/lecturer/880-gede-primahadi-wijaya-rajeg)
<a itemprop="sameAs" content="https://orcid.org/0000-0002-2047-8621" href="https://orcid.org/0000-0002-2047-8621" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon"></a>,
[Poppy
Siahaan](https://orient.phil-fak.uni-koeln.de/en/personen/wissenschaftliche-mitarbeiterinnen/dr-poppy-siahaan),
& [Alice Gaby](https://research.monash.edu/en/persons/alice-gaby)
<a itemprop="sameAs" content="https://orcid.org/0000-0003-4637-5513" href="https://orcid.org/0000-0003-4637-5513" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon"></a>

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

<a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/"><img alt="Creative Commons Licence" style="border-width:0" src="https://i.creativecommons.org/l/by-nc/4.0/88x31.png" /></a><br />This
work is licensed under a
<a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/">Creative
Commons Attribution-NonCommercial 4.0 International License</a>.
[![](https://img.shields.io/badge/doi-10.17605/OSF.IO/FJE6K-green.svg)](https://doi.org/10.17605/OSF.IO/FJE6K)

<!-- badges: end -->

# Overview

This is an
[RStudio-project](https://r4ds.had.co.nz/workflow-projects.html)
repository ([Rajeg, Siahaan & Gaby 2021b](#ref-rajeg_data_2021)) hosting
the dataset and R codes used in the
[paper](http://ojs.linguistik-indonesia.org/index.php/linguistik_indonesia/article/view/297)
published open access in [*Linguistik
Indonesia*](http://ojs.linguistik-indonesia.org/index.php/linguistik_indonesia),
the official journal for [*Masyarakat Linguistik
Indonesia*](https://www.mlindonesia.org/) (MLI) (the Linguistic Society
of Indonesia). The paper reports on a [project on the spatio-temporal
metaphor in
Indonesian](https://udayananetworking.unud.ac.id/lecturer/research/880-gede-primahadi-wijaya-rajeg/spatial-construal-of-time-in-indonesian-language-and-co-speech-gestures-132)
based on investigating evidence from Indonesian language corpus and
spontaneous co-speech gestural data.

If the materials in this repository are used in your research and/or
teaching, please cite the paper and the repository as follows:

> How to cite the paper — Rajeg, Gede Primahadi Wijaya, Poppy Siahaan &
> Alice Gaby. 2022. The Spatial Construal of TIME in Indonesian:
> Evidence from Language and Gesture. *Linguistik Indonesia* 40(1).
> 1–24.
> [https://doi.org/10.26499/li.v40i1.297](http://ojs.linguistik-indonesia.org/index.php/linguistik_indonesia/article/view/297).

> How to cite this repository — Rajeg, Gede Primahadi Wijaya, Poppy
> Siahaan & Alice Gaby. 2021. Data and R codes for analyses on the
> spatial construal of TIME in Indonesian language and co-speech
> gestures. *Open Science Framework (OSF)*. OSF.
> <https://doi.org/10.17605/OSF.IO/FJE6K>. <https://osf.io/fje6k/>.

The earlier version of the paper has been presented at (i) the
[*15<sup>th</sup> International Cognitive Linguistics
Conference*](https://iclc2019.site/general-theme-session-presentations/#fri-07-03)
(ICLC-15) in August 2019 in Nishinomiya, Japan ([Rajeg, Siahaan & Gaby
2019](#ref-rajeg_linguistic_2019)), and (ii) *Kongres Internasional
Masyarakat Linguistik Indonesia 2021* (KIMLI 2021) ([Rajeg, Siahaan &
Gaby 2021a](#ref-rajeg_spatial_2021)). We thank the participants at
these conferences for their feedback and comments. We are also grateful
for the travel grant awarded by the [*International Cognitive
Linguistics Association*](https://www.cognitivelinguistics.org/en)
(ICLA) for our presentation at ICLC-15.

The early phase of the project was partly funded by Monash University,
Australia through the PhD research scholarships scheme awarded to Gede
Primahadi Wijaya Rajeg: (i) *Monash International Postgraduate Research
Scholarships (MIPRS)*, and (ii) *Monash Graduate Scholarships* (MGS).
The pre-processing stage of the linguistic data was supported by the
computing facility at Monash University, Australia via the [*MonARCH
High Performance Computing
Cluster*](https://docs.monarch.erc.monash.edu/MonARCH/aboutMonArch.html).

# Materials

-   The R codes for the quantitative analyses and data visualisations
    are contained in the
    [`R-scripts`](https://github.com/gederajeg/spatiotemporal-metaphor-indonesian/tree/main/R-scripts)
    directory.

-   The codes for the in-line computations of gestural, quantitative
    analyses within the body text are contained in the R Markdown
    Notebook file
    “[5-rnotebook-for-the-inline-text-code.Rmd](https://github.com/gederajeg/spatiotemporal-metaphor-indonesian/blob/main/5-rnotebook-for-the-inline-text-code.Rmd).”

-   The R codes also make use of functions from the
    [`tidyverse`](https://www.tidyverse.org/) family of R packages
    ([Wickham & Grolemund 2017](#ref-wickham_r_2017); [Wickham et al.
    2019](#ref-wickham_welcome_2019)), the
    [RColorBrewer](https://cran.r-project.org/web/packages/RColorBrewer/index.html)
    package ([Neuwirth 2014](#ref-brewer2014)), and
    [corplingr](https://gederajeg.github.io/corplingr/) ([Rajeg
    2021](#ref-corplingr)). Users are required to install these packages
    in order to run the codes.

-   The
    [`data`](https://github.com/gederajeg/spatiotemporal-metaphor-indonesian/tree/main/data)
    directory contains the main (pre-processed) linguistic data,
    annotation notes for the gestural data, and the derived tabular
    data.

-   The
    [`figures`](https://github.com/gederajeg/spatiotemporal-metaphor-indonesian/tree/main/figures)
    directory contains all statistical graphics used in the paper.

# R session info

``` r
devtools::session_info()
#> ─ Session info ───────────────────────────────────────────────────────────────
#>  setting  value
#>  version  R version 4.1.2 (2021-11-01)
#>  os       macOS Big Sur 10.16
#>  system   x86_64, darwin17.0
#>  ui       X11
#>  language (EN)
#>  collate  en_US.UTF-8
#>  ctype    en_US.UTF-8
#>  tz       Asia/Makassar
#>  date     2022-02-04
#>  pandoc   2.14.0.3 @ /Applications/RStudio.app/Contents/MacOS/pandoc/ (via rmarkdown)
#> 
#> ─ Packages ───────────────────────────────────────────────────────────────────
#>  package     * version date (UTC) lib source
#>  cachem        1.0.6   2021-08-19 [1] CRAN (R 4.1.0)
#>  callr         3.7.0   2021-04-20 [1] CRAN (R 4.1.0)
#>  cli           3.1.0   2021-10-27 [1] CRAN (R 4.1.0)
#>  crayon        1.4.2   2021-10-29 [1] CRAN (R 4.1.0)
#>  desc          1.4.0   2021-09-28 [1] CRAN (R 4.1.0)
#>  devtools      2.4.3   2021-11-30 [1] CRAN (R 4.1.0)
#>  digest        0.6.29  2021-12-01 [1] CRAN (R 4.1.0)
#>  ellipsis      0.3.2   2021-04-29 [1] CRAN (R 4.1.0)
#>  evaluate      0.14    2019-05-28 [1] CRAN (R 4.1.0)
#>  fastmap       1.1.0   2021-01-25 [1] CRAN (R 4.1.0)
#>  fs            1.5.2   2021-12-08 [1] CRAN (R 4.1.0)
#>  glue          1.6.0   2021-12-17 [1] CRAN (R 4.1.0)
#>  htmltools     0.5.2   2021-08-25 [1] CRAN (R 4.1.0)
#>  knitr         1.37    2021-12-16 [1] CRAN (R 4.1.0)
#>  lifecycle     1.0.1   2021-09-24 [1] CRAN (R 4.1.0)
#>  magrittr      2.0.1   2020-11-17 [1] CRAN (R 4.1.0)
#>  memoise       2.0.1   2021-11-26 [1] CRAN (R 4.1.0)
#>  pkgbuild      1.3.1   2021-12-20 [1] CRAN (R 4.1.0)
#>  pkgload       1.2.4   2021-11-30 [1] CRAN (R 4.1.0)
#>  prettyunits   1.1.1   2020-01-24 [1] CRAN (R 4.1.0)
#>  processx      3.5.2   2021-04-30 [1] CRAN (R 4.1.0)
#>  ps            1.6.0   2021-02-28 [1] CRAN (R 4.1.0)
#>  purrr         0.3.4   2020-04-17 [1] CRAN (R 4.1.0)
#>  R6            2.5.1   2021-08-19 [1] CRAN (R 4.1.0)
#>  remotes       2.4.2   2021-11-30 [1] CRAN (R 4.1.0)
#>  rlang         0.4.12  2021-10-18 [1] CRAN (R 4.1.0)
#>  rmarkdown     2.11    2021-09-14 [1] CRAN (R 4.1.0)
#>  rprojroot     2.0.2   2020-11-15 [1] CRAN (R 4.1.0)
#>  rstudioapi    0.13    2020-11-12 [1] CRAN (R 4.1.0)
#>  sessioninfo   1.2.2   2021-12-06 [1] CRAN (R 4.1.0)
#>  stringi       1.7.6   2021-11-29 [1] CRAN (R 4.1.0)
#>  stringr       1.4.0   2019-02-10 [1] CRAN (R 4.1.0)
#>  testthat      3.1.1   2021-12-03 [1] CRAN (R 4.1.0)
#>  usethis       2.1.5   2021-12-09 [1] CRAN (R 4.1.0)
#>  withr         2.4.3   2021-11-30 [1] CRAN (R 4.1.0)
#>  xfun          0.29    2021-12-14 [1] CRAN (R 4.1.0)
#>  yaml          2.2.1   2020-02-01 [1] CRAN (R 4.1.0)
#> 
#>  [1] /Users/Primahadi/Rlibs
#>  [2] /Library/Frameworks/R.framework/Versions/4.1/Resources/library
#> 
#> ──────────────────────────────────────────────────────────────────────────────
```

# References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-brewer2014" class="csl-entry">

Neuwirth, Erich. 2014. *RColorBrewer: ColorBrewer palettes*.
<https://CRAN.R-project.org/package=RColorBrewer>.

</div>

<div id="ref-corplingr" class="csl-entry">

Rajeg, Gede Primahadi Wijaya. 2021. *<span
class="nocase">corplingr</span>: Tidy concordances, collocates, and
wordlist*. *Open Science Framework*.
doi:[10.17605/OSF.IO/X8CW4](https://doi.org/10.17605/OSF.IO/X8CW4).
<https://gederajeg.github.io/corplingr/>.

</div>

<div id="ref-rajeg_linguistic_2019" class="csl-entry">

Rajeg, Gede Primahadi Wijaya, Poppy Siahaan & Alice Gaby. 2019.
Linguistic and co-speech gestural patterns of spatiotemporal metaphors
in Indonesian. Paper. 15th International Cognitive Linguistics
Conference (ICLC-15), Nishinomiya, Japan.
doi:[10.26180/5c660c9786cef](https://doi.org/10.26180/5c660c9786cef).
<https://monash.figshare.com/articles/Linguistic_and_co-speech_gestural_patterns_of_spatiotemporal_metaphors_in_Indonesian/7722362>.

</div>

<div id="ref-rajeg_spatial_2021" class="csl-entry">

Rajeg, Gede Primahadi Wijaya, Poppy Siahaan & Alice Gaby. 2021a. The
spatial construal of TIME in Indonesian: Evidence from language and
gesture. Paper. Kongres Internasional Masyarakat Linguistik Indonesia
2021 (KIMLI 2021), Online. <https://youtu.be/Onkfr8xVeBg>.

</div>

<div id="ref-rajeg_data_2021" class="csl-entry">

Rajeg, Gede Primahadi Wijaya, Poppy Siahaan & Alice Gaby. 2021b. Data
and R codes for analyses on the spatial construal of TIME in Indonesian
language and co-speech gestures. *Open Science Framework (OSF)*.
doi:[10.17605/OSF.IO/FJE6K](https://doi.org/10.17605/OSF.IO/FJE6K).
<https://osf.io/fje6k/> (17 November, 2021).

</div>

<div id="ref-wickham_welcome_2019" class="csl-entry">

Wickham, Hadley, Mara Averick, Jennifer Bryan, Winston Chang, Lucy
McGowan, Romain François, Garrett Grolemund, et al. 2019. Welcome to the
Tidyverse. *Journal of Open Source Software* 4(43), 1686.
doi:[10.21105/joss.01686](https://doi.org/10.21105/joss.01686).
<https://joss.theoj.org/papers/10.21105/joss.01686> (18 December, 2019).

</div>

<div id="ref-wickham_r_2017" class="csl-entry">

Wickham, Hadley & Garrett Grolemund. 2017. *R for Data Science*. Canada:
O’Reilly. <http://r4ds.had.co.nz/> (7 March, 2017).

</div>

</div>
