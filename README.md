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

<!-- badges: end -->

# Overview

This is an
[RStudio-project](https://r4ds.had.co.nz/workflow-projects.html)[1]
repository hosting the dataset and R codes used in the paper accepted
for publication in [*Linguistik
Indonesia*](http://ojs.linguistik-indonesia.org/index.php/linguistik_indonesia),
the official journal for [*Masyarakat Linguistik
Indonesia*](https://www.mlindonesia.org/) (MLI) (the Linguistic Society
of Indonesia). The paper reports on a [project on the spatio-temporal
metaphor in
Indonesian](https://udayananetworking.unud.ac.id/lecturer/research/880-gede-primahadi-wijaya-rajeg/spatial-construal-of-time-in-indonesian-language-and-co-speech-gestures-132)
based on investigating evidence from Indonesian language corpus and
spontaneous co-speech gestural data.

The earlier version of the paper has been presented at (i) the
[*15<sup>th</sup> International Cognitive Linguistics
Conference*](https://iclc2019.site/general-theme-session-presentations/#fri-07-03)
(ICLC-15) in August 2019 in Nishinomiya, Japan ([Rajeg, Siahaan & Gaby
2019](#ref-rajeg_linguistic_2019)), and (ii) *Kongres Internasional
Masyarakat Linguistik Indonesia 2021* (KIMLI 2021) ([Rajeg, Siahaan &
Gaby 2021](#ref-rajeg_spatial_2021)). We thank the participants at these
conferences for their feedback and comments. We are also grateful for
the travel grant awarded by the [*International Cognitive Linguistics
Association*](https://www.cognitivelinguistics.org/en) (ICLA) for our
presentation at ICLC-15.

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
    [`main-analysis-code.R`](https://github.com/gederajeg/spatiotemporal-metaphor-indonesian/blob/main/R-scripts/main-analysis-code.R)
    file in the
    [`R-scripts`](https://github.com/gederajeg/spatiotemporal-metaphor-indonesian/tree/main/R-scripts)
    directory.

-   The R codes also make use of functions from the
    [`tidyverse`](https://www.tidyverse.org/) family of R packages
    ([Wickham & Grolemund 2017](#ref-wickham_r_2017); [Wickham et al.
    2019](#ref-wickham_welcome_2019)). Users are required to install the
    tidyverse collection of packages in order to run (some parts of) the
    code.

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
#>  version  R version 4.0.5 (2021-03-31)
#>  os       macOS Big Sur 10.16         
#>  system   x86_64, darwin17.0          
#>  ui       X11                         
#>  language (EN)                        
#>  collate  en_US.UTF-8                 
#>  ctype    en_US.UTF-8                 
#>  tz       Asia/Makassar               
#>  date     2021-11-17                  
#> 
#> ─ Packages ───────────────────────────────────────────────────────────────────
#>  package     * version date       lib source        
#>  backports     1.1.7   2020-05-13 [1] CRAN (R 4.0.0)
#>  cachem        1.0.5   2021-05-15 [1] CRAN (R 4.0.2)
#>  callr         3.6.0   2021-03-28 [1] CRAN (R 4.0.2)
#>  cli           3.1.0   2021-10-27 [1] CRAN (R 4.0.2)
#>  crayon        1.4.1   2021-02-08 [1] CRAN (R 4.0.2)
#>  desc          1.4.0   2021-09-28 [1] CRAN (R 4.0.2)
#>  devtools      2.3.0   2020-04-10 [1] CRAN (R 4.0.0)
#>  digest        0.6.25  2020-02-23 [1] CRAN (R 4.0.0)
#>  ellipsis      0.3.1   2020-05-15 [1] CRAN (R 4.0.0)
#>  evaluate      0.14    2019-05-28 [1] CRAN (R 4.0.0)
#>  fastmap       1.1.0   2021-01-25 [1] CRAN (R 4.0.2)
#>  fs            1.4.1   2020-04-04 [1] CRAN (R 4.0.0)
#>  glue          1.4.1   2020-05-13 [1] CRAN (R 4.0.0)
#>  htmltools     0.5.2   2021-08-25 [1] CRAN (R 4.0.2)
#>  knitr         1.30    2020-09-22 [1] CRAN (R 4.0.2)
#>  lifecycle     1.0.0   2021-02-15 [1] CRAN (R 4.0.2)
#>  magrittr      2.0.1   2020-11-17 [1] CRAN (R 4.0.2)
#>  memoise       2.0.0   2021-01-26 [1] CRAN (R 4.0.2)
#>  pkgbuild      1.0.8   2020-05-07 [1] CRAN (R 4.0.0)
#>  pkgload       1.1.0   2020-05-29 [1] CRAN (R 4.0.0)
#>  prettyunits   1.1.1   2020-01-24 [1] CRAN (R 4.0.0)
#>  processx      3.5.1   2021-04-04 [1] CRAN (R 4.0.2)
#>  ps            1.6.0   2021-02-28 [1] CRAN (R 4.0.2)
#>  purrr         0.3.4   2020-04-17 [1] CRAN (R 4.0.0)
#>  R6            2.4.1   2019-11-12 [1] CRAN (R 4.0.0)
#>  remotes       2.1.1   2020-02-15 [1] CRAN (R 4.0.0)
#>  rlang         0.4.11  2021-04-30 [1] CRAN (R 4.0.2)
#>  rmarkdown     2.11    2021-09-14 [1] CRAN (R 4.0.2)
#>  rprojroot     1.3-2   2018-01-03 [1] CRAN (R 4.0.0)
#>  rstudioapi    0.13    2020-11-12 [1] CRAN (R 4.0.2)
#>  sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 4.0.0)
#>  stringi       1.5.3   2020-09-09 [1] CRAN (R 4.0.2)
#>  stringr       1.4.0   2019-02-10 [1] CRAN (R 4.0.0)
#>  testthat      3.0.2   2021-02-14 [1] CRAN (R 4.0.2)
#>  usethis       2.1.3   2021-10-27 [1] CRAN (R 4.0.2)
#>  withr         2.4.1   2021-01-26 [1] CRAN (R 4.0.2)
#>  xfun          0.22    2021-03-11 [1] CRAN (R 4.0.2)
#>  yaml          2.2.1   2020-02-01 [1] CRAN (R 4.0.0)
#> 
#> [1] /Users/Primahadi/Rlibs
#> [2] /Library/Frameworks/R.framework/Versions/4.0/Resources/library
```

# References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-rajeg_linguistic_2019" class="csl-entry">

Rajeg, Gede Primahadi Wijaya, Poppy Siahaan & Alice Gaby. 2019.
Linguistic and co-speech gestural patterns of spatiotemporal metaphors
in Indonesian. Paper. 15th International Cognitive Linguistics
Conference (ICLC-15), Nishinomiya, Japan.
doi:[10.26180/5c660c9786cef](https://doi.org/10.26180/5c660c9786cef).
<https://monash.figshare.com/articles/Linguistic_and_co-speech_gestural_patterns_of_spatiotemporal_metaphors_in_Indonesian/7722362>.

</div>

<div id="ref-rajeg_spatial_2021" class="csl-entry">

Rajeg, Gede Primahadi Wijaya, Poppy Siahaan & Alice Gaby. 2021. The
spatial construal of TIME in Indonesian: Evidence from language and
gesture. Paper. Kongres Internasional Masyarakat Linguistik Indonesia
2021 (KIMLI 2021), Online. <https://youtu.be/Onkfr8xVeBg>.

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

[1] For another overview on the RStudio Project, see this blogpost from
RStudio:
<https://support.rstudio.com/hc/en-us/articles/200526207-Using-RStudio-Projects>
