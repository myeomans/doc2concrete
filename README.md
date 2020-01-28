
<!-- README.md is generated from README.Rmd. Please edit that file -->

Concreteness
============

Concreteness has long been central to psychological theories of learning and thinking, and increasingly has practical applications to domains with prevalent natural language data, like advice and plan-making. However, the literature provides diffuse and competing definitions of concreteness in natural language. In this package, we codify simple guidelines for automated concreteness detection within and across domains, developed from a review of existing methods in the literature.

Installation
------------

You can install the doc2concrete package directly, like so:

``` r
devtools::install_github("myeomans/doc2concrete")
```

Usage
-----

This package is built as an accompaniment to Yeomans (2020). Here, we operationalize models of document-level concreteness based on a survey of datasets in several domains, including advice. We offer two applications. First, we provide a pre-trained model specifically tuned to measure concreteness in open-ended advice. This was developed using supervised machine learning tools, and robustly outperforms other domain-specific models we tested across a wide range of advice datasets from lab and field settings. Second, we provide an open-domain model based on a word-level concreteness dictionary in Byrsbaert, Warriner & Kuperman (2014). While the open domain model did seem relatively robust in our research, we also found substantial variation in concreteness within and across domains. We provide this open-domain model as a scaleable starting point for researchers interested in concreteness in other domains. However, we highly recommend that researchers conduct deeper work to better understand their own domain-specific model of concreteness.

``` r


library(doc2concrete)

cor.test(doc2concrete(feedback_dat$feedback,domain="open"),
    feedback_dat$concrete)

cor.test(doc2concrete(feedback_dat$feedback,domain="advice"),
         feedback_dat$concrete)
```

References
----------

Yeomans, M. (2020). Concreteness, Concretely. Working Paper.

Brysbaert, M., Warriner, A. B., & Kuperman, V. (2014). Concreteness ratings for 40 thousand generally known English word lemmas. Behavior Research Methods, 46(3), 904-911.
