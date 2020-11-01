## Evaluating the use of the reproduction number as an epidemiological tool, using spatio-temporal trends of the Covid-19 outbreak in England

[![DOI](https://zenodo.org/badge/288408878.svg)](https://zenodo.org/badge/latestdoi/288408878)

### Abstract

The time-varying reproduction number (Rt: the average number secondary infections caused by each infected person) may be used to assess changes in transmission potential during an epidemic. Since new infections usually are not observed directly, it can only be estimated from delayed and potentially biased data. We estimated Rt using a model that mapped unobserved infections to observed test-positive cases, hospital admissions, and deaths with confirmed Covid-19, in seven regions of England over March through August 2020. We explored the sensitivity of Rt estimates of Covid-19 in England to different data sources, and investigated the potential of using differences in the estimates to track epidemic dynamics in population sub-groups.

Our estimates of transmission potential varied for each data source. The divergence between estimates from each source was not consistent within or across regions over time, although estimates based on hospital admissions and deaths were more spatio-temporally synchronous than compared to estimates from all test-positives. We compared differences in Rt with the demographic and social context of transmission, and found the differences between Rt may be linked to biased representations of sub-populations in each data source: from uneven testing rates, or increasing severity of disease with age, seen via outbreaks in care home populations and changing age distributions of cases.

We highlight that policy makers should consider the source populations of Rt estimates. Further work should clarify the best way to combine and interpret Rt estimates from different data sources based on the desired use.

### Resources

#### Quick highlights on twitter
<div class="center">
<blockquote class="twitter-tweet"><p lang="en" dir="ltr">1/7 In the UK, the reproduction (R) number is a <a href="https://twitter.com/hashtag/COVID19?src=hash&amp;ref_src=twsrc%5Etfw">#COVID19</a> policy headline. But what is R based on, and what does it really tell us about who is getting infected? New paper with <a href="https://twitter.com/seabbs?ref_src=twsrc%5Etfw">@seabbs</a>, <a href="https://twitter.com/sbfunk?ref_src=twsrc%5Etfw">@sbfunk</a>, and others at <a href="https://twitter.com/cmmid_lshtm?ref_src=twsrc%5Etfw">@cmmid_lshtm</a>: <a href="https://t.co/cAWEL1oKYn">https://t.co/cAWEL1oKYn</a> [NOT PEER REVIEWED] <a href="https://t.co/DfWWRCeBvk">pic.twitter.com/DfWWRCeBvk</a></p>&mdash; Katharine Sherratt (@kathsherratt) <a href="https://twitter.com/kathsherratt/status/1320836554911338500?ref_src=twsrc%5Etfw">October 26, 2020</a></blockquote>
</div>

#### Daily updating UK estimates
We publish daily estimates of Rt from each data source across the UK, and Rt from case counts globally, at [epiforecasts.io](https://epiforecasts.io/covid/posts/national/united-kingdom/).

#### Run the code used in the paper
These steps are a minimal guide to reproducing the code.
- [Update Rt estimates](https://github.com/epiforecasts/rt-comparison-uk-public/blob/master/rt-estimate/estimate-all-time/update-rt-estimate.R) for test-positive cases, hospital admissions, and deaths, in England and NHS regions of England
- [Format estimates for use elsewhere](https://github.com/epiforecasts/rt-comparison-uk-public/blob/master/rt-estimate/estimate-all-time/update-format.R)
- [Plot all Rt estimates against data](https://github.com/epiforecasts/rt-comparison-uk-public/blob/master/compare/plots/plot-all.R)
- [Compare Rt against known transmission dynamics](https://github.com/epiforecasts/rt-comparison-uk-public/tree/master/compare/supporting-analyses) - including test positivity rates, cases by age, and deaths in care homes. Code here should function but will not pull the latest data, as this currently relies on manual URL updating. Sources for this data are in the relevant code.

Please raise an [issue](https://github.com/epiforecasts/rt-comparison-uk-public/issues) if you are interested in running any of the code and find problems, or to discuss any part of the ideas, paper, or code base.
