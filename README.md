# Master's CE -- Estimating the County Health Rankings Using Bayesian Hierarchical Modeling

### Abstract

Public health officials can greatly benefit their communities by taking into account the results from the County Health Rankings. The County Health Rankings rank counties within each state based on a number of health outcomes and health factors. Though these rankings are valuable, they remain limited by their uncertainty quantification. This paper aims to use Bayesian hierarchical models to rank each county and measure differences between the County Health Rankings and the estimated ranks. Estimated rankings are produced for each of the health outcomes using two models. One of the model uses state and county level random effects. The other model uses state and county level random effects and includes demographic fixed effects (race, ethnicity, sex) and urbanization classification fixed effects. A successful implementation can provide a framework for estimating uncertainty in the rankings.

### Repo Overview

- data/ contains all of the data used for this project excluding the census data file because it was too large. For details on where the data was retrieved from, please check out the paper pdf.
- figures/ contains some of the figures used in the paper
