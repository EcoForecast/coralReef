##Repo Description: This is a repo to store our GE585 course project on forecasting coral bleaching in Florida, USA. We fit a hiearchical jags model to yearly coral bleaching and number of high heat stress days for five regions in the Florida Keys.

##Authors and Contact Information

James Fifer - jfifer@bu.edu 

Christopher Reyes - chrislr@bu.edu

Shelby Sundquist - sundqshe@bu.edu

Kathryn Wheeler - kiwheel@bu.edu

##Data
We used some coral bleaching data from:

Donner, S.D., Rickbeil, G.J.M., Heron, S.F. (2017) "A new high-resolution global mass coral bleaching database." PLoS ONE 12(4): e0175490. https://doi.org/10.1371/journal.pone.0175490

Sea surface temperature data from NOAA: https://www.ncei.noaa.gov/data/sea-surface-temperature-whoi/

Days of high heat stress were determined using https://coralreefwatch.noaa.gov/satellite/methodology/methodology.php

To fill in the large data gaps in the Donner et al. (2017) database we relied on relationships between a coral bleaching and heat stress relationship based on  Eakin et al. (2010) "Caribbean Corals in Crisis: Record Thermal Stress, Bleaching, and Mortality in 2005." PLoS ONE 5(11): e13969. 

##Calibration JAGS Model Description
The state-space model was calibrating using JAGS. The model is a hierarchical linear model with random effects on year and region. Normal parameters were set on the region and year random effects with means of 0 and precisions ~ dgamma(1, 0.1). The mean (mu_r,t) for the latent state of bleaching for each time step (x_r,t) was set to be equal beta0 * x_r,(t-1) - rec + beta1 * S_r,t + year_t + reg_r where rec indicates a recovery term (rec ~ dgamma(6,10)), year is the year random effect, reg is the region random effect, and S indicates the heat stress term. Beta0 ~ dnorm(0.0341,10000) and Beta1 ~ dnorm(0.2694,4) based off of Eakin et al. (2010). All error precisions (on region, year, and observation) were ~ dgamma(1,0.1). The means for the latent state of bleaching were capped at 1 (for 100%) and then given a normal distribution data model. 

##Data Assimilation Technique
We implemented an ensemble kalman filter as the forecast-analysis cycle. 

##How to: All of the necessary functions are called from the mainCoralReefForecast.R script, which labels the steps. Downloading the SST data takes a long time, but all of the data is currently downloaded on BU's shared computing cluster (/projectnb/dietzelab/kiwheel/coralReef). Additionally, we included csv's for the SST data for the different regions in this repo. These data processing steps are commented out in the mainCoralReefForecast.R script to allow it to run. 

##Cron job to run the coral reef forecast 

0 7 * * * mainCoralReefForecast.R
