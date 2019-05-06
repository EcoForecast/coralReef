
# Coral Bleaching Group Project - GE585 Spring 2019

Christopher Reyes - chrislr@bu.edu

Shelby Sundquist - sundqshe@bu.edu

James Fifer - jfifer@bu.edu 

Kathryn Wheeler - kiwheel@bu.edu

## Cron job to run the coral reef forecast (now just downloading and plotting data)

0 7 * * * mainCoralReefForecast.R

## Model Description: 
The process model is a dynamic linear model that relates heat stress to the amount of coral bleaching. As the amount of coral bleaching has three levels (no/low, medium, and high), a categorical data distribution is used. Random effects are given for the different years and the different regions. 
