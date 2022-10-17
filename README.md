# On the temporal clustering of European extreme precipitation events and its relationship to persistent and transient large-scale atmospheric drivers
## R-code
____________________________________________________________________________________________

#### Read in required libraries
library(mgcv) # library for GAMs  
library(lubridate) # library for nice dates  
library(data.table) # optional; libray for advanced data frames

#### We simulate a homogeneous Poisson point process for occurrence of events (to be replaced with real data)
pp <- rbinom(n = 365 * 40, size = 1, prob = 200 / (365 * 40)) # length 365 days x 40 years with ~200 points (ones)

#### We simulate a continuous covariate (to be replaced with real data)
v1 <- rnorm(365 * 40, mean = 0, sd = 1) # length 365 days x 40 years

#### We organize the data in a data table with dates and the day of the year
myDates <- seq(from=ymd('1979-01-01'), to=ymd('2018-12-31'), by='day') # Date array  
myDates <- myDates[!(month(myDates) == 2 & day(myDates) == 29)] # Remove Feb 29 from leap years  
doy <- rep(1:365, 40) # Day of the year array  
dt <- data.table(date=myDates, doy=doy, y=pp, V1=v1) # create data table with daily entries

#### We create a new variable used thereafter to apply group arithmetics
w <- 21 # let's choose a window size of 21 days  
dt[, gp := rep(seq(1, ceiling(nrow(dt)/w)), each=w)[1:nrow(dt)]]

#### For each group, we count the number of events and compute the mean over the covariates
dt.21d <- dt[, .(y=sum(y), date=date[11], doy=as.numeric(format(date[11], '%j')), V1=mean(V1, na.rm=T)), by=gp]

#### Standarsize covariates
cols <- c('V1') # enter here which columns need standardization  
dt.21d[, (cols) := lapply(.SD, function(x) scale(x)), .SDcols = cols] # columns are standardized (0-mean, 1-sd)  

#### Fit a GAM
knots <- list(doy = c(0.5, 365.5)) # define array of knots as such to ensure smooth cyclic transition from doy 365 to doy 1  
m1 <- gam(y ~ s(doy, bs='cc') + s(V1) + ti(V1, doy, bs=c('tp','cc')), data = dt.21d, knots = knots, method="REML", family="poisson")

#### check model output
summary(m1)

#### plot effects for each model term
plot(m1)
