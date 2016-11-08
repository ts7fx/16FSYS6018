library(SensusR)

setwd("~/Documents")
data = sensus.read.json("~/Documents/sys6018_cs3_data/round_2/AccelerometerDatum")

plot(data$AccelerometerDatum)

test <- data[[1]]


# checking out heart rate data. especially looking at its completeness. 
setwd("~/Documents/sys6018_cs3_data/round_2/AccelerometerDatum")
data2 = sensus.read.json(data.path, is.directory = TRUE, recursive = TRUE,convert.to.local.timezone = TRUE, local.timezone = Sys.timezone())
test2 <- data2[[1]]

data_HR = sensus.read.json("~/Documents/sys6018_cs3_data/round_2/MicrosoftBandHeartRateDatum")

sample  <- data_HR[[1]]
