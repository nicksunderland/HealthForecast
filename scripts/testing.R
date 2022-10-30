library(devtools)
library(icdb)
library(microbenchmark)
rm(list=ls())
load_all()

s    = as.Date('1990/01/01')
e    = as.Date('1990/01/10')
size = 1e4
ids  = sample(letters, size, replace=T)
dobs = sample(seq(as.Date('1990/01/01'), as.Date('2000/01/01'), by="day"), size, replace=T)

srv <- Databases("XSW")

cohort <- Cohort()

dateWindow(cohort) <- c(s,e)

setDiagnoses(cohort, "index") <- system.file("/acs_codes.csv", package="HealthForecast")





ethnicity(cohort@patients)
ethnicity(cohort@patients) <- sample(c("test", "white", "black"), size = length(cohort@patients@id), replace = T)
ethnicity(cohort@patients)

age(cohort@patients, Sys.Date())








