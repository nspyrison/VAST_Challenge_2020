library(tictoc); library(beepr); library(tidyverse)
do_run <- T

filepath <- "./Data/MC1 Data/CGCS-GraphData.csv"

samp <- read.csv2(filepath, #select a random 10000 rows
                  sep = (","),
                  header = T, 
                  check.names = T, 
                  stringsAsFactors = F,
                  na.strings = "",
                  nrows = 10000)
)
as.tibble(samp)
message("Note 10000 NA's in last 4 columns.")
message("Time is negative int ( on order -1E9), doesn't look like a concat of granularities.
        Google-foo suggests Unix time (POSIX time), or 'the number of 
        seconds that have passed since 00:00:00 UTC Thursday, 1 January 1970' ")
samp$Time <- lubridate::as_datetime(samp$Time) ## Where x is POSIXt
message("Seems to be many 16:00 hr, might be able to infer a timezone from this") 

broom::glance(samp) ## na.fraction: .545 = 6/11, good
skimr::skim(samp)

## On to a real slice
if (do_run){
  warning("Going to read a 5.9 GB csv into ram.")
  ## Full sample is more than 123 million rows of 11 var, break into sections of 21 million obs?
  nSlices <- 1 #6
  rowsPerSlice <- 21000000
  
  for (i in 1:nSlices) {
    tic(paste0("read data for: dat", i))
    .dat <- read.csv2(filepath, 
                      sep = (","),
                      header = T, 
                      check.names = T, 
                      stringsAsFactors = F,
                      na.strings = "",
                      nrows = rowsPerSlice,
                      skip = (i - 1) * rowsPerSlice)
    .dat$Time <- lubridate::as_datetime(.dat$Time) ## Where x is POSIXt
    assign(paste0(dat, i), .dat)
    rm(.dat)
    toc() ## took 61sec on ns Dell laptop for dat1
    beep()
  }
}

summary(dat1)
message("not all source and target locations are NA, ")


