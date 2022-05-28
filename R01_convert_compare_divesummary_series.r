### convert and compare dive summary to series
# this is an interactive program that allows manual qa/qc of
# predicted dives from the time-series data.
# intermediate outputs produced by this script from the run 
# used in the present analysis can be found in 00_data/working

# libraries and helper functions
library(sattagutils) # github.com/williamcioffi/sattagutils
source("01_helper_functions/clip_ser_beh.r")
source("01_helper_functions/trim_ser_beh.r")
source("01_helper_functions/compare_beh_ser.r")
source("01_helper_functions/findgaps.r")
source("01_helper_functions/plot_dives.r")
source("01_helper_functions/censor_series.r")
source("01_helper_functions/quicklook_compare_beh_ser.r")

# load data
tags <- readRDS("00_data/tags/baseline_tags.rds")

### compare series and behavior for baseline tags at {2.5, 5, 7.5} sampling periods

native_period <- 2.5
periods_to_test <- c(native_period, 5, 7.5)

for(p in periods_to_test) {

outs <- list()
outs_33 <- list()
dothesetags <- 1:length(tags)
for(t in dothesetags[1:length(tags)]) {
  curtag <- tags[[t]]
  
  if(periods_to_test[p] != native_period) {
    serindex <- which(sattagutils::streamtype(curtag) == "series")
    ser1 <- curtag[[serindex]]
    ser2 <- censor_series(ser1, periods_to_test[p]*60)
    curtag[[serindex]] <- ser2
  }
  
  # clip the behavior and series to each other
  outclip <- clip_ser_beh(curtag, period = periods_to_test[p]*60)
	
  # convert ser2beh
  beh_converted_tmp <- lapply(outclip$serl, ser2beh, period = periods_to_test[p]*60)
  notnull <- !sapply(beh_converted_tmp, is.null)
  beh_converted_tmp <- beh_converted_tmp[notnull]
  outclip$behl <- outclip$behl[notnull]
  outclip$serl <- outclip$serl[notnull]
	
  # save some intermediate files
  # saveRDS(beh_converted_tmp, file = paste0("00_data/working/beh_converted_tmp-",  periods_to_test[p], "-", t, ".rds"))
  # saveRDS(outclip, file = paste0("00_data/working/outclip-", periods_to_test[p], "-", t, ".rds"))
	
  # calculate some comparison stats baseline censored
  outcomp <- compare_beh_ser(outclip$behl, outclip$serl, beh_converted_tmp, only_dives = TRUE, mindep = 50, minduration = 30)
  outs[[t]] <- outcomp
  # saveRDS(outcomp, file = paste0("00_data/working/outcomp-", periods_to_test[p], "-", t, ".rds"))
	
  # calculate some comparison stats brs censored
  outcomp <- compare_beh_ser(outclip$behl, outclip$serl, beh_converted_tmp, only_dives = TRUE, mindep = 50, minduration = 33*60)
  outs_33[[t]] <- outcomp
  # saveRDS(outcomp, file = paste0("00_data/working/outcomp-33min-", periods_to_test[p], "-", t, ".rds"))
	
  # take a quicklook
  dev.new()
  quicklook_compare_beh_ser(outcomp)

  response <- readline("ready for the next one? (press return)")
}

saveRDS(outs, file = paste0("00_data/working/outs_alltags_", periods_to_test[p], "_period_brs_censored.rds"))
saveRDS(outs_33, file = paste0("00_data/working/outs_alltags_33min_", periods_to_test[p], "_period_brs_censored.ds"))

}
