### output summary statistics for comparison

# libraries and helper functions
library(sattagutils) # github.com/williamcioffi/sattagutils

# tiny helper function
boolperc <- function(comp) {
	if(mode(comp) != "logical") error("i need a bool vec")
	length(which(comp)) / length(comp)
}

# load tag data
tags <- readRDS("00_data/tags/baseline_tags.rds")

# load in metric data for each sampling period
periods <- c(2.5, 5, 7.5)
allouts <- list()
for(n in 1:length(periods)) {
  allouts_tmp <- readRDS(file.path(paste0("00_data/working/outs_alltags_", periods[n], "_period_brs_censored.rds")))
  allouts_tmp <- allouts_tmp[!is.na(allouts_tmp)]
  
  allouts[[n]] <- allouts_tmp
}

### output stats
for(i in 1:length(allouts)) {
	outs <- allouts[[i]]
	
	# concat some useful metrics
	what <- do.call('c', lapply(outs, "[[", "what"))
	conv_dur <- do.call('c', lapply(outs, "[[", "conv_dur"))
	orig_dur <- do.call('c', lapply(outs, "[[", "orig_dur"))
	
	conv_dep <- do.call('c', lapply(outs, "[[", "conv_dep"))
	orig_dep <- do.call('c', lapply(outs, "[[", "orig_dep"))
	
	conv_shp <- do.call('c', lapply(outs, "[[", "conv_shp"))
	orig_shp <- do.call('c', lapply(outs, "[[", "orig_shp"))
	
	
	## summary stats
	durdiff <- conv_dur - orig_dur
	depdiff <- conv_dep - orig_dep
	depdiff_lt33 <- conv_dep[orig_dur < 33*60] - orig_dep[orig_dur < 33*60]
	depdiff_ge33 <- conv_dep[orig_dur >= 33*60] - orig_dep[orig_dur >= 33*60]
	
	print(paste0("######## ", periods[i]))
	print(paste0("n: ",            length(conv_dur)))
	print(paste0("dur: ",          mean(abs(durdiff))))
	print(paste0("range: ",        paste(range(abs(durdiff)), collapse = ', ')))
	print(paste0("dur %:",         boolperc(abs(durdiff) < periods[i]*60)))
	print(paste0("dep: ",          mean(abs(depdiff))))
	print(paste0("dep < 33 min: ", mean(abs(depdiff_lt33))))
	print(paste0("range: ",        paste(range(abs(depdiff_lt33)), collapse = ', ')))
	print(paste0("dep >= 33 min",  mean(abs(depdiff_ge33))))
	print(paste0("range: ",        paste(range(abs(depdiff_ge33)), collapse = ', ')))
}