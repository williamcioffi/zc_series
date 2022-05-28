# function to compare a single tag behavior and series

compare_beh_ser <- function(behl, serl, beh_converted_tmp, overlap_threshold = 0.5, only_dives = FALSE, mindep = 50, minduration = 33*60) {
	# remove empties
	delete <- sapply(beh_converted_tmp, nrow) == 0
	beh_converted_tmp <- beh_converted_tmp[!delete]
	behl <- behl[!delete]
	serl <- serl[!delete]
	
	# sensor these so they look like BRS tags
	beh_converted_censored <- lapply(beh_converted_tmp, censor_beh, depth = mindep, duration = minduration)
	beh_original_censored <- lapply(behl, censor_beh, depth = mindep, duration = minduration)
	
	# trim to each other
	out <- trim_ser_beh(beh_converted_censored, beh_original_censored, serl)
	beh_converted_trimmed <- out$beh_converted_trimmed
	beh_original_trimmed <- out$beh_original_trimmed
	ser_original_trimmed <- out$ser_original_trimmed
	
	# remove NULLs
	notnull <- !sapply(beh_converted_trimmed, is.null)
	beh_converted_trimmed <- beh_converted_trimmed[notnull]
	beh_original_trimmed <- beh_original_trimmed[notnull]
	ser_original_trimmed <- ser_original_trimmed[notnull]
	
	# at this point see if you have anything after trimming.
	# if no return an NA
	if(length(beh_original_trimmed) == 0 | length(beh_converted_trimmed) == 0) return(NA)
	if(all(sapply(beh_converted_trimmed, is.null)) | all(sapply(beh_original_trimmed, is.null))) return(NA)
	
	if(only_dives) {
	  for(i in 1:length(beh_original_trimmed)) {
	    beh_original_trimmed[[i]] <- beh_original_trimmed[[i]][beh_original_trimmed[[i]]$What == "Dive", ]
	  }
	  for(i in 1:length(beh_converted_trimmed)) {
	    beh_converted_trimmed[[i]] <- beh_converted_trimmed[[i]][beh_converted_trimmed[[i]]$What == "Dive", ]
	  }
	}
	
	# at this point check again to see if we have anything
	# if no return NA
	if(length(beh_original_trimmed) == 0 | length(beh_converted_trimmed) == 0) return(NA)
	if(all(sapply(beh_converted_trimmed, is.null)) | all(sapply(beh_original_trimmed, is.null))) return(NA)
	
	# contants
	OVERLAP_THRESHOLD <- overlap_threshold
	
	# tiny helper function
	intervaloverlap <- function(a1, a2, b1, b2) {
		if(length(a1) != length(a2) | length(b1) != length(b2)) stop("a1, a2 and b1, b2 must be same length")
		
		na <- length(a1)
		nb <- length(b1)
		
		out <- list()
		
		for(i in 1:na) {
			tmpout <- vector()
			for(p in 1:nb) {
				tmpout[p] <- abs(median(c(a1[i], b1[p], b2[p])) - median(c(a2[i], b1[p], b2[p]))) / (max(a2[i], b2[p]) - min(a1[i], b1[p]))
			}
			
			out[[i]] <- tmpout
		} 
		
		out
	}
	
	beh_conv_trim_matched <- list()
	beh_orig_trim_matched <- list()
	
	for(i in 1:length(beh_converted_trimmed)) {
		bx <- beh_converted_trimmed[[i]] 
		bo <- beh_original_trimmed[[i]]
		
		bx[, 'match'] <- NA
		bo[, 'match'] <- NA
		
		# first check for bo matches to bx	
		for(p in 1:nrow(bo)) {
			if(bo$What[p] != "Message") {
				overlaps <- intervaloverlap(bo$Start[p], bo$End[p], bx$Start, bx$End)[[1]]
				bx_event_match <- which(overlaps > OVERLAP_THRESHOLD & (bx$What == bo$What[p]))
				if(length(bx_event_match == 1)) {
					bx[bx_event_match, 'match'] <- 	p
				}
			}
		}
		
		# now check for bx matches to bo
		for(p in 1:nrow(bx)) {
			if(bx$What[p] != "Message") {
				overlaps <- intervaloverlap(bx$Start[p], bx$End[p], bo$Start, bo$End)[[1]]
				bo_event_match <- which(overlaps > OVERLAP_THRESHOLD & (bo$What == bx$What[p]))
				if(length(bo_event_match == 1)) {
					bo[bo_event_match, 'match'] <- 	p
				}
			}
		}	
		# copy these back into a new list
		beh_conv_trim_matched[[i]] <- bx
		beh_orig_trim_matched[[i]] <- bo
	}
	
	cdur <- list()
	odur <- list()
	cdurlo <- list()
	cdurhi <- list()
	odurlo <- list()
	odurhi <- list()
	
	cdep <- list()
	odep <- list()
	cdeplo <- list()
	cdephi <- list()
	odeplo <- list()
	odephi <- list()
	
	wht <- list()
	cshp <- list()
	oshp <- list()
	
	cmis <- vector()
	omis <- vector()
	cn <- vector()
	on <- vector()
	
	cmisswht <- list()
	omisswht <- list()
	
	cmissdur <- list()
	omissdur <- list()
	cmissdep <- list()
	omissdep <- list()
	
	for(i in 1:length(beh_conv_trim_matched)) {
		curconv <- beh_conv_trim_matched[[i]]
		curorig <- beh_orig_trim_matched[[i]]
	
		# events that didn't match
		curconv.miss <- is.na(curconv$match) & curconv$What != "Message"
		curorig.miss <- is.na(curorig$match) & curorig$What != "Message"
		
		curconv <- curconv[!is.na(curconv$match), ]
		
		# get the basics
		cdur[[i]] <- apply(curconv[, c('DurationMax', 'DurationMin')], 1, mean)
		odur[[i]] <- apply(curorig[, c('DurationMax', 'DurationMin')], 1, mean)[curconv$match]
		cdurlo[[i]] <- curconv[, 'DurationMin'][curconv$match]
		cdurhi[[i]] <- curconv[, 'DurationMax'][curconv$match]
		odurlo[[i]] <- curorig[, 'DurationMin']
		odurhi[[i]] <- curorig[, 'DurationMax']
		
		cdep[[i]] <- apply(curconv[, c('DepthMax', 'DepthMin')], 1, mean)
		odep[[i]] <- apply(curorig[, c('DepthMax', 'DepthMin')], 1, mean)[curconv$match]
		cdeplo[[i]] <- curconv[, 'DepthMin'][curconv$match]
		cdephi[[i]] <- curconv[, 'DepthMax'][curconv$match]
		odeplo[[i]] <- curorig[, 'DepthMin']
		odephi[[i]] <- curorig[, 'DepthMax']
		
		cshp[[i]] <- curconv$Shape
		oshp[[i]] <- curorig$Shape[curconv$match]
		
		wht[[i]] <- curconv$What
		
		# calculate the missed stats
		# remove trailing or leading misses because those are likely artifcats	
		# if(curconv.miss[1] == TRUE) curconv.miss[-1]
		# if(curconv.miss[length(curconv.miss)] == TRUE) curconv.miss[-length(curconv.miss)]
	
		# if(curorig.miss[1] == TRUE) curorig.miss[-1]
		# if(curorig.miss[length(curorig.miss)] == TRUE) curorig.miss[-length(curorig.miss)]
		
		# cmis[i] <- length(which(curconv.miss))
		# omis[i] <- length(which(curorig.miss))
		
		# cn[i] <- length(curconv.miss)
		# on[i] <- length(curorig.miss)
		
		# cmisswht[[i]] <- curconv[curconv.miss, ]$What
		# cmissdur[[i]] <- apply(curconv[curconv.miss, c('DurationMax', 'DurationMin')], 1, mean)
		# cmissdep[[i]] <- apply(curconv[curconv.miss, c('DepthMax', 'DepthMin')], 1, mean, na.rm = TRUE)
		
		# omisswht[[i]] <- curorig[curorig.miss, ]$What
		# omissdur[[i]] <- apply(curorig[curorig.miss, c('DurationMax', 'DurationMin')], 1, mean)
		# omissdep[[i]] <- apply(curorig[curorig.miss, c('DepthMax', 'DepthMin')], 1, mean, na.rm = TRUE)
	}
	
	# unlist everything
	cdur <- unlist(cdur)
	odur <- unlist(odur)
	cdep <- unlist(cdep)
	odep <- unlist(odep)
	wht <- unlist(wht)
	cshp <- unlist(cshp)
	oshp <- unlist(oshp)
	
	cdeplo <- unlist(cdeplo)
	cdephi <- unlist(cdephi)
	odeplo <- unlist(odeplo)
	odephi <- unlist(odephi)
	
	cdurlo <- unlist(cdurlo)
	cdurhi <- unlist(cdurhi)
	odurlo <- unlist(odurlo)
	odurhi <- unlist(odurhi)
	
	# cmissdur <- unlist(cmissdur)
	# omissdur <- unlist(omissdur)
	# cmissdep <- unlist(cmissdep)
	# omissdep <- unlist(omissdep)
	
	# cmisswht <- unlist(cmisswht)
	# omisswht <- unlist(omisswht)
	
	# output
	list(
		# conv_n			= cn,
		# orig_n			= on,
		what			= wht,
		conv_dur		= cdur,
		orig_dur		= odur,
		conv_dep		= cdep,
		orig_dep		= odep,
		conv_dep_lo		= cdeplo,
		conv_dep_hi		= cdephi,
		orig_dep_lo		= odeplo,
		orig_dep_hi		= odephi,
		conv_dur_lo		= cdurlo,
		conv_dur_hi		= cdurhi,
		orig_dur_lo		= odurlo,
		orig_dur_hi		= odurhi,
		conv_shp		= cshp,
		orig_shp 		= oshp
		# miss_conv_n		= cmis,
		# miss_orig_n		= omis,
		# miss_conv_dur 	= cmissdur,
		# miss_orig_dur 	= omissdur,
		# miss_conv_dep	= cmissdep,
		# miss_orig_dep 	= omissdep,
		# miss_conv_wht 	= cmisswht,
		# miss_orig_wht 	= omisswht
	)
}
