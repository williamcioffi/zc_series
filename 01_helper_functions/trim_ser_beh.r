# trim records to each other
trim_ser_beh <- function(beh_converted_censored, beh_original_censored, serl) {
	
	beh_converted_trimmed <- list()
	beh_original_trimmed <- list()
	ser_original_trimmed <- list()
	
	for(i in 1:length(beh_converted_censored)) {
		cur <- beh_converted_censored[[i]]
		keep <- rep(TRUE, nrow(cur))
		if(cur$What[2] == "Surface") keep[2] <- FALSE
		if(cur$What[nrow(cur)] == "Surface") keep[nrow(cur)] <- FALSE
	
		cur_clipped <- cur[keep, ]
		
		if(nrow(cur_clipped) > 1) {	
			# adjust message length
			cur_clipped$Start[1] <- cur_clipped$Start[2]
			cur_clipped$End[1] <- cur_clipped$End[nrow(cur_clipped)]
			
			beh_converted_trimmed[[i]] <- cur_clipped
		
			# clip original series and behavior exactly to data
			curorig <- beh_original_censored[[i]]
			serorig <- serl[[i]]
			
			# get start and end of series behavior data
			serst <- cur_clipped$Start[1]
			seren <- cur_clipped$End[1]
			
			# series
			ser_original_trimmed[[i]] <- serorig[serorig$Date >= serst & serorig$Date <= seren, ]
			
			# behavior
			keep_beh <- curorig$End >= serst & curorig$Start <= seren
			beh_clipped <- NULL
			
			if(any(keep_beh)) {
				beh_clipped <- curorig[keep_beh, ]
				
				msgid <- cumsum(beh_clipped$What == "Message")
				umsgid <- unique(msgid)
			
				# adjust message start and end times
				for(m in 1:length(umsgid)) {
					dese <- msgid == umsgid[m]
					beh_clipped[dese, ]$Start[1] <- beh_clipped[dese, ]$Start[2]
					beh_clipped[dese, ]$End[1] <- beh_clipped[dese, ]$End[length(which(dese))]
				}	
			}
						
			beh_original_trimmed[[i]] <- beh_clipped
		}
	}

	list(beh_converted_trimmed = beh_converted_trimmed, beh_original_trimmed = beh_original_trimmed, ser_original_trimmed = ser_original_trimmed)
}