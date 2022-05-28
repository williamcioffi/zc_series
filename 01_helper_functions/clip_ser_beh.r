# functionalized steps

# 1. clip out matching series and behavior and take a look

clip_ser_beh <- function(tag, period = 2.5*60) {
	PERIOD <- period
	
	s <- getstream(tag, "series")[[1]]
	m <- getstream(tag, "seriesrange")[[1]]
	b <- getstream(tag, "behavior")[[1]]
	
	# identify continuous stretches of behavior and series
	stretches <- findgaps2(b)$stretchid
	ustretches <- unique(stretches)
	
	# these lists will hold a sequence of data frame
	# trimed to overlapping behavior and series data
	behl <- list()
	serl <- list()
	
	# list index counter
	k <- 1
		
	for(i in 1:length(ustretches)) {
		dese <- stretches == ustretches[i]
		curb <- b[dese, ]
		
		st <- min(curb$Start)
		en <- max(curb$End)
		
		curs <- s[s$Date >= st & s$Date <= en, ]
		
		# only proceed if series data exists for this stretch of behavior data
		if(nrow(curs) > 0) {
			# split series into continuous stretch within this behavior stretch
			serstretch <- findgaps2(data.frame(Start = curs$Date, End = curs$Date, What = "Message"), tolerance = PERIOD)$stretchid
			userstretch <- unique(serstretch)
			
			# iterate over each stretch of series data
			# clip out corresponding behavior
			for(n in 1:length(userstretch)) {
				dese_ser <- serstretch == userstretch[n]
				curcurs <- curs[dese_ser, ]
				
				sts <- min(curcurs$Date)
				ens <- max(curcurs$Date)
				
				curcurb <- curb[curb$End >= sts & curb$Start <= ens, ]
				
				###
				# take a look
				# plot_dives2(curcurb, pch = NA, show_gaps = FALSE)
				# points(curcurs$Date, -curcurs$Depth, pch = 16, cex = .5)
				# lines(curcurs$Date, -curcurs$Depth)
				# legend("bottomleft", legend = k, bty = 'n')
				# pause <- readline("(press return to continue)")
				# END: take a look
				###
				
				# store data frames in matching lists
				behl[[k]] <- curcurb
				serl[[k]] <- curcurs
				k <- k + 1
			}	
		}
	}
	
	list(behl = behl, serl = serl)
}
