# quicklook_compare_beh_ser

quicklook_compare_beh_ser <- function(outcomp) {
  # take a peak
  cbind(outcomp$conv_dur, outcomp$orig_dur, outcomp$conv_dur - outcomp$orig_dur)
  cbind(outcomp$conv_dep, outcomp$orig_dep, outcomp$conv_dep - outcomp$orig_dep)
  par(mfrow = c(2, 2), mar = c(4.1, 4.1, 3.1, 1.1))
  hist(outcomp$conv_dur - outcomp$orig_dur, nclass = 20, col = "grey", main = "DURATION", xlab = "duration difference (s)", las = 1)
  box()
  axis(1, tcl = 0.3, labels = NA)
  axis(2, tcl = 0.3, labels = NA)
  hist(outcomp$conv_dep - outcomp$orig_dep, nclass = 20, col = "grey", main = "DEPTH", xlab = "depth difference (meters)", las = 1)
  box()
  axis(1, tcl = 0.3, labels = NA)
  axis(2, tcl = 0.3, labels = NA)
  
  plot(jitter(outcomp$orig_dur), jitter(outcomp$conv_dur - outcomp$orig_dur), pch = 16, cex = .75, las = 1, xlab = "behavior duration (s)", ylab = "duration difference (s)", main = "")
  axis(1, tcl = 0.3, labels = NA)
  axis(2, tcl = 0.3, labels = NA)
  plot(jitter(outcomp$orig_dep), jitter(outcomp$conv_dep - outcomp$orig_dep), pch = 16, cex = .75, las = 1, xlab = "behavior max depth (meters)", ylab = "depth difference (meters)", main = "")
  axis(1, tcl = 0.3, labels = NA)
  axis(2, tcl = 0.3, labels = NA)
  
  outcomp$conv_shp[outcomp$conv_shp == ""] <- NA
  print("p(shape_correct)")
  print(table(outcomp$conv_shp == outcomp$orig_shp) / length(which(!is.na(outcomp$conv_shp))))
}