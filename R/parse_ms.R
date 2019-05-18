# parse_ms <- function(ms_out) .
# 
# ms_out <- readLines("~/ms_out.txt")
# 
# N <- regmatches(ms_out[1], gregexpr("[[:digit:]]+", ms_out[1]))
# nsam <- as.numeric(unlist(N)[1])
# nreps <- as.numeric(unlist(N)[2])
# 
# ms_out <- ms_out[-(1:3)]
# ms_out <- ms_out[grepl("^\\d", ms_out)]
# 
# m <- matrix(data = ms_out, nrow = nsam*nreps, ncol = nsam, byrow = T)
# 
# l <- as.list(rep(NA, nsam))
# 
# idx <- seq(nreps, nsam*nreps, by = nreps)
# idx1 <- c(0, idx[-length(idx)]) + 1L
# 
# for (i in seq_along(l)) {
#   l[[i]] <- m[idx1[i]:idx[i], ]
# }

