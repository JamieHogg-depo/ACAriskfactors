#' @title getWavePlotVars
#' @param draws matrix (iterations x observations) of posterior draws
#' @param prefix (defaults to "") character vector that will be appended to 
#' the start of the column names. 
#' @param verbose (defaults to FALSE) reports progress of operation.
#' @param truncation_upper (defaults to 4) truncation of density approximation of 
#' ratios. With a value of 4, density evaluations above 4 and below 1/4 are removed. 
#' @returns Dataframe (observations x 2) with `xValues` and `yValues`
#' #' WARNING: Only use with relative measures where 1 is a meaningful middle value. 
#' #' NOTE: `draws` should NOT be log-transformed - the function will perform this action.
getWavePlotVars <- function(draws, prefix = "", verbose = FALSE, truncation = c(1/4, 4)){

if(any(draws < 0))
  stop(paste0("Ensure all values in `", deparse(substitute(draws)), "` are positive..."))
  
# create empty data.frame
# temp.wave <- NULL

# loop through each column of `draws`
# if(verbose){pb <- txtProgressBar(min = 0, max = ncol(draws), style = 3)}
# for(i in 1:ncol(draws)){

  # set truncation amount
  # truncation <- c(1/truncation_upper, truncation_upper)
  
  # Initial density estimation
  f <- density(log2(draws), n = 128)
  x.out <- f$x
  y.out <- f$y
  
  # truncate densities
  keep <- which(x.out > log2(truncation[1]) & x.out < log2(truncation[2]))
  
  # select points
  if(length(keep) > 0){
    x.out <- x.out[keep]
    y.out <- y.out[keep]
    
    # Identify mode and points at strategic y values
    n2 <- length(x.out)
    Mode <- which.max(y.out)
    h85 <- abs(y.out[Mode] * 0.85 - y.out)
    h10 <- abs(y.out[Mode] * 0.1 - y.out)
    zeros <- y.out
    zeros[which(zeros < y.out[Mode] * 0.002)] <- Inf
    # Set left points
    if(Mode == 1){
      h85.L <- Mode
      h10.L <- Mode
      Min <- Mode
    }else{
      h85.L <- which.min(h85[1:(Mode - 1)])
      h10.L <- which.min(h10[1:(Mode - 1)])
      Min <- which.min(zeros[1:(Mode - 1)])
    }
    # Set right points
    if(Mode == n2){
      h85.R <- Mode
      h10.R <- Mode
      Max <- Mode
    }else{
      h85.R <- which.min(h85[(Mode + 1):n2]) + Mode
      h10.R <- which.min(h10[(Mode + 1):n2]) + Mode
      Max <- which.min(zeros[(Mode + 1):n2]) + Mode
    }
    keep <- c(Min, h10.L, h85.L, Mode, h85.R, h10.R, Max)
  }else{
    keep <- which.max(y.out)
    if(keep < n){   # Stack at lower truncation
      keep <- 1:7
      x.out <- rep(log2(truncation[1]), 7)
    }else{          # Stack at upper truncation
      keep <- (n-6):n
      x.out <- rep(log2(truncation[2]), 7)
    }
    y.out <- y.out[keep]
  }
  
  # Reduce to 7 points
  x.out <- x.out[keep]
  y.out <- y.out[keep]
  
  # Duplicate end value with zero y values to ensure correct shading of wave plot
  x.out <- c(x.out[1], x.out, x.out[7])
  y.out <- c(0, y.out, 0)
  
  # add to dataset
  temp.wave <- c(x.out, y.out)
  
  # progress bar
  # if(verbose)setTxtProgressBar(pb, i)

# } # end for loop
# if(verbose)close(pb)

# Round wave x-values to 2 dp
x.round <- round(x.out, digits = 2)

# Round wave y-values to 3 dp
y.round <- round(y.out, digits = 3)

# Concatenate wave x and y values into strings
x_str <- paste(x.round, collapse = ",")
y_str <- paste(y.round, collapse = ",")
temp.wave <- data.frame(xValues = x_str, 
                        yValues = y_str)

# return dataframe
return(temp.wave)
# cat(paste0(i, ": ", temp.wave, "\n, \n"))
}
