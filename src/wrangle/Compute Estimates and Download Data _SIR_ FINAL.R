# Compute 10, 20, 50, 80, and 90 percentiles, and PP value for each Cancer-sex-SA2
# Compute log base 2 percentiles
# Compute quantiles for downloadable data (DLD) CSV file
#
# Authors: Earl Duncan
# Created: 16/08/18 (Based on "Compute Estimates and Wave Plot Combined" script)
# Updated: 16/08/18 Removed wave plot calculations;
#                   Added output for downloadable content in long format
#           03/09/18 Re-added wave plot, but for log-SIR rather than SIR
#                   Updated fp.data filepath
#           06/09/18 quantiles now log base e rather than base 2
#           17/09/18 Wave plot based on log2(SIR)
#           18/09/18 DLD quantiles not truncated
#                    Changed the number of density points from 64 to 128
#==========================================================================

# IMPORTANT NOTE

# Please ensure the object current.extract points to latest data extract before
# running this script.

#==========================================================================

# Load packages
library(readstata13)        # For read.dta13()
library(ggplot2)            # For debugging

# Input year of extract
current.extract <- 2016
rep = "_more_iter"

# Set filepaths
fp.root <- "G:"
fp.wd <- paste0(fp.root, "/Results/", current.extract, " Extract/Export Data")
if (!dir.exists(fp.wd)) dir.create(fp.wd)
setwd(fp.wd)

fp.data <- function(cancer, sex, current.extract, rep) {
    temp <- paste0(fp.root, "/Results/", current.extract, " Extract/SIR/MCMC Output/")
    trial <- paste0(temp, "MCMC ", cancer, "_", sex, rep, ".Rdata")
    if (file.exists(trial)) {
        out <- trial
    } else {
        out <- paste0(temp, "MCMC ", cancer, "_", sex, ".Rdata")
    }
    return(out)
}


fp.concordance <- paste0(fp.root, "/Geography/Shapefile Concordance.csv")

# Read in concordance data
concordance <- read.csv(fp.concordance)

# IMPORTANT: reduce concordance rows to match shapefile (2193)
concordance <- concordance[!is.na(concordance$ID_2193),]
stopifnot(2193 == nrow(concordance))

# SA2 codes to Labels
SA2.code <- concordance$SA2_Code_Short[which(!is.na(concordance$ID_2193))]
# N.B. Data only available for 2148 areas, but need to list 2193 areas for Thom


# Define cancer and sex codes
Ref.cancer <- data.frame(
    code = c(11, 12, 14, 18, 20, 23, 27, 29, 33, 35, 36, 37, 39, 42, 43, 45, 48,
             53, 54, 60, 64, 69, 71),
    value = c("Oesophageal", "Stomach", "Colorectal", "Liver", "Pancreatic",
              "Lung", "Melanoma", "Mesothelioma", "Breast", "Cervical", "Uterine", 
              "Ovarian", "Prostate", "Kidney", "Bladder", "Brain", "Thyroid",
              "Non-Hodgkin lymphoma", "Leukaemia", "Myeloma", "All Invasive", 
              "Myeloproliferative neoplasms", "Head and neck")
)

Ref.sex <- data.frame(
    code = c(1, 2, 3),
    value = c("Males", "Females", "Persons")
)

# Remove bladder (43)
Ref.cancer <- Ref.cancer[-which(Ref.cancer$value == 'Bladder'),]
row.names(Ref.cancer) <- NULL

#==========================================================================
# LOOP OVER DIFFERENT CANCER SITES/ TYPES
#==========================================================================

N <- 2148

# Truncation of box-plot extreme values
truncation <- c(1/4, 4)     # Note wave plot uses log(truncation) values

# Number of equally spaced points at which the density is to be estimated
n <- 128        # Was 64

# Initialise combined data output
Estimates <- NULL
Wave.plot.vals <- NULL
Labels <- NULL
Long.format <- NULL

for(z1 in 1:nrow(Ref.cancer)){
    temp.quant.2 <- NULL
    temp.quant.log.2 <- NULL
    temp.wave.2 <- NULL
    
    for(z2 in 1:nrow(Ref.sex)){
        c.type <- Ref.cancer$code[z1]
        sex <- Ref.sex$code[z2]
        Combo <- paste(c.type, sex, sep = "_")
        print(c(c.type, sex))
        
        if(Combo %in% c("33_1", "33_3", "35_1", "35_3", "36_1", "36_3", 
            "37_1", "37_3", "39_2", "39_3")){
            # message("Invalid cancer-sex combination.  Adding NA values.")
            
            # Insert NA values for this cancer-sex
            temp.quant <- data.frame(
                p025 = rep(NA, N),
                p10 = rep(NA, N),
                p20 = rep(NA, N),
                p50 = rep(NA, N),
                p80 = rep(NA, N),
                p90 = rep(NA, N),
                p975 = rep(NA, N),
                v = rep(NA, N)
            )
            temp.quant.log <- temp.quant[, 1:7]
            
            temp.wave <- data.frame(rep(NA, N), rep(NA, N))
        }else{
            
            fp.SIR <- fp.data(c.type, sex, current.extract, rep)
            
            if(!file.exists(fp.SIR)){
                message("File not found. Skipping this combination.")
                
                # Insert NA values for this cancer-sex
                temp.quant <- data.frame(
                    p025 = rep(NA, N),
                    p10 = rep(NA, N),
                    p20 = rep(NA, N),
                    p50 = rep(NA, N),
                    p80 = rep(NA, N),
                    p90 = rep(NA, N),
                    p975 = rep(NA, N),
                    v = rep(NA, N)
                )
                temp.quant.log <- temp.quant[, 1:7]
                
                temp.wave <- data.frame(rep(NA, N), rep(NA, N))
            }else{
                # Load SIR estimates
                test <- load(fp.SIR)    # Loads object, name stored in "test"
                # SIR <- get(test)
                # SIR <- SIR[,-1]     # Remove column of IDs
                # SIR <- as.matrix(SIR)
                
                MCMC <- get(test)
                alpha <- as.vector(MCMC$samples$beta)
                logSIR <- MCMC$samples$phi + alpha      # phi = u in our schematic (and R in documentation)
                SIR <- exp(logSIR)
                log2SIR <- log2(SIR)
                rm(list = test)
                rm(test)
                
                # N <- ncol(SIR)
                M <- nrow(SIR)
                
                # --------------------------------
                # Compute quantile values
                # --------------------------------
                temp.quant <- data.frame(
                    p025 = apply(SIR, 2, quantile, 0.025),
                    p10 = apply(SIR, 2, quantile, 0.1),
                    p20 = apply(SIR, 2, quantile, 0.2),
                    p50 = apply(SIR, 2, quantile, 0.5),
                    p80 = apply(SIR, 2, quantile, 0.8),
                    p90 = apply(SIR, 2, quantile, 0.9),
                    p975 = apply(SIR, 2, quantile, 0.975),
                    v = apply(SIR, 2, function(x) sum(x > 1)) / M
                )
                temp.quant$v <- abs(0.5 - temp.quant$v) * 2     # Convert to PP value
                
                # --------------------------------
                # Points for re-constructing density (wave) plot of log-SIR
                # --------------------------------
                
                temp.wave <- NULL
                for(i in 1:N){
                    # message(concordance$SA2_Name[which(concordance$ID_2148 == i)])
                    
                    # f <- density(SIR[,i], n = n)
                    f <- density(log2SIR[,i], n = n)
                    x.out <- f$x
                    y.out <- f$y
                                        
                    # Debugging: Test
                    # plot(f)
                    # lines(x.out, y.out, col = 'blue', type = 'p')
                    # lines(round(x.out, 2), round(y.out, 3), col = 'red', type = 'p')
                    
                    # Remove values outside truncation limits
                    # keep <- which(x.out > truncation[1] & x.out < truncation[2])
                    keep <- which(x.out > log2(truncation[1]) & x.out < log2(truncation[2]))
                    # keep <- which(x.out > log(truncation[1]) & x.out < log(truncation[2]))
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
                        # keep <- c(1, h10.L, h85.L, Mode, h85.R, h10.R, n2)
                        keep <- c(Min, h10.L, h85.L, Mode, h85.R, h10.R, Max)
                    }else{
                        keep <- which.max(y.out)
                        if(keep < n){   # Stack at lower truncation
                            keep <- 1:7
                            # x.out <- rep(log(truncation[1]), 7)
                            x.out <- rep(log2(truncation[1]), 7)
                        }else{          # Stack at upper truncation
                            keep <- (n-6):n
                            # x.out <- rep(log(truncation[2]), 7)
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
                    
                    # Debugging: Test 2
                    # plot(f)
                    ### lines(x.out, y.out, col = 'blue', type = 'p')
                    # lines(x.out[keep], y.out[keep], col = 'red', type = 'p')
                    ### lines(round(x.out[keep], 2), round(y.out[keep], 3), col = 'red', type = 'p')
                    
                    temp.wave <- rbind(temp.wave, c(x.out, y.out))
                    
                    # --------------------------------
                    # Debugging: simulate wave plot
                    # --------------------------------
                    # dat <- data.frame(
                    #     x = x.out,
                    #     y = y.out
                    # )
                    # quant <- data.frame(
                    #     x = as.numeric(log(temp.quant[i, 2:6])),
                    #     y = rep(0, 5)
                    # )
                    # ggplot(dat, aes(x = x, y = y)) +
                    #     geom_line(colour = "grey80", linetype = 3, size = 1) +
                    #     scale_x_continuous("",
                    #         breaks = log(c(0.25, 0.5, 1, 2, 4)),
                    #         limits = log(c(0.25, 4)),
                    #         labels = as.character(c(0.25, 0.5, 1, 2, 4)),
                    #         expand = c(0.01, 0.01)
                    #     ) +
                    #     scale_y_continuous("", breaks = NULL) +
                    #     coord_cartesian(ylim = c(0, 4)) +
                    #     theme(
                    #         panel.background = element_rect(fill ="grey30"),
                    #         panel.grid.major.x = element_blank(),
                    #         panel.grid.minor.x = element_blank()
                    #     ) +
                    #     geom_line(data = quant, size = 1, colour = "white") +
                    #     geom_point(data = quant,
                    #         size = c(2, 2.5, 3.5, 2.5, 2),
                    #         colour = c("grey80", "white", "white", "white", "grey80"),
                    #         fill = c("grey30", "red", "red", "red", "grey30"),
                    #         shape = 21
                    #     ) +
                    #     ggtitle(paste0(
                    #         concordance$SA2_Name[which(concordance$ID_2148 == i)], ", ",
                    #         concordance$SA2_Code_Short[which(concordance$ID_2148 == i)], " (", i, ")"
                    #     ))
                    
                    
                } # For each area
            } # If file exists
            
            # --------------------------------
            # Reformatting
            # --------------------------------
            
            # QUANTILES
            # Create log base 2 estimates (ignoring the "v" column)
            temp.quant.log <- log2(temp.quant[, 1:7])
            temp.quant.log <- data.frame(temp.quant.log)
            
            # Round estimates to 2 dp except v column (v column is 4 dp by construction)
            temp.quant[, 1:7] <- sapply(temp.quant[, 1:7], round, digits = 2)
            
            # Round logged estimates to 4 dp
            temp.quant.log <- sapply(temp.quant.log, round, digits = 4)
            
            
            # WAVE PLOT VALUES
            # Convert wave x-values to log base 2 (exponentiate then log2)
            # temp.wave[, 1:9] <- log2(exp(temp.wave[, 1:9]))       # Obsolete 17/09/18

            # Round wave x-values to 2 dp
            temp.wave[, 1:9] <- sapply(temp.wave[, 1:9], round, digits = 2)
    
            # Round wave y-values to 3 dp
            temp.wave[, 10:18] <- sapply(temp.wave[, 10:18], round, digits = 3)
            
            # Concatenate wave x and y values into strings
            temp1 <- apply(temp.wave[,1:9], 1, paste, collapse = ",")
            temp2 <- apply(temp.wave[,10:18], 1, paste, collapse = ",")
            temp.wave <- cbind(temp1, temp2)
            rm(temp1)
            rm(temp2)
            
            # Relevant year based on cancer (for DLD)
            if(c.type %in% c(14, 23, 27, 33, 39, 64)){
                y.group <- paste0(current.extract - 4, "-", current.extract)
            }else{
                y.group <- paste0(current.extract - 9, "-", current.extract)
            }
            
            # Downloadable data (only 2148 areas)
            # Rows <- match(concordance$ID_2148, concordance$ID_2193)
            Rows <- which(!is.na(concordance$ID_2148))
            Long.format <- rbind(Long.format, 
                data.frame(
                    Year = y.group,
                    Cancer_code = Ref.cancer$code[z1],
                    Cancer_name = Ref.cancer$value[z1],
                    Sex_code = Ref.sex$code[z2],
                    Sex_name = Ref.sex$value[z2],         
                    SA2_code = concordance[Rows, 6],
                    SA2_name = concordance[Rows, 8],
                    temp.quant[, c(1, 2, 4, 6, 7)],
                    Prob = round(temp.quant[, 8], 3)
                )
            )
            
            # Truncate quantiles and logged quantiles (for atlas only, not DLD)
            temp.quant[which(temp.quant[,1:7] < truncation[1], arr.ind = TRUE)] <- truncation[1]
            temp.quant[which(temp.quant[,1:7] > truncation[2], arr.ind = TRUE)] <- truncation[2]
            
            temp.quant.log[which(temp.quant.log < log2(truncation[1]), arr.ind = TRUE)] <- log2(truncation[1])
            temp.quant.log[which(temp.quant.log > log2(truncation[2]), arr.ind = TRUE)] <- log2(truncation[2])
            
        } # Combo is valid
        
        # --------------------------------
        # Combining
        # --------------------------------
        
        # Remove 95% CrI (only used for DLD)
        temp.quant <- temp.quant[, c(2:6, 8)]
        temp.quant.log <- temp.quant.log[, c(2:6)]
        
        # Combine estimate quantiles with other sexes
        if(is.null(temp.quant.2)){
            temp.quant.2 <- temp.quant
            temp.quant.log.2 <- temp.quant.log
        }else{
            temp.quant.2 <- data.frame(temp.quant.2, temp.quant)
            temp.quant.log.2 <- data.frame(temp.quant.log.2, temp.quant.log)
        }
        
        # Combine wave plot points with other sexes
        colnames(temp.wave) <- paste0("x", sex, 1:2)
        if(is.null(temp.wave.2)){
            temp.wave.2 <- temp.wave
        }else{
            temp.wave.2 <- data.frame(temp.wave.2, temp.wave)
        }
        
    }# For each sex
    # Add in rows for missing values (up to 2193 areas)
    all.rows <- match(concordance$ID_2148, concordance$ID_2193)
    temp.quant.2 <- temp.quant.2[all.rows, ]
    temp.quant.log.2 <- temp.quant.log.2[all.rows, ]
    temp.wave.2 <- temp.wave.2[all.rows, ]
    
    Labels <- rbind(Labels, data.frame(
        cancergrp = c.type,
        sa2 = concordance$SA2_Code_Short
    ))
    
    # Combine logged and non-logged estimates
    temp.quant.2 <- data.frame(temp.quant.2, temp.quant.log.2)
    
    # Append to other cancers
    Estimates <- rbind(Estimates, temp.quant.2)
    Wave.plot.vals <- rbind(Wave.plot.vals, temp.wave.2)
}



# --------------------------------
# Final format and export
# --------------------------------

# Remove row headings
row.names(Estimates) <- NULL
row.names(Wave.plot.vals) <- NULL

# Reformat column headings
names(Estimates) <- c(
    "males_p10", "males_p20", "males_p50", "males_p80", "males_p90", "males_v",
    "females_p10", "females_p20", "females_p50", "females_p80", "females_p90", "females_v",
    "persons_p10", "persons_p20", "persons_p50", "persons_p80", "persons_p90", "persons_v",
    "males_logp10", "males_logp20", "males_logp50", "males_logp80", "males_logp90",
    "females_logp10", "females_logp20", "females_logp50", "females_logp80", "females_logp90",
    "persons_logp10", "persons_logp20", "persons_logp50", "persons_logp80", "persons_logp90"
)
names(Wave.plot.vals) <- c(
    "males_xValues", "males_yValues", 
    "females_xValues", "females_yValues",
    "persons_xValues", "persons_yValues"
)

# Add Labels, Estimates, and and Wave plot values together
Combined <- data.frame(Labels, Estimates, Wave.plot.vals)

# Export
write.csv(Combined, file = "SIR Estimates for ViseR.csv", na = "", row.names = FALSE)
write.csv(Long.format, file = "SIR Downloadable Data.csv", na = "", row.names = FALSE)

# EOF

