#
# PROGRAMMING LANGUAGE:
# S-language (for use in R)
#
# FUNCTION NAME:
# readSAGOB.sl
#
# PURPOSE:
# To read ASCII files generated from SAGOB data containing AVHRR
# information (channel info and angles).
# 
# NOTES:
# NA
#
# BUGS:
# NA
#
# AUTHOR:
# Øystein Godøy, DNMI/FOU, 13/06/2001
# MODIFIED:
# Øystein Godøy, DNMI/FOU, 02/11/2001
# Changed basepath...
# Øystein Godøy, met.no/FOU, 14.04.2004
# Changed basepath...
#
readSAGOB <- function(filename="") {
#    basepath <- "/raid1/sat/fase/SAF/res_anabase"
#    basepath <- "/disk2/SAGOB/saf"
#    basepath <- "/disk2/pdfavhrris"
#    filename <- paste(basepath,file,sep="/")

# Read Header
# Could not decode names in list...
    header <- scan(filename,nlines=4,what=character(),sep="\n")
    class <- substring(header[1],17)
    pnames <- strsplit(substring(header[4],19)," ")
    pnames<-pnames[[1]]

# Read Data
    dm <- read.table(filename,header=F,skip=5)

#    len <- length(dm[,1])

# Check and adjust for missing values
    dm[,1][dm[,7] < 200.0] <- NA
    dm[,1][dm[,8] < 200.0] <- NA
    dm[,1][dm[,9] < 200.0] <- NA
    dm[,1][is.na(dm[,5])] <- NA
    dm[,2][is.na(dm[,5])] <- NA
    dm[,3][is.na(dm[,5])] <- NA
    dm[,4][is.na(dm[,5])] <- NA
    dm[,6][is.na(dm[,5])] <- NA
    dm[,7][is.na(dm[,5])] <- NA 
    dm[,8][is.na(dm[,5])] <- NA
    dm[,9][is.na(dm[,5])] <- NA

# Correct for solar zenith angle
    dm[,5] <- dm[,5]/cos(dm[,1]*pi/180.0)
    dm[,6] <- dm[,6]/cos(dm[,1]*pi/180.0)

# Create new feature (A2/A1)
    dm <- cbind(dm,dm[,6]/dm[,5])
    pnames <- c(pnames,"CH2/CH1")

# Create new feature (T3-T4)
    dm <- cbind(dm,dm[,7] - dm[,8])
    pnames <- c(pnames,"CH3-CH4")

# Create new feature (T4-T5)
    dm <- cbind(dm,dm[,8] - dm[,9])
    pnames <- c(pnames,"CH4-CH5")

# Create new angle (RaZ)
    dm <- cbind(dm,dm[,4] - dm[,3])
    pnames <- c(pnames,"RaZ")

# Update column names
    colnames(dm) <- pnames

# Create a list object containing name etc...
    ldm <- list(class,dm)
    names(ldm) <- c("class","data")
    return(ldm)
}
