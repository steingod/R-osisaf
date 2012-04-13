#
# $Id: sdevmargin.R,v 1.1 2012-04-13 22:36:12 steingod Exp $
#

sdevmargin <- function(meanobs,sdev,threshold=10) {

    thresholdmargin <- threshold*meanobs/100.

    sdevmargin <- 100.*(1-abs(sdev/thresholdmargin))

    return(sdevmargin)
}

