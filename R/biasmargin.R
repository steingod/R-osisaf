#
# $Id: biasmargin.R,v 1.1 2012-04-13 22:36:12 steingod Exp $
#

biasmargin <- function(meanobs,bias,threshold=10) {

    thresholdmargin <- threshold*meanobs/100

    bmargin <- 100.*(1-abs(bias/thresholdmargin))

    return(bmargin)
}
