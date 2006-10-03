#
# PROGRAMMING LANGUAGE:
# S-language (for use in R)
#
# FUNCTION NAME:
# gammamulti.sl
#
# PURPOSE:
# To present several features in one graphical view.
# 
# NOTES:
# NA
#
# BUGS:
# NA
#
# AUTHOR:
# Øystein Godøy, DNMI/FOU, 13/06/2001
#
gammaPDFestSAGOBmulti <- function(basepath,month="mar", class="water", overcast="c", method="F") {

    filename <- paste(basepath,"/",class,"_",month,"00_",overcast,".txt",sep="")

    split.screen(c(2,2))

    screen(1)
    par(pty="s",cex=0.3)
    gammaPDFestSAGOB(filename,feat="CH2",res=0.01,m=method)

    screen(2)
    par(pty="s",cex=0.3)
    gammaPDFestSAGOB(filename,feat="CH2/CH1",res=0.01,m=method)

    screen(3)
    par(pty="s",cex=0.3)
    gammaPDFestSAGOB(filename,feat="CH3-CH4",res=0.01,m=method)

    screen(4)
    par(pty="s",cex=0.3)
    gammaPDFestSAGOB(filename,feat="CH4-CH5",res=0.01,m=method)

    close.screen()
}
