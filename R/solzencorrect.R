solzencorrect <- function(soz,data) {

    DEG2RAD <- pi/180.

    t <- data/cos(DEG2RAD*soz)

    return(t)
}
