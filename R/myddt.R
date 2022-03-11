#' Generate a plot of LENGTH vs WEIGHT of the DDT dataset.
#'
#' @import dplyr
#' @import ggplot2
#' @import Intro2R
#' @export
#'
#' @param df the dataframe to be read in (ddt.csv)
#' @param spec species of fish to be plotted
#' @return a ggplot of the given species
#' @return a named list of DDT data frame before and after subsetting and rel freq table of RIVER before subsetting
#' @examples
#' \dontrun{
#' myddt(ddt, "CCATFISH")
#' myddt(ddt, "SMBUFFALO")
#' myddt(ddt, "LMBASS")
#' }

myddt <- function(df, spec) {
  print(df)

  df1 <- filter(df, SPECIES == spec)
  print(df1)

  dffrq <- table(df$RIVER)/length(df$RIVER)
  print(dffrq)

  g <- ggplot(df1, aes_string(x="WEIGHT", y="LENGTH")) + geom_point(aes_string(color="RIVER")) + geom_smooth(formula=y~x+I(x^2), method = "lm") + ggtitle("Grant Talkington")
  print(g)

  write.csv(x=df1, file=paste("LvsWfor", spec, ".csv", sep=""), row.names=FALSE)
}
