#' Create a simple plot with a linear regression line
#'
#' @param df Data table
#' @param y Dependent variable name
#' @param x Independent variable name
#' @param xlb x-axis label
#' @param ylb y-axis label
#' @return A plot with independent and dependent variables labelled with a linear regression line.
#' @examples
#' \dontrun{
#' spruce.df <- read.csv("SPRUCE.csv")
#' linearscatter(spruce.df, "Height", "BHDiameter", "Breast Height Diameter (cm)", "Height (m)")
#' }

linearscatter <- function(df, y, x, xlb, ylb) {
  dataf = df
  dataf.lm <- lm(data=df, y~x)

  with(dataf, plot(y~x, xlim=c(0, 1.1*max(x)), ylim=c(0, 1.1*max(y)), xlab=xlb, ylab=ylb, pch=21, bg="Blue", cex=1.2))
  abline(dataf.lm)
}
