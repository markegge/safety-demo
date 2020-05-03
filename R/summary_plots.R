#' Plot a histogram for each numeric variable, and bar chart for 
#' factors - works best with StringsAsFactors
#'
#' @param df A data.frame-like object
#' @param out_file Path and Name of the PDF file to output
#' @return None
summary_plots <- function(df, out_file = "out/summary_plots.pdf") {
  if(out_file == "out/summary_plots.pdf")
    dir.create("out", showWarnings = FALSE)
  
  pdf(file = out_file, paper = "letter", w = 8, h = 9)
  par(mfrow =c (3,3))
  par(mar = c(5.1, 4.1, 4.1, 2.1))
  for(col in colnames(df)) {
    if(is.character(df[[col]]) &&
       length(na.omit(unique(df[[col]]))) > 0 &&
       length(unique(df[[col]])) < 50) {
      df[[col]] <- as.factor(df[[col]])
    }
    
    if(is.factor(df[[col]])) {
      tryCatch(barplot(table(df[[col]], exclude = c("", NA, NaN)), main=col),
               error = function(e) {
                 print(paste("error plotting", col))
                 NaN
               }
      )
    } else if (is.numeric(df[[col]])) {
      hist(df[[col]], main=col)
    }
  }
  dev.off()
}