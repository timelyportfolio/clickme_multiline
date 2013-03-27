#' Translate the data object to the format expected by current template
#'
#' @param data input data object
#' @param opts options of current template
#' @return The opts variable with the opts$data variable filled in
translate <- function(data, opts = NULL) {
  require(df2json)
  
  #I would like to generalize this to handle both price and return
  #right now just handles return
  #clickme template.Rmd javascript can handle prices or cumulative
  #so we will send cumulative which can serve as price
  
  #remove na
  data[is.na(data)] <- 0
  #get cumulative growth
  data <- cumprod(1+data)
  
  
  #convert to data frame
  data.df <- data.frame(cbind(format(index(data),"%Y-%m-%d"),coredata(data)))
  colnames(data.df)[1] = "date"
  data.melt <- melt(data.df,id.vars=1,stringsAsFactors=FALSE)
  colnames(data.melt) <- c('date','indexname','price')
  #remove periods from indexnames to prevent javascript confusion
  #these . usually come from spaces in the colnames when melted
  data.melt[,"indexname"] <- apply(matrix(data.melt[,"indexname"]),2,gsub,pattern="[.]",replacement="")
  opts$data <- df2json(data.melt)
  opts
}
