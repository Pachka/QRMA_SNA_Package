# Merging data
#
# This function merges several data.frame
# @param x first data.frame (by default, x)
# @param y second data.frame (by defaut, y)
# @keywords fusion data.frame
#
# Cette fonction sert a fusionner des data.frame
# @param x la première data.frame (par defaut, x)
# @param y la seconde data.frame (par defaut, y)
# @keywords fusion data.frame
#
MyMerge         <- function(x, y){
  df            <- merge(x, y, by= "row.names", all.x= F, all.y= F)
  rownames(df)  <- df$Row.names
  df$Row.names  <- NULL
  return(df)
}
