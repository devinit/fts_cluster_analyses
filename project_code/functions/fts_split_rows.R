fts_split_rows <- function(data, value.cols = "amountUSD", split.col = "destinationObjects_Location.name", split.pattern = "; ", remove.unsplit = T){
  split.pattern <- trimws(split.pattern)
  temp <- data[, .(trimws(unlist(strsplit(get(split.col), split.pattern))), get(value.cols)/(1+ifelse(is.na(get(split.col)), 0, nchar(get(split.col))-nchar(gsub(split.pattern, "", get(split.col)))))), by=rownames(data)]
  if(remove.unsplit){
    names(temp) <- c("rownames", split.col, value.cols)
    data[, (split.col) := NULL]
    data[, (value.cols) := NULL]
  } else {
    names(temp) <- c("rownames", paste0(split.col, ".split"), paste0(value.cols, ".split"))
  }
  data <- merge(data[, rownames := rownames(data)], temp, by = "rownames")
  data[, rownames := NULL]
  return(data)
}
