suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))

#Load FTS utility functions
setwd(dirname(getActiveDocumentContext()$path))
lapply(c("functions/fts_get_flows.R", "functions/fts_unnest_flows.R", "functions/fts_split_rows.R"), source)
setwd("..")

#Download FTS flows and unnest from JSON
fts_raw <- fts_get_flows(year = c(2015:2021))
fts <- fts[status == "paid"]
fts <- fts_unnest_flows(fts_raw)

#Split rows into individual years where multiple are recorded
fts[, year := destinationObjects_UsageYear.name]
fts <- fts_split_rows(fts, value.cols = "amountUSD", split.col = "year", split.pattern = "; ", remove.unsplit = T)

#Assign a sector from available fields
fts[, sector := destinationObjects_GlobalCluster.name]
fts[is.na(sector) & !is.na(sourceObjects_GlobalCluster.name), sector := sourceObjects_GlobalCluster.name]
fts[is.na(sector) & !is.na(destinationObjects_Cluster.name), sector := destinationObjects_Cluster.name]

#Split rows into individual sectors where multiple are recorded
fts <- fts_split_rows(fts, value.cols = "amountUSD", split.col = "sector", split.pattern = "; ", remove.unsplit = T)

#Split rows into individual recipient type where multiple are recorded
fts$recipient_type <- apply(matrix(paste(as.matrix(fts[, tstrsplit(destinationObjects_Organization.organizationTypes, "; ")]), as.matrix(fts[, tstrsplit(destinationObjects_Organization.organizationSubTypes, "; ")]), sep = ": "), nrow = nrow(fts)), 1, function(x) gsub("NA|: NA|; NA: NA|: NULL", "", paste(x, collapse = "; ")))
fts <- fts_split_rows(fts, value.cols = "amountUSD", split.col = "recipient_type", split.pattern = "; ", remove.unsplit = T)

fts <- fts[year %in% c(2015:2021)]
fts[is.null(fts) | fts == "NULL"] <- NA
fts[, `:=` (reportDetails = NULL, childFlowIds = NULL)]

fts[grepl(911, sourceObjects_Emergency.id)|grepl(911, destinationObjects_Emergency.id)|
           grepl(952, sourceObjects_Plan.id)|grepl(952, destinationObjects_Plan.id), covid_emergency := TRUE]

fwrite(fts, "project_data/fts_flows.csv")
