suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))

#Load FTS utility functions
setwd(dirname(getActiveDocumentContext()$path))
lapply(c("functions/fts_get_flows.R", "functions/fts_unnest_flows.R", "functions/fts_split_rows.R", "functions/fts_appeals_data.R"), source)
setwd("..")

#Download FTS flows and unnest from JSON
fts_raw <- fts_get_flows(year = c(2015:2021))
fts <- fts_unnest_flows(fts_raw)

#Only disbursements
fts <- fts[status == "paid"]

#Get plan years
appeal_list <- fts_get_appeal_urls(2015:2021)

fts[, plan_id := destinationObjects_Plan.id]
fts[is.na(plan_id) & !is.na(sourceObjects_Plan.id), plan_id := sourceObjects_Plan.id]

fts <- merge(fts, appeal_list[, .(plan_id = id, plan_year = as.character(year))], by = "plan_id", all.x = T)
fts[is.na(plan_year), plan_year := destinationObjects_UsageYear.name]

#Split rows into individual years where multiple are recorded
fts <- fts_split_rows(fts, value.cols = "amountUSD", split.col = "plan_year", split.pattern = "; ", remove.unsplit = T)
fts[, `:=` (year = plan_year, plan_year = NULL)]

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
