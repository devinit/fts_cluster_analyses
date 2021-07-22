suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))

setwd(dirname(getActiveDocumentContext()$path))
lapply(c("functions/fts_appeals_data.R"), source)

appeal_list <- fts_get_appeal_urls(2015:2021)

appeal_requirements <- list()
pb <- txtProgressBar(max = nrow(appeal_list), style = 3)
for(i in 1:nrow(appeal_list)){
  appeal_requirements[[i]] <- fts_get_appeal_requirements(appeal_list$id[i])
  setTxtProgressBar(pb, i)
}
appeal_requirements <- rbindlist(appeal_requirements, fill = T)
close(pb)

appeal_clusters <- list()
pb <- txtProgressBar(max = nrow(appeal_list), style = 3)
for(i in 1:nrow(appeal_list)){
  appeal_clusters[[i]] <- fts_get_appeal_clusters(appeal_list$id[i])
  setTxtProgressBar(pb, i)
}
appeal_clusters <- rbindlist(appeal_clusters, fill = T)
close(pb)

appeal_clusters <- appeal_clusters[, lapply(.SD, function(x) as.numeric(gsub(",", "", x))), .SDcols = grep("US[$]", names(appeal_clusters), value = T), by = .(plan_name, Cluster)]
appeal_clusters[, coverage := `Funding US$`/`Current requirements US$`]

fwrite(appeal_clusters, "output/appeal_clusters.csv")
