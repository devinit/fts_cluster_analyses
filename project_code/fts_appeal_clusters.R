suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))

setwd(dirname(getActiveDocumentContext()$path))
lapply(c("functions/fts_appeals_data.R"), source)
setwd("..")

appeal_list <- fts_get_appeal_urls(2015:2021)

appeal_requirements <- list()
pb <- txtProgressBar(max = nrow(appeal_list), style = 3)
for(i in 1:nrow(appeal_list)){
  appeal_requirements[[i]] <- fts_get_appeal_requirements(appeal_id = appeal_list$id[i], year = appeal_list$year[i])
  setTxtProgressBar(pb, i)
}
appeal_requirements <- rbindlist(appeal_requirements, fill = T)
close(pb)

appeal_clusters <- list()
pb <- txtProgressBar(max = nrow(appeal_list), style = 3)
for(i in 1:nrow(appeal_list)){
  appeal_clusters[[i]] <- fts_get_appeal_clusters(appeal_id = appeal_list$id[i], year = appeal_list$year[i])
  setTxtProgressBar(pb, i)
}
appeal_clusters <- rbindlist(appeal_clusters, fill = T)
close(pb)

appeal_clusters <- appeal_clusters[, lapply(.SD, function(x) as.numeric(gsub(",", "", x))), .SDcols = grep("US[$]", names(appeal_clusters), value = T), by = .(year, plan_name, Cluster)]
appeal_clusters[, coverage := `Funding US$`/`Current requirements US$`]

fwrite(appeal_clusters, "project_data/appeal_clusters.csv")

cluster.map <- fread("project_data/appeal_cluster_mapping.csv", encoding = "UTF-8")

cluster.map[, `:=` (caps_cluster = toupper(Cluster), Cluster = NULL)]
cluster.map <- unique(cluster.map)
appeal_clusters[, caps_cluster := toupper(Cluster)]

appeal_clusters <- merge(appeal_clusters, cluster.map, by = "caps_cluster")[, caps_cluster := NULL][order(year, plan_name, `Global cluster`, Cluster)]

fwrite(appeal_clusters, "output/appeal_clusters_global.csv")

cluster_years <- appeal_clusters[, lapply(.SD, function(x) sum(x, na.rm = T)), .SDcols = grep("US[$]", names(appeal_clusters), value = T), by = .(year, `Global cluster`)]

fwrite(cluster_years, "output/year_cluster_requirements.csv")
