suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))

setwd(dirname(getActiveDocumentContext()$path))
setwd("..")

fts <- fread("project_data/fts_flows.csv", encoding = "UTF-8")

sectors <- fts[, .(amountUSD = sum(amountUSD)), by = .(year, sector)]
sectors <- sectors[, .(amount = sum(amountUSD)), by = .(year, sector)][order(year, sector)]

sectors[, proportion := amount/sum(amount), by = year]

dcast(sectors, sector ~ year, value.var = c("amount", "proportion"))
