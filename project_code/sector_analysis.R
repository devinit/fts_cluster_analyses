suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))

setwd(dirname(getActiveDocumentContext()$path))
setwd("..")

fts <- fread("project_data/fts_flows.csv", encoding = "UTF-8")

sector.decode <- (fts[!(sector %in% fts$destinationObjects_GlobalCluster.name), .(sector = unique(sector), new_sector = NA_character_)])
sector.decode[grepl("COVID-19", sector), new_sector := "COVID-19"]
sector.decode[grepl("Multisector|All non-COVID", sector, ignore.case = T), new_sector := "Multi-sector"]
sector.decode[grepl("WASH", sector), new_sector := "Water Sanitation Hygiene"]
sector.decode[grepl("Shelter|NFI", sector), new_sector := "Emergency Shelter and NFI"]
sector.decode[grepl("Food Security", sector), new_sector := "Food Security"]
sector.decode[is.na(new_sector), new_sector := "Other"]

fts[sector %in% sector.decode$sector, sector := merge(fts[sector %in% sector.decode$sector, .(sector)], sector.decode, by = "sector")$new_sector]

fts[sector == "", sector := "Unspecified"]
fts[grepl("Protection", sector), sector := "Protection"]

#all sectors
sectors <- fts[, .(amountUSD = sum(amountUSD)), by = .(year, sector)]
sectors <- sectors[, .(amount = sum(amountUSD)), by = .(year, sector)][order(year, sector)]

sectors[, proportion := amount/sum(amount), by = year]

sectors.cast <- dcast(sectors, sector ~ year, value.var = c("amount", "proportion"))
sectors.cast[is.na(sectors.cast)] <- 0

fwrite(sectors.cast, "output/fts_sectors.csv")

#covid emergency sectors
sectors_covid <- fts[covid_emergency == TRUE, .(amountUSD = sum(amountUSD)), by = .(year, sector)]
sectors_covid <- sectors_covid[, .(amount = sum(amountUSD)), by = .(year, sector)][order(year, sector)]

sectors_covid[, proportion := amount/sum(amount), by = year]

sectors_covid.cast <- dcast(sectors_covid, sector ~ year, value.var = c("amount", "proportion"))
sectors_covid.cast[is.na(sectors_covid.cast)] <- 0

fwrite(sectors.cast, "output/fts_covid_emergency_sectors.csv")

#sectors by plan (HRP)
fts[, plan := destinationObjects_Plan.name]
fts[is.na(plan) & !is.na(sourceObjects_Plan.name), plan := sourceObjects_Plan.name]

sectors_hrp <- fts[, .(amountUSD = sum(amountUSD)), by = .(year, plan, sector)]
sectors_hrp <- sectors_hrp[, .(amount = sum(amountUSD)), by = .(year, plan, sector)][order(year, plan, sector)]

sectors_hrp[, proportion := amount/sum(amount), by = .(year, plan)]

sectors_hrp.cast <- dcast(sectors_hrp, year + plan ~ sector, value.var = c("amount", "proportion"))
sectors_hrp.cast[is.na(sectors_hrp.cast)] <- 0

fwrite(sectors_hrp.cast, "output/fts_sectors_hrp.csv")
