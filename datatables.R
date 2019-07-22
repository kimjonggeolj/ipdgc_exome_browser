#geneList <- fread("www/geneList.txt")
load("www/geneList.RData")
#varList <- fread("www/varList.txt")
load("www/varList.RData")
#load("www/MTvarList.RData")

#save(emptyVarList, file = "www/MTvarList.RData")

#varList <- data.table("rsID" = "rs1422910994", "id" = "LRRK2")

#below two tables are generated as samples. The real data will be fread on "geneinfo-page.R" in observeEvent
#aggregateVariantTable <- data.table("Variant Type" = c("SAMPLE", "SAMPLE"), "Count" = c(1234, 1234))
#variantTable <- data.table("Alleles (rsIDs)" = c("A/C (rsXXXXXXXXXX)"), "Consequence" = c("missense"), "CADD" = "", "N Alleles" = "ZZZZZZ", "HomAlt" = 0, "Frequency" = "1%")

# File name format for variant table:
# "varTab/[geneid].txt" in ALL lower case
#grepl("^\\d{1,2}:\\d+-\\d+$", "22:43088902-4028", ignore.case = T)