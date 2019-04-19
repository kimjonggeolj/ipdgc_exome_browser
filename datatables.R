#geneList <- fread("www/geneList.txt")


#below two tables are generated as samples. The real data will be fread on "geneinfo-page.R" in observeEvent
#aggregateVariantTable <- data.table("Variant Type" = c("SAMPLE", "SAMPLE"), "Count" = c(1234, 1234))
#variantTable <- data.table("Alleles (rsIDs)" = c("A/C (rsXXXXXXXXXX)"), "Consequence" = c("missense"), "CADD" = "", "N Alleles" = "ZZZZZZ", "HomAlt" = 0, "Frequency" = "1%")

# File name format for variant table:
# "varTab/[geneid].txt" in ALL lower case