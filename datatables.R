geneList <- fread("www/geneList.txt")

aggregateVariantTable <- data.table("Variant Type" = c("SAMPLE", "SAMPLE"), "Count" = c(1234, 1234))

variantTable <- data.table("Alleles (rsIDs)" = c("A/C (rsXXXXXXXXXX)"), "Consequence" = c("missense"), "CADD" = "", "N Alleles" = "ZZZZZZ", "HomAlt" = 0, "Frequency" = "1%")