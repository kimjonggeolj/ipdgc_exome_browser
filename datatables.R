if (isDemo == T) {
  load("demodata/searchLists/geneList.RData")
  
  load("demodata/searchLists/varList.RData")
  
  load("demodata/searchLists/rsID.List.RData")
  
  # load("demodata/searchLists/uniprot.id.list.RData") # add if uniprot domains added
} else {
  load("data/searchLists/geneList.RData")
  
  load("data/searchLists/varList.RData")
  
  load("data/searchLists/rsID.List.RData")
  
  # load("data/searchLists/uniprot.id.list.RData") # add if uniprot domains added
}