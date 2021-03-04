
###################################################################################
## GET STATS ABOUT NUMBER OF ORIGINAL ARTICLES, BACKUP ARTICLES, AND CODED ARTICLES
###################################################################################

### Read Assigned Data
dat.assigned = read.xlsx2("Assigned Data Jan-Dec.xlsx", "Sheet1", header=TRUE)[,1:11]
dat.assigned = dat.assigned[, -which(colnames(dat.assigned) %in% c("Coder1", "Coder2"))]

### Read Backup Data
dat.backup = read.xlsx2("Backup Sample Jan-Dec.xlsx", "Sheet1", header=TRUE)[,1:9]

### Combine Assigned Data and Backup Data
dat.input.combined = rbind(cbind(status="original", dat.assigned), 
                           cbind(status="backup", dat.backup))

### Because errors in giving IDs to articles from Oct-Dec, some IDs are duplicated
### Therefore, duplicated IDs are added -2 if coming from after September
### Duplicate picks up the second appearance, so need to sort to ensure that Oct-Dec appears second
dat.input.combined = dat.input.combined[order(dat.input.combined$ID, 
                                              dat.input.combined$Cluster, 
                                              dat.input.combined$Media, 
                                              dat.input.combined$Publish.Date),]

index = which(duplicated(dat.input.combined$ID) & (as.numeric(dat.input.combined$Month) > 9))

dat.input.combined$ID[index] = paste0(dat.input.combined$ID[index],"-2")

dim(dat.input.combined)             # 9,090 articles
table(dat.input.combined$status)    # 2,750 original ; 6,340 backup


### Combine articles from all coders
list.articles = rbind(cbind(coder1 = 1, coder2 = NA, dat[[1]]), 
                      cbind(coder1 = 2, coder2 = NA, dat[[2]]), 
                      cbind(coder1 = 3, coder2 = NA, dat[[3]]), 
                      cbind(coder1 = 4, coder2 = NA, dat[[4]]), 
                      cbind(coder1 = 5, coder2 = NA, dat[[5]]), 
                      cbind(coder1 = 6, coder2 = NA, dat[[6]]))
rownames(list.articles) = 1:nrow(list.articles)

### Change id to ID to match with assigned data
index = which(colnames(list.articles) == "id")
colnames(list.articles)[index] = c("ID")

### Delete irrelevant columns from coded data
index = which(colnames(list.articles) %in% 
              c("valid", "formal.no",	"formal.media",	"formal.tanggal",	"formal.tautan",	
                "formal.arsip",	"formal.panjang",	"formal.cluster"))
list.articles = list.articles[, -index]


dim(double.coded)                 # 151 articles double coded; "d-14733-re" is recoded three times by 2,3,4
length(unique(double.coded$id))   # 149 unique articles double coded
dim(list.articles)                # 2,088 articles; include original and backup; include double-coded
length(unique(list.articles$ID))  # 1,938 unique articles

unique.articles = list.articles[!duplicated(list.articles$ID),]
dim(unique.articles)          # 1,938 articles

dat.full = merge(unique.articles, dat.input.combined, by="ID", all.x = TRUE)
dim(dat.full)


### Add second coder if double coded
list.double.coded = which(dat.full$ID %in% double.coded$id)

for (i in list.double.coded) {
  index = which(double.coded$id == dat.full[i, "ID"])[1]
  dat.full[i, "coder1"] = double.coded[index, "coder1"]
  dat.full[i, "coder2"] = double.coded[index, "coder2"]
}

sum(!is.na(dat.full$coder2))          # 149 articles have coder 2, good

colnames(dat.full) = tolower(colnames(dat.full))
write.xlsx2(dat.full, file="Out - Article Information.xlsx", row.names = FALSE)  
