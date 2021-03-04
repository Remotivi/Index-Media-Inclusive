
####################################################################
## READ AND PREPARE DATA
####################################################################

### Giving Column Names
col.names = c("valid","id", 
              "formal.no", "formal.media", "formal.tanggal", "formal.tautan", "formal.arsip", 
              "formal.panjang", "formal.konteks", "formal.cluster", "formal.narsum",
              "standar.sumber", "standar.berimbang", "standar.privasi", "standar.perkosaan", "standar.clickbait",
              "inklusifitas.narsum", "inklusifitas.tone", "inklusifitas.istilah", "inklusifitas.jumlah", "inklusifitas.marking",
              "agenda.isu", 
              "aksesibilitas.front", "aksesibilitas.artikel", "aksesibilitas.manual")

for (i in 1:num.coders) {
  colnames(dat[[i]]) = col.names
}

colnames(dat.pengganti) = c("lama", "baru")

### Columns For Reliability and Scoring so must complete
col.must.complete = c("formal.konteks", "standar.sumber", "standar.berimbang", "standar.privasi", "standar.perkosaan", "standar.clickbait",
                    "inklusifitas.narsum", "inklusifitas.tone", "inklusifitas.jumlah", "inklusifitas.marking", "agenda.isu")

### Numeric Columns
col.numeric = c("valid", "standar.sumber", "standar.berimbang", "standar.privasi", "standar.perkosaan", "standar.clickbait",
                "inklusifitas.jumlah", "inklusifitas.marking", "agenda.isu")


### Convert Column Types to Character
for (i in 1:num.coders) {
  for (j in col.names){
    dat[[i]][, j] = as.character(dat[[i]][, j])
  }
}
dat.pengganti$lama = as.character(dat.pengganti$lama)
dat.pengganti$baru = as.character(dat.pengganti$baru)


### Convert Numeric Columns from Character to Numeric
for (i in 1:num.coders) {
  for (j in col.numeric) {
    dat[[i]][, j] = as.numeric(dat[[i]][, j])
  }
}


### Delete Irrelevant Rows, defined as Invalid==2
for (i in 1:num.coders) {
  dat[[i]] = subset(dat[[i]], subset=valid==1)
  
  # Reset rownames because otherwise confusing
  rownames(dat[[i]]) = 1:nrow(dat[[i]])
}



valid.values = c("konflik (1), everyday life (2), seremoni (3), kebijakan (4),lain-lain (5)",
                 "valid (1), invalid (2)",
                 "tidak ada (-1), ada (0)",
                 "tidak berimbang (-1), not applicable (0), berimbang (1)",
                 "memuat data pribadi (-1), tidak memuat (0)",
                 "eksploitatif (-1), not applicable (0), tidak eksploitatif (1)",
                 "clickbait (-1), tidak (0)",
                 "0 ... ",
                 "ya (-1), tidak (0)",
                 "episodic (0), thematic (1)"
)
names(valid.values) = c("formal.konteks", col.numeric)

## formal.konteks: konflik (1), everyday life (2), seremoni (3), kebijakan (4),lain-lain (5)
## valid: valid (1), invalid (2)
## standar.sumber: tidak ada (-1), ada (0)
## standar.berimbang: tidak berimbang (-1), not applicable (0), berimbang (1)
## standar.privasi: memuat data pribadi (-1), tidak memuat (0)
## standar.perkosaan: eksploitatif (-1), not applicable (0), tidak eksploitatif (1)
## standar.clickbait: clickbait (-1), tidak (0)
## inklusifitas.narsum: for each narsum, tak teridentifikasi (-1), non-marjinal (0), lembaga marjinal (1), individu marjinal (2)
## inklusifitas.tone: for each narsum, negatif (-1), netral (0), positif (1)
## inklusifitas.jumlah: 0 ..
## inklusifitas.marking: ya (-1), tidak (0)
## agenda.isu = episodic (0), thematic (1)


### Turn formal.konteks to factor
for (i in 1:num.coders) {
  dat[[i]][, "formal.konteks"] = as.factor(dat[[i]][, "formal.konteks"])
}


### Check whether all responses are valid
sink("Out - Response Tabulation.rtf")
for (i in 1:num.coders) {
  cat(paste("=============== Coder:",i, "===============\n"))
  for (j in c("formal.konteks", col.numeric)) {
    cat(paste("Variable:",j, "\n"))
    cat("Response options:", valid.values[j], "\n")
    cat("Actual responses: ")
    print(table(dat[[i]][,j]))
    cat("\n")
  }  
}
sink(NULL)


######################################################################
### Fixing consequences of incorrect article IDs by changing manually
######################################################################

### i-204-ko
index = which(dat[[1]][, "id"] == "i-204-ko")
dat[[1]][index, "id"] = "i-204-ko-2"

index = which(dat[[3]][, "id"] == "i-204-ko")
dat[[3]][index, "id"] = "i-204-ko-2"

### "d-14733-re" ---> coded by coders 2,3,4

### "i-88-ti"
index = which(dat[[5]][, "id"] == "i-88-ti")[2]
dat[[5]][index, "id"] = "i-88-ti-2"


