

####################################################################
## PRE-PROCESS DATA AND RECODE VARIABLES FOR SCORING
####################################################################

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


### Give NAs to standard.berimbang and standar.perkosaan that are 0 (not applicable)
for (i in 1:num.coders) {
  for (j in c("standar.berimbang", "standar.perkosaan")) {
    dat[[i]][,j] = recode(dat[[i]][,j], "0 = NA; -1 = 0")
  }
}

### Eliminate negative score by adding 1 for:
### standar.sumber, standar.privasi, standar.clickbait, inklusifitas.marking
for (i in 1:num.coders) {
  list.col = c("standar.sumber", "standar.privasi",
               "standar.clickbait", "inklusifitas.marking")
  dat[[i]][,list.col] = dat[[i]][,list.col] + 1
}



### Calculate number and proportion of inklusifitas.narsum and inklusifitas.tone
options(warn = 2)
for (i in 1:num.coders) {
  
  dat[[i]][, "num.ind.marginal"] = dat[[i]][, "num.lembaga.marginal"] = 
    dat[[i]][, "num.nonmarginal"] = dat[[i]][, "num.unidentified"] = NA
  dat[[i]][, "pr.ind.marginal"] = dat[[i]][, "pr.lembaga.marginal"] = 
    dat[[i]][, "pr.nonmarginal"] = dat[[i]][, "pr.unidentified"] = NA
  
  dat[[i]][, "num.tone.negatif"] = dat[[i]][, "num.tone.netral"] = dat[[i]][, "num.tone.positif"] = NA
  dat[[i]][, "pr.tone.negatif"] = dat[[i]][, "pr.tone.netral"] = dat[[i]][, "pr.tone.positif"] = NA
  
  dat[[i]][, "jumlah.narsum"] = 0
  
  
  for (j in 1:nrow(dat[[i]])) {
    
    if (dat[[i]][j, "inklusifitas.narsum"] != "") {
      vec.inklusifitas = as.numeric(str_extract_all(dat[[i]][j, "inklusifitas.narsum"],"[^*]+")[[1]])
      vec.tone = as.numeric(str_extract_all(dat[[i]][j, "inklusifitas.tone"],"[^*]+")[[1]])
      
      dat[[i]][j,"jumlah.narsum"] = length(vec.inklusifitas)

      dat[[i]][j, "num.ind.marginal"] = sum(vec.inklusifitas == 2)
      dat[[i]][j, "num.lembaga.marginal"] = sum(vec.inklusifitas == 1)
      dat[[i]][j, "num.nonmarginal"] = sum(vec.inklusifitas == 0)
      dat[[i]][j, "num.unidentified"] = sum(vec.inklusifitas == -1)
      
      dat[[i]][j, "pr.ind.marginal"] = dat[[i]][j, "num.ind.marginal"] / dat[[i]][j, "jumlah.narsum"]
      dat[[i]][j, "pr.lembaga.marginal"] = dat[[i]][j, "num.lembaga.marginal"] / dat[[i]][j, "jumlah.narsum"]
      dat[[i]][j, "pr.nonmarginal"] = dat[[i]][j, "num.nonmarginal"] / dat[[i]][j, "jumlah.narsum"]
      dat[[i]][j, "pr.unidentified"] = dat[[i]][j, "num.unidentified"] / dat[[i]][j, "jumlah.narsum"]
      
      dat[[i]][j, "num.tone.negatif"] = sum(vec.tone == -1)
      dat[[i]][j, "num.tone.netral"] = sum(vec.tone == 0)
      dat[[i]][j, "num.tone.positif"] = sum(vec.tone == 1)
      
      dat[[i]][j, "pr.tone.negatif"] = dat[[i]][j, "num.tone.negatif"] / dat[[i]][j, "jumlah.narsum"]
      dat[[i]][j, "pr.tone.netral"] = dat[[i]][j, "num.tone.netral"] / dat[[i]][j, "jumlah.narsum"]
      dat[[i]][j, "pr.tone.positif"] = dat[[i]][j, "num.tone.positif"] / dat[[i]][j, "jumlah.narsum"]
    }
  }
}
options(warn = 0)


### generate score based on proportions of inklusifitas.narsum and inklusifitas.tone
# note: tak teridentifikasi is given the same point as non-marjinal
# note: for tone, score is added +1 to avoid negative score
# inklusifitas.narsum: for each narsum, tak teridentifikasi (0), non-marjinal (0), lembaga marjinal (0.5), individu marjinal (1)
# inklusifitas.tone: for each narsum, negatif (0), netral (0.5), positif (1)

options(warn = 2)
for (i in 1:num.coders){
  dat[[i]][,"score.inklusifitas.narsum"] = dat[[i]][,"score.inklusifitas.tone"] = 0
  
  dat[[i]][,"score.inklusifitas.narsum"] =  dat[[i]][,"pr.ind.marginal"] * 1 +
    dat[[i]][,"pr.lembaga.marginal"] * .5 +
    dat[[i]][,"pr.nonmarginal"] * 0 +
    dat[[i]][,"pr.unidentified"] * 0
  
  dat[[i]][,"score.inklusifitas.tone"] =  dat[[i]][,"pr.tone.positif"] * 1 +
    dat[[i]][,"pr.tone.netral"] * .5 +
    dat[[i]][,"pr.tone.negatif"] * 0
  
  index.missing = is.na(dat[[i]][,"score.inklusifitas.narsum"])
  dat[[i]][index.missing,"score.inklusifitas.narsum"] = 0;
  
  index.missing = is.na(dat[[i]][,"score.inklusifitas.tone"])
  dat[[i]][index.missing, "score.inklusifitas.tone"] = 0;
  
  # for (j in 1:nrow(dat[[i]])) {
  #   
  # }
}
options(warn = 0)


