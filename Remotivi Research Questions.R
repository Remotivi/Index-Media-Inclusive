
####################################################################
## ANSWERING RESEARCH QUESTIONS
####################################################################

decimal = 3

rtffile <- RTF("Out - Results of Analysis.doc")

####################################################################
### Jumlah coded news yang valid (secara umum, tiap media, per klaster)
####################################################################

# Total
nrow(dat.full)

# Per Cluster/Media
out = table(dat.full$media, dat.full$cluster)
out

addHeader(rtffile, "Jumlah coded news yang valid (secara umum, tiap media, per klaster)", font.size = 16)
addTable(rtffile, out, col.justify = "C")

####################################################################
### Pada "konteks berita" apa pembahasan marjinalitas muncul (secara umum)
####################################################################

dat.full$formal.konteks.lab = NA
dat.full$formal.konteks.lab[dat.full$formal.konteks=="1"] = "Konflik"
dat.full$formal.konteks.lab[dat.full$formal.konteks=="2"] = "Everyday Life"
dat.full$formal.konteks.lab[dat.full$formal.konteks=="3"] = "Seremoni"
dat.full$formal.konteks.lab[dat.full$formal.konteks=="4"] = "Kebijakan"
dat.full$formal.konteks.lab[dat.full$formal.konteks=="5"] = "Lain-Lain"

out.1 = table(dat.full$formal.konteks.lab)
out.2 = table(dat.full$cluster, dat.full$formal.konteks.lab)
out.3 = table(dat.full$media, dat.full$formal.konteks.lab)

addPageBreak(rtffile)
addHeader(rtffile, "Pada konteks berita apa pembahasan marjinalitas muncul (secara umum)", font.size = 16)
addTable(rtffile, out.1, col.justify = "C")
addNewLine.RTF(rtffile, n=5)
addTable(rtffile, round(prop.table(out.1), decimal), col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph.RTF(rtffile, "Jumlah artikel per cluster")
addTable(rtffile, out.2, col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph.RTF(rtffile, "Persentase per cluster")
addTable(rtffile, round(prop.table(out.2,1), decimal), col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph.RTF(rtffile, "Jumlah artikel per media")
addTable(rtffile, out.3, col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph.RTF(rtffile, "Persentase per media")
addTable(rtffile, round(prop.table(out.3,1), decimal), col.justify = "C")


####################################################################
### Pada "konteks berita" apa pembahasan marjinalitas muncul (per klaster)
####################################################################

temp.dat = subset(dat.full, subset = cluster=="agama marjinal")
out.agama = table(temp.dat$media, temp.dat$formal.konteks.lab)

temp.dat = subset(dat.full, subset = cluster=="disabilitas")
out.disabilitas = table(temp.dat$media, temp.dat$formal.konteks.lab)

temp.dat = subset(dat.full, subset = cluster=="identitas gender dan orientasi seksual")
out.gender = table(temp.dat$media, temp.dat$formal.konteks.lab)

temp.dat = subset(dat.full, subset = cluster=="perempuan dalam kekerasan")
out.perempuan = table(temp.dat$media, temp.dat$formal.konteks.lab)


addPageBreak(rtffile)
addHeader(rtffile, "Pada konteks berita apa pembahasan marjinalitas muncul (per klaster)", font.size = 16)
addParagraph.RTF(rtffile, "Cluster Agama")
addTable(rtffile, out.agama, col.justify = "C")
addNewLine.RTF(rtffile, n=5)
addTable(rtffile, round(prop.table(out.agama,1), decimal), col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph.RTF(rtffile, "Cluster Disabilitas")
addTable(rtffile, out.disabilitas, col.justify = "C")
addNewLine.RTF(rtffile, n=5)
addTable(rtffile, round(prop.table(out.disabilitas,1), decimal), col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph.RTF(rtffile, "Cluster Identitas Gender")
addTable(rtffile, out.gender, col.justify = "C")
addNewLine.RTF(rtffile, n=5)
addTable(rtffile, round(prop.table(out.gender,1), decimal), col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph.RTF(rtffile, "Cluster Perempuan")
addTable(rtffile, out.perempuan, col.justify = "C")
addNewLine.RTF(rtffile, n=5)
addTable(rtffile, round(prop.table(out.perempuan,1), decimal), col.justify = "C")
addNewLine.RTF(rtffile, n=5)

####################################################################
### Perbandingan antara thematic framing vs episodic framing (per media dan per klaster)
####################################################################

dat.full$agenda.isu.lab = NA
dat.full$agenda.isu.lab[dat.full$agenda.isu==0] = "Episodik"
dat.full$agenda.isu.lab[dat.full$agenda.isu==1] = "Tematik"

out.1 = table(dat.full$agenda.isu.lab)
out.1
out.2 = table(dat.full$cluster, dat.full$agenda.isu.lab)
out.2
prop.table(out.2,1)

out.3 = table(dat.full$media, dat.full$agenda.isu.lab)
out.3
prop.table(out.3,1)


addPageBreak(rtffile)
addHeader(rtffile, "Perbandingan antara thematic framing vs episodic framing (per media dan per klaster)", font.size = 16)
addTable(rtffile, out.1, col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph.RTF(rtffile, "Jumlah Artikel Per Cluster")
addTable(rtffile, out.2, col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph.RTF(rtffile, "Proporsi Artikel Per Cluster")
addTable(rtffile, round(prop.table(out.2, 1), decimal), col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph.RTF(rtffile, "Jumlah Artikel Per Media")
addTable(rtffile, out.3, col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph.RTF(rtffile, "Proporsi Artikel Per Media")
addTable(rtffile, round(prop.table(out.3, 1), decimal), col.justify = "C")


####################################################################
### Berita tanpa sumber (secara umum, per media, dan per klaster)
####################################################################

dat.full$with.sumber.lab = "Tanpa Sumber"
dat.full$with.sumber.lab[dat.full$standar.sumber==1] = "Dengan Sumber"

out.1 = table(dat.full$with.sumber.lab)   
out.1
round(prop.table(out.1), decimal)

out.2 = table(dat.full$cluster, dat.full$with.sumber.lab)
out.2
round(prop.table(out.2, 1), decimal)

out.3 = table(dat.full$media, dat.full$with.sumber.lab)
out.3
round(prop.table(out.3, 1), decimal)

addPageBreak(rtffile)
addHeader(rtffile, "Berita tanpa sumber (secara umum, per media, dan per klaster)", font.size = 16)
addTable(rtffile, out.1, col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addTable(rtffile, round(prop.table(out.1), decimal), col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph.RTF(rtffile, "Jumlah Artikel Per Cluster")
addTable(rtffile, out.2, col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph.RTF(rtffile, "Proporsi Artikel Per Cluster")
addTable(rtffile, round(prop.table(out.2, 1), decimal), col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph.RTF(rtffile, "Jumlah Artikel Per Media")
addTable(rtffile, out.3, col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph.RTF(rtffile, "Proporsi Artikel Per Media")
addTable(rtffile, round(prop.table(out.3, 1), decimal), col.justify = "C")


####################################################################
### Berita tanpa narasumber (secara umum, per media, dan per klaster)
####################################################################

dat.full$with.narsum.lab = "Tanpa Narsum"
dat.full$with.narsum.lab[dat.full$jumlah.narsum>0] = "Dengan Narsum"

out.1 = table(dat.full$with.narsum.lab)   
out.1
round(prop.table(out.1), decimal)

out.2 = table(dat.full$cluster, dat.full$with.narsum.lab)
out.2
round(prop.table(out.2, 1), decimal)

out.3 = table(dat.full$media, dat.full$with.narsum.lab)
out.3
round(prop.table(out.3, 1), decimal)

addPageBreak(rtffile)
addHeader(rtffile, "Berita tanpa narasumber (secara umum, per media, dan per klaster)", font.size = 16)
addTable(rtffile, out.1, col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addTable(rtffile, round(prop.table(out.1), decimal), col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph.RTF(rtffile, "Jumlah Artikel Per Cluster")
addTable(rtffile, out.2, col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph.RTF(rtffile, "Proporsi Artikel Per Cluster")
addTable(rtffile, round(prop.table(out.2, 1), decimal), col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph.RTF(rtffile, "Jumlah Artikel Per Media")
addTable(rtffile, out.3, col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph.RTF(rtffile, "Proporsi Artikel Per Media")
addTable(rtffile, round(prop.table(out.3, 1), decimal), col.justify = "C")


####################################################################
### Istilah apa saja yang bermasalah? 
####################################################################

list.istilah = ("PLACEHOLDER TO DELETE")

for (i in 1:nrow(dat.full)){
    ### if n/a
    if (dat.full[i , "inklusifitas.istilah"] == "n/a") next;
    
    ### if -
    if (dat.full[i , "inklusifitas.istilah"] == "-") next;
    
    split = strsplit(dat.full[i , "inklusifitas.istilah"], "\\*\\*\\*")[[1]]
    list.istilah = c(list.istilah, split)
} # end i for


for (i in 1:length(list.istilah)) {
  ### some istilah use quotation "", remove quotations
  list.istilah[i] =  gsub("\"", "", list.istilah[i])

  ### make lower case
  list.istilah[i] =  tolower(list.istilah[i])
}

### delete placeholder
list.istilah = list.istilah[-which(list.istilah == "placeholder to delete")]

out.1 = data.frame(table(list.istilah))
out.1 = out.1[order(out.1$Freq, decreasing=TRUE),]

### one observation just gives -1***-1***-1***-1***-1, so delete -1
### not fixing the dataset because it's not going to be calculated for score
out.1 = out.1[which(out.1$list.istilah != "-1"), ]


addPageBreak(rtffile)
addHeader(rtffile, "Istilah Apa Saja Yang Bermasalah", font.size = 16)
addTable(rtffile, out.1, col.justify = c("L","C"))


####################################################################
### Perbandingan narasumber individu marjinal vs lembaga marjinal vs non-marjinal (umum, per media, per klaster)
####################################################################

out.1 = data.frame(individu.marginal = sum(dat.full$num.ind.marginal, na.rm=TRUE),
                   lembaga.marginal = sum(dat.full$num.lembaga.marginal, na.rm=TRUE),
                   non.marginal = sum(dat.full$num.nonmarginal, na.rm=TRUE),
                   tidak.teridentifikasi = sum(dat.full$num.unidentified, na.rm=TRUE))
out.1 = rbind(out.1,
              cbind(individu.marginal = out.1$individu.marginal / rowSums(out.1[1,]),
                    lembaga.marginal = out.1$lembaga.marginal / rowSums(out.1[1,]),
                    non.marginal = out.1$non.marginal / rowSums(out.1[1,]),
                    tidak.teridentifikasi = out.1$tidak.teridentifikasi / rowSums(out.1[1,])))
out.1[1, ] = as.integer(out.1[1, ])
out.1[2, ] = round(out.1[2, ], decimal)

out.2 = data.frame(cluster = NA,
                   individu.marginal = tapply(dat.full$pr.ind.marginal, dat.full$cluster, mean, na.rm=TRUE),
                   lembaga.marginal = tapply(dat.full$pr.lembaga.marginal, dat.full$cluster, mean, na.rm=TRUE),
                   non.marginal = tapply(dat.full$pr.nonmarginal, dat.full$cluster, mean, na.rm=TRUE),
                   tidak.teridentifikasi = tapply(dat.full$pr.unidentified, dat.full$cluster, mean, na.rm=TRUE))
out.2$cluster = rownames(out.2)
out.2$individu.marginal = round(out.2$individu.marginal, decimal)
out.2$lembaga.marginal = round(out.2$lembaga.marginal, decimal)
out.2$non.marginal = round(out.2$non.marginal, decimal)
out.2$tidak.teridentifikasi = round(out.2$tidak.teridentifikasi, decimal)

out.3 = data.frame(media = NA,
                   individu.marginal = tapply(dat.full$pr.ind.marginal, dat.full$media, mean, na.rm=TRUE),
                   lembaga.marginal = tapply(dat.full$pr.lembaga.marginal, dat.full$media, mean, na.rm=TRUE),
                   non.marginal = tapply(dat.full$pr.nonmarginal, dat.full$media, mean, na.rm=TRUE),
                   tidak.teridentifikasi = tapply(dat.full$pr.unidentified, dat.full$media, mean, na.rm=TRUE))
out.3$media = rownames(out.3)
out.3$individu.marginal = round(out.3$individu.marginal, decimal)
out.3$lembaga.marginal = round(out.3$lembaga.marginal, decimal)
out.3$non.marginal = round(out.3$non.marginal, decimal)
out.3$tidak.teridentifikasi = round(out.3$tidak.teridentifikasi, decimal)

addPageBreak(rtffile)
addHeader(rtffile, "Perbandingan Karakteristik Narasumber", font.size = 16)
addTable(rtffile, out.1, col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph(rtffile, "Proporsi Tipe Narasumber per Cluster")
addTable(rtffile, out.2, col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph(rtffile, "Proporsi Tipe Narasumber per Media")
addTable(rtffile, out.3, col.justify = "C")


####################################################################
### Bagaimana hubungan antara jenis narasumber
### (individu marjinal, lembaga marjinal, dan non-marjinal dengan tone (positif, negatif, netral)
####################################################################

with(dat.full, {
  print(cor.test(score.inklusifitas.narsum, score.inklusifitas.tone))
  for (i in unique(cluster)) {
    print(cor.test(score.inklusifitas.narsum[cluster == i], score.inklusifitas.tone[cluster == i]))
  }
})


table.narsum.tone = data.frame(cluster=NA, media=NA, narsum=NA, tone=NA)

for (i in 1:nrow(dat.full)) {
  vec.inklusifitas = as.numeric(str_extract_all(dat.full[i, "inklusifitas.narsum"],"[^*]+")[[1]])
  vec.tone = as.numeric(str_extract_all(dat.full[i, "inklusifitas.tone"],"[^*]+")[[1]])
  
  if (length(vec.inklusifitas) == 0) next;
  for (j in 1:length(vec.inklusifitas)) {
    
    table.narsum.tone = rbind(table.narsum.tone,
                                       cbind(cluster = dat.full[i, "cluster"],
                                             media = dat.full[i, "media"],
                                             narsum = as.character(vec.inklusifitas[j]),
                                             tone = as.character(vec.tone[j])
                                             ))
  }
}

# delete first row because NA
table.narsum.tone = table.narsum.tone[-1, ]

table.narsum.tone$narsum[which(table.narsum.tone$narsum == "2")] = "individu or lembaga marginal"
table.narsum.tone$narsum[which(table.narsum.tone$narsum == "1")] = "individu or lembaga marginal"
table.narsum.tone$narsum[which(table.narsum.tone$narsum == "0")] = "non-marginal"
table.narsum.tone$narsum[which(table.narsum.tone$narsum == "-1")] = "tidak teridentifikasi"

table.narsum.tone$tone[which(table.narsum.tone$tone == "1")] = "positif"
table.narsum.tone$tone[which(table.narsum.tone$tone == "0")] = "netral"
table.narsum.tone$tone[which(table.narsum.tone$tone == "-1")] = "negatif"

out.all = table(table.narsum.tone$narsum, table.narsum.tone$tone)

temp.dat = subset(table.narsum.tone, subset=cluster == "agama marjinal")
out.agama = table(temp.dat$narsum, temp.dat$tone)

temp.dat = subset(table.narsum.tone, subset=cluster == "disabilitas")
out.disabilitas = table(temp.dat$narsum, temp.dat$tone)

temp.dat = subset(table.narsum.tone, subset=cluster == "identitas gender dan orientasi seksual")
out.gender = table(temp.dat$narsum, temp.dat$tone)

temp.dat = subset(table.narsum.tone, subset=cluster == "perempuan dalam kekerasan")
out.perempuan = table(temp.dat$narsum, temp.dat$tone)


addPageBreak(rtffile)
addHeader(rtffile, "Karakteristik Narsum dan Tone", font.size = 16)
addParagraph(rtffile, "Keseluruhan Artikel")
addTable(rtffile, out.all, col.justify = "C")
addTable(rtffile, round(prop.table(out.all, 1), decimal), col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph(rtffile, "Artikel Cluster Agama")
addTable(rtffile, out.agama, col.justify = "C")
addTable(rtffile, round(prop.table(out.agama, 1), decimal), col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph(rtffile, "Artikel Cluster Disabilitas")
addTable(rtffile, out.disabilitas, col.justify = "C")
addTable(rtffile, round(prop.table(out.disabilitas, 1), decimal), col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph(rtffile, "Artikel Cluster Identitas Gender")
addTable(rtffile, out.gender, col.justify = "C")
addTable(rtffile, round(prop.table(out.gender, 1), decimal), col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph(rtffile, "Artikel Cluster Perempuan")
addTable(rtffile, out.perempuan, col.justify = "C")
addTable(rtffile, round(prop.table(out.perempuan, 1), decimal), col.justify = "C")
addNewLine.RTF(rtffile, n=5)



####################################################################
### SCORING CALCULATION
####################################################################


## standar.sumber: tidak ada (0), ada (1)
## standar.berimbang: not applicable (NA), tidak berimbang (0), berimbang (1)
## standar.privasi: memuat data pribadi (0), tidak memuat (1)
## standar.perkosaan: not applicable (NA), eksploitatif (0), tidak eksploitatif (1)
## standar.clickbait: clickbait (0), tidak (1)
## inklusifitas.narsum: for each narsum, tak teridentifikasi (0), non-marjinal (0), lembaga marjinal (1), individu marjinal (2)
## inklusifitas.tone: for each narsum, negatif (0), netral (1), positif (2)
## inklusifitas.marking: ya (0), tidak (1)
## agenda.isu = episodic (0), thematic (1)

summary(dat.full$standar.sumber)                  # 0 1         15%
summary(dat.full$standar.berimbang)               # 0 1         15%
summary(dat.full$standar.privasi)                 # 0 1          5%
summary(dat.full$standar.perkosaan)               # 0 1          5%
summary(dat.full$standar.clickbait)               # 0 1          5%
summary(dat.full$score.inklusifitas.narsum)       # 0 .5 1      20%
summary(dat.full$score.inklusifitas.tone)         # 0 .5 1      10%
summary(dat.full$inklusifitas.marking)            # 0 1          5%
summary(dat.full$agenda.isu)                      # 0 1         10%


### All Aspects
col = c("standar.sumber", "standar.berimbang", "standar.privasi", "standar.perkosaan", "standar.clickbait", "inklusifitas.marking",
        "score.inklusifitas.narsum", "score.inklusifitas.tone", "agenda.isu")
weight = c(.10, .10, .10, .10, .10, .10,
           .20, .10, .10)
names(weight) = col
dat.scoring = dat.full[, col]
raw.score = denominator = rep(NA, nrow(dat.scoring))

dat.avail = as.matrix(!is.na(dat.scoring))

for (i in 1:nrow(dat.scoring)) {
  raw.score[i] = sum(dat.scoring[i, ] * weight, na.rm=TRUE)
  denominator[i] = (dat.avail[i,] %*% weight)
}
dat.full$score.all = raw.score/denominator


### Aspek standar pemberitaan
col.standar = c("standar.sumber", "standar.berimbang", "standar.privasi", "standar.perkosaan", "standar.clickbait", "inklusifitas.marking")
weight.standar = c(.10, .10, .10, .10, .10, .10)

dat.scoring = dat.full[, col.standar]
raw.score = denominator = rep(NA, nrow(dat.scoring))
dat.avail = as.matrix(!is.na(dat.scoring))

for (i in 1:nrow(dat.scoring)) {
  raw.score[i] = sum(dat.scoring[i, ] * weight.standar, na.rm=TRUE)
  denominator[i] = (dat.avail[i,] %*% weight.standar)
}
dat.full$score.standar = raw.score/denominator


### Aspek inklusifitas
col.inklusifitas = c("score.inklusifitas.narsum", "score.inklusifitas.tone", "agenda.isu")
weight.inklusifitas = c(.20, .10, .10)

dat.scoring = dat.full[, col.inklusifitas]
raw.score = denominator = rep(NA, nrow(dat.scoring))
dat.avail = as.matrix(!is.na(dat.scoring))

for (i in 1:nrow(dat.scoring)) {
  raw.score[i] = sum(dat.scoring[i, ] * weight.inklusifitas, na.rm=TRUE)
  denominator[i] = (dat.avail[i,] %*% weight.inklusifitas)
}

dat.full$score.inklusifitas = raw.score/denominator



####################################################################
### Skor/ranking tiap media secara umum (skor dari koder eksternal?)
####################################################################

out = data.frame(media = unique(dat.full$media),
                 min=NA, q1=NA, median=NA, mean=NA, q3=NA, max=NA)

for (i in 1:nrow(out)) {
  index = which(dat.full$media == out[i, "media"])
  out[i,2:7] = round(summary(dat.full[index, "score.all"]), decimal)
}


addPageBreak(rtffile)
addHeader(rtffile, "Skor/Ranking Tiap Media Secara Umum", font.size = 16)
addTable(rtffile, out, col.justify = "C")
addNewLine.RTF(rtffile, n=5)


####################################################################
### Skor/ranking tiap media untuk tiap klaster (disabilitas, perempuan, agama marginal, gender)
####################################################################

out.agama = out.disabilitas = out.gender = out.perempuan = 
            data.frame( media = unique(dat.full$media),
                        min=NA, q1=NA, median=NA, mean=NA, q3=NA, max=NA)

### Agama
for (i in 1:nrow(out.agama)) {
  temp.dat = subset(dat.full, subset=cluster == "agama marjinal")
  index = which(temp.dat$media == out.agama[i, "media"])
  out.agama[i,2:7] = round(summary(temp.dat[index, "score.all"]), decimal)
}

### Disabilitas
for (i in 1:nrow(out.disabilitas)) {
  temp.dat = subset(dat.full, subset=cluster == "disabilitas")
  index = which(temp.dat$media==out.disabilitas[i, "media"])
  out.disabilitas[i,2:7] = round(summary(temp.dat[index, "score.all"]), decimal)
}

### Identitas Gender
for (i in 1:nrow(out.gender)) {
  temp.dat = subset(dat.full, subset=cluster == "identitas gender dan orientasi seksual")
  index = which(temp.dat$media==out.gender[i, "media"])
  out.gender[i,2:7] = round(summary(temp.dat[index, "score.all"]), decimal)
}

### Perempuan Dalam Kekerasan
for (i in 1:nrow(out.perempuan)) {
  temp.dat = subset(dat.full, subset=cluster == "perempuan dalam kekerasan")
  index = which(temp.dat$media==out.perempuan[i, "media"])
  out.perempuan[i,2:7] = round(summary(temp.dat[index, "score.all"]), decimal)
}


addPageBreak(rtffile)
addHeader(rtffile, "Skor Media per Cluster", font.size = 16)
addParagraph(rtffile, "Cluster Agama")
addTable(rtffile, out.agama, col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph(rtffile, "Cluster Disabilitas")
addTable(rtffile, out.disabilitas, col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph(rtffile, "Cluster Identitas Gender")
addTable(rtffile, out.gender, col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph(rtffile, "Cluster Perempuan")
addTable(rtffile, out.perempuan, col.justify = "C")
addNewLine.RTF(rtffile, n=5)


####################################################################
### Skor/ranking tiap media untuk tiap aspek
####################################################################


### Standar Pemberitaan
out.standar = out.agama.standar = out.disabilitas.standar = out.gender.standar = out.perempuan.standar =
                    data.frame(media = unique(dat.full$media),
                               min=NA, q1=NA, median=NA, mean=NA, q3=NA, max=NA)

for (i in 1:nrow(out.standar)) {
  index = which(dat.full$media==out.standar[i, "media"])
  out.standar[i,2:7] = round(summary(dat.full[index, "score.standar"]), decimal)
}

for (i in 1:nrow(out.agama.standar)) {
  temp.dat = subset(dat.full, subset=cluster == "agama marjinal")
  index = which(temp.dat$media==out.agama.standar[i, "media"])
  out.agama.standar[i,2:7] = round(summary(temp.dat[index, "score.standar"]), decimal)
}

for (i in 1:nrow(out.disabilitas.standar)) {
  temp.dat = subset(dat.full, subset=cluster == "disabilitas")
  index = which(temp.dat$media==out.disabilitas.standar[i, "media"])
  out.disabilitas.standar[i,2:7] = round(summary(temp.dat[index, "score.standar"]), decimal)
}

for (i in 1:nrow(out.gender.standar)) {
  temp.dat = subset(dat.full, subset=cluster == "identitas gender dan orientasi seksual")
  index = which(temp.dat$media==out.gender.standar[i, "media"])
  out.gender.standar[i,2:7] = round(summary(temp.dat[index, "score.standar"]), decimal)
}

for (i in 1:nrow(out.perempuan.standar)) {
  temp.dat = subset(dat.full, subset=cluster == "perempuan dalam kekerasan")
  index = which(temp.dat$media==out.perempuan.standar[i, "media"])
  out.perempuan.standar[i,2:7] = round(summary(temp.dat[index, "score.standar"]), decimal)
}


addPageBreak(rtffile)
addHeader(rtffile, "Skor/Ranking Tiap Media Aspek Standar Pemberitaan", font.size = 16)
addParagraph(rtffile, "Keseluruhan")
addTable(rtffile, out.standar, col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph(rtffile, "Cluster Agama")
addTable(rtffile, out.agama.standar, col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph(rtffile, "Cluster Disabilitas")
addTable(rtffile, out.disabilitas.standar, col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph(rtffile, "Cluster Identitas Gender")
addTable(rtffile, out.gender.standar, col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph(rtffile, "Cluster Perempuan")
addTable(rtffile, out.perempuan.standar, col.justify = "C")
addNewLine.RTF(rtffile, n=5)


### Inklusifitas
out.inklusifitas = out.agama.inklusifitas = out.disabilitas.inklusifitas = out.gender.inklusifitas = out.perempuan.inklusifitas =
  data.frame(media = unique(dat.full$media),
             min=NA, q1=NA, median=NA, mean=NA, q3=NA, max=NA)

for (i in 1:nrow(out.inklusifitas)) {
  index = which(dat.full$media==out.inklusifitas[i, "media"])
  out.inklusifitas[i,2:7] = round(summary(dat.full[index, "score.inklusifitas"]), decimal)
}

for (i in 1:nrow(out.agama.inklusifitas)) {
  temp.dat = subset(dat.full, subset=cluster == "agama marjinal")
  index = which(temp.dat$media==out.agama.inklusifitas[i, "media"])
  out.agama.inklusifitas[i,2:7] = round(summary(temp.dat[index, "score.inklusifitas"]), decimal)
}

for (i in 1:nrow(out.disabilitas.inklusifitas)) {
  temp.dat = subset(dat.full, subset=cluster == "disabilitas")
  index = which(temp.dat$media==out.disabilitas.inklusifitas[i, "media"])
  out.disabilitas.inklusifitas[i,2:7] = round(summary(temp.dat[index, "score.inklusifitas"]), decimal)
}

for (i in 1:nrow(out.gender.inklusifitas)) {
  temp.dat = subset(dat.full, subset=cluster == "identitas gender dan orientasi seksual")
  index = which(temp.dat$media==out.gender.inklusifitas[i, "media"])
  out.gender.inklusifitas[i,2:7] = round(summary(temp.dat[index, "score.inklusifitas"]), decimal)
}

for (i in 1:nrow(out.perempuan.inklusifitas)) {
  temp.dat = subset(dat.full, subset=cluster == "perempuan dalam kekerasan")
  index = which(temp.dat$media==out.perempuan.inklusifitas[i, "media"])
  out.perempuan.inklusifitas[i,2:7] = round(summary(temp.dat[index, "score.inklusifitas"]), decimal)
}


addPageBreak(rtffile)
addHeader(rtffile, "Skor/Ranking Tiap Media Aspek Inklusifitas", font.size = 16)
addParagraph(rtffile, "Keseluruhan")
addTable(rtffile, out.inklusifitas, col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph(rtffile, "Cluster Agama")
addTable(rtffile, out.agama.inklusifitas, col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph(rtffile, "Cluster Disabilitas")
addTable(rtffile, out.disabilitas.inklusifitas, col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph(rtffile, "Cluster Identitas Gender")
addTable(rtffile, out.gender.inklusifitas, col.justify = "C")
addNewLine.RTF(rtffile, n=5)

addParagraph(rtffile, "Cluster Perempuan")
addTable(rtffile, out.perempuan.inklusifitas, col.justify = "C")
addNewLine.RTF(rtffile, n=5)


done(rtffile)

