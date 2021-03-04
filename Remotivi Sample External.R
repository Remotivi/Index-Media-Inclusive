
####################################################################
## SAMPLE FOR EXTERNAL CODERS
####################################################################

dat.agama = subset(dat.full, subset=(cluster == "agama marjinal"))
dat.disabilitas = subset(dat.full, subset=(cluster == "disabilitas"))
dat.gender = subset(dat.full, subset=(cluster == "identitas gender dan orientasi seksual"))
dat.perempuan = subset(dat.full, subset=(cluster == "perempuan dalam kekerasan"))

dim(dat.full)
dim(dat.agama)
dim(dat.disabilitas)
dim(dat.gender)
dim(dat.perempuan)

nrow(dat.agama) + nrow(dat.disabilitas) + nrow(dat.gender) + nrow(dat.perempuan)

# index = which(colnames(dat.full) %in% 
#                 c("coder1",	"coder2",	"formal.konteks",	"formal.narsum",	
#                   "standar.sumber",	"standar.berimbang",	"standar.privasi",
#                   "standar.perkosaan",	"standar.clickbait",	"inklusifitas.narsum",
#                   "inklusifitas.tone",	"inklusifitas.istilah",	"inklusifitas.jumlah",	
#                   "inklusifitas.marking",	"agenda.isu",	"aksesibilitas.front",
#                   "aksesibilitas.artikel",	"aksesibilitas.manual",	"inklusifitas.narsum.total",
#                   "inklusifitas.tone.total", "jumlah.narsum",	"status"))

col.index = which(colnames(dat.full) %in%
                c("id", "cluster", "media", "weekday", "month", "publish.date", "judul", "link"))


sampling <- function(temp.dat) {
  
  out = NA
  for (i in unique(temp.dat$media)) {
    index = which(temp.dat$media == i)
    sampled = sample(index, 15)
    out = rbind(out, temp.dat[sampled, ])
  }
  
  out = out[-1, ]
  out$catatan = out$bahasa = out$representasi = out$tone = out$komposisi.narsum = out$frame = out$nilai.judul = ""
  out$coder = sample(rep(c(1,2,3,4,5,10), 25))
  
  return (out)
}


### sample cluster agama
set.seed(1)
sample.agama = sampling(dat.agama[, col.index])
col.delete = which(colnames(sample.agama)=="coder")
write.xlsx2(sample.agama[sample.agama$coder %in% c(1,10), -col.delete], 
            file="Out - External Cluster Agama.xlsx", sheetName = "R1", row.names = FALSE)
write.xlsx2(sample.agama[sample.agama$coder %in% c(2,10), -col.delete], 
            file="Out - External Cluster Agama.xlsx", sheetName = "R2", row.names = FALSE, append=TRUE)
write.xlsx2(sample.agama[sample.agama$coder %in% c(3,10), -col.delete], 
            file="Out - External Cluster Agama.xlsx", sheetName = "R3", row.names = FALSE, append=TRUE)
write.xlsx2(sample.agama[sample.agama$coder %in% c(4,10), -col.delete], 
            file="Out - External Cluster Agama.xlsx", sheetName = "R4", row.names = FALSE, append=TRUE)
write.xlsx2(sample.agama[sample.agama$coder %in% c(5,10), -col.delete], 
            file="Out - External Cluster Agama.xlsx", sheetName = "R5", row.names = FALSE, append=TRUE)
write.xlsx2(sample.agama, 
            file="Out - External Cluster Agama.xlsx", sheetName = "ALL", row.names = FALSE, append=TRUE)


### sample cluster disabilitas
set.seed(1)
sample.disabilitas = sampling(dat.disabilitas[, col.index])
col.delete = which(colnames(sample.disabilitas)=="coder")
write.xlsx2(sample.disabilitas[sample.disabilitas$coder %in% c(1,10), -col.delete], 
            file="Out - External Cluster Disabilitas.xlsx", sheetName = "R1", row.names = FALSE)
write.xlsx2(sample.disabilitas[sample.disabilitas$coder %in% c(2,10), -col.delete], 
            file="Out - External Cluster Disabilitas.xlsx", sheetName = "R2", row.names = FALSE, append=TRUE)
write.xlsx2(sample.disabilitas[sample.disabilitas$coder %in% c(3,10), -col.delete], 
            file="Out - External Cluster Disabilitas.xlsx", sheetName = "R3", row.names = FALSE, append=TRUE)
write.xlsx2(sample.disabilitas[sample.disabilitas$coder %in% c(4,10), -col.delete], 
            file="Out - External Cluster Disabilitas.xlsx", sheetName = "R4", row.names = FALSE, append=TRUE)
write.xlsx2(sample.disabilitas[sample.disabilitas$coder %in% c(5,10), -col.delete], 
            file="Out - External Cluster Disabilitas.xlsx", sheetName = "R5", row.names = FALSE, append=TRUE)
write.xlsx2(sample.disabilitas, 
            file="Out - External Cluster Disabilitas.xlsx", sheetName = "ALL", row.names = FALSE, append=TRUE)


### sample cluster gender
set.seed(1)
sample.gender = sampling(dat.gender[, col.index])
col.delete = which(colnames(sample.gender)=="coder")
write.xlsx2(sample.gender[sample.gender$coder %in% c(1,10), -col.delete], 
            file="Out - External Cluster Gender.xlsx", sheetName = "R1", row.names = FALSE)
write.xlsx2(sample.gender[sample.gender$coder %in% c(2,10), -col.delete], 
            file="Out - External Cluster Gender.xlsx", sheetName = "R2", row.names = FALSE, append=TRUE)
write.xlsx2(sample.gender[sample.gender$coder %in% c(3,10), -col.delete], 
            file="Out - External Cluster Gender.xlsx", sheetName = "R3", row.names = FALSE, append=TRUE)
write.xlsx2(sample.gender[sample.gender$coder %in% c(4,10), -col.delete], 
            file="Out - External Cluster Gender.xlsx", sheetName = "R4", row.names = FALSE, append=TRUE)
write.xlsx2(sample.gender[sample.gender$coder %in% c(5,10), -col.delete], 
            file="Out - External Cluster Gender.xlsx", sheetName = "R5", row.names = FALSE, append=TRUE)
write.xlsx2(sample.gender, 
            file="Out - External Cluster Gender.xlsx", sheetName = "ALL", row.names = FALSE, append=TRUE)


### sample cluster perempuan
set.seed(1)
sample.perempuan = sampling(dat.perempuan[, col.index])
col.delete = which(colnames(sample.perempuan)=="coder")
write.xlsx2(sample.perempuan[sample.perempuan$coder %in% c(1,10), -col.delete], 
            file="Out - External Cluster Perempuan.xlsx", sheetName = "R1", row.names = FALSE)
write.xlsx2(sample.perempuan[sample.perempuan$coder %in% c(2,10), -col.delete], 
            file="Out - External Cluster Perempuan.xlsx", sheetName = "R2", row.names = FALSE, append=TRUE)
write.xlsx2(sample.perempuan[sample.perempuan$coder %in% c(3,10), -col.delete], 
            file="Out - External Cluster Perempuan.xlsx", sheetName = "R3", row.names = FALSE, append=TRUE)
write.xlsx2(sample.perempuan[sample.perempuan$coder %in% c(4,10), -col.delete], 
            file="Out - External Cluster Perempuan.xlsx", sheetName = "R4", row.names = FALSE, append=TRUE)
write.xlsx2(sample.perempuan[sample.perempuan$coder %in% c(5,10), -col.delete], 
            file="Out - External Cluster Perempuan.xlsx", sheetName = "R5", row.names = FALSE, append=TRUE)
write.xlsx2(sample.perempuan, 
            file="Out - External Cluster Perempuan.xlsx", sheetName = "ALL", row.names = FALSE, append=TRUE)
