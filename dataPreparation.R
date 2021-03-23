rm(list=ls())
library(data.table)

# Import datas
stat <- data.table(read.csv("stat-2019.csv"))


# clean datas
getTimeRange <- function (timeRange1, timeRange2, timeRange3) {
  if(!is.na(timeRange1) & timeRange1 == 1) {return("13h00 - 15h30")}
  if(!is.na(timeRange2) & timeRange2 == 1) {return("15h30 - 18h00")}
  if(!is.na(timeRange3) & timeRange3 == 1) {return("18h00 - 20h00")}
}

stat[, timeRange := getTimeRange(timeRange1, timeRange2, timeRange3), by=c("timeRange1", "timeRange2", "timeRange3")]

stat[, timeRange1:=NULL]
stat[, timeRange2:=NULL]
stat[, timeRange3:=NULL]


colnames(stat) <- c(
  "date", 
  "Physical", 
  "Telephone", 
  "Bar associatif", 
  "Doc associatives", 
  "Infos touristiques", 
  "Docs infos santé", 
  "activités Pôle Santé",     
  "Activités Pôle Culture",
  "Autres activités",           
  "événements hors pôles",      
  "Activités des associations",
  "Autres",
  "timeRange"
)

stat <- stat[!is.na(timeRange)]

meltStat <- as.data.table(melt(stat, id.vars=c("date", "timeRange", "Physical", "Telephone"))[!is.na(value)])

randomHour = function(timeRange, date) {
  if(timeRange == "15h30 - 18h00") {
    return(as.POSIXct(paste0(date, " 15:30"), format='%d/%m/%Y %H:%M') + runif(1, 0, 60*150))
  }
  if(timeRange == "13h00 - 15h30") {
    return(as.POSIXct(paste0(date, " 13:00"), format='%d/%m/%Y %H:%M') + runif(1, 0, 60*150))
  }
  if(timeRange == "18h00 - 20h00") {
    return(as.POSIXct(paste0(date, " 18:00"), format='%d/%m/%Y %H:%M') + runif(1, 0, 60*120))
  }
}

# Create JSON

res = c()

for (dateValue in unique(meltStat[, date])) {
  for (timeRangeValue in unique(meltStat[date == dateValue, timeRange])) {
    segment <- meltStat[date == dateValue & timeRange == timeRangeValue]
    nbPhys <- head(segment[, Physical],1)
    nbTel <- head(segment[,Telephone], 1)

    if (is.na(nbPhys)) {total<- nbTel ;telList <- 1:nbTel}
    if (is.na(nbTel)) {total<- nbPhys ; telList <- c()}
    if (!is.na(nbPhys) & !is.na(nbTel)) {total<- nbPhys+nbTel; telList <- sample(1:(nbPhys+nbTel), nbTel)}

    cpt=1
    for (k in 1:dim(segment)[1]) {
      for (l in 1:segment[k, value]) {
        if(cpt %in% telList) {
          res <- rbind(res,  segment[k, list(variable, randomHour(timeRange, date), "Téléphone"), by = c("timeRange", "date")] )
        } else {
          res <- rbind(res,  segment[k, list(variable, randomHour(timeRange, date), "Physique"), by = c("timeRange", "date")] )
        }
        cpt<-cpt+1
      }
    }
  }
}


res <- res[,list(variable, V2, V3)]
colnames(res) <- c("category", "date", "contactType")

res[, seed := paste0('{"category":"',category,'", "date":"',date,'", "contactType":"',contactType,'"}')]
res2020 <- copy(res)
res2020[, date := date+364*24*3600]
res2020 <- res2020[format(date,"%Y-%m-%d") != "2020-01-01"]
sort(res2020[,unique(format(date,"%Y-%m-%d"))])
res2021 <- copy(res)
res2021[, date := date+(365+363)*24*3600]
res2021 <- res2021[!(format(date,"%Y-%m-%d") %in% c("2019-01-01", "2020-12-30", "2020-12-31"))]
sort(res2021[,unique(format(date,"%Y-%m-%d"))])

querySeed <- paste0("[", paste(res[,seed], collapse=",\n"), "]")
write.csv(querySeed, "seed.txt", quote=FALSE)

