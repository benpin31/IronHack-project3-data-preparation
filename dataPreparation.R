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

# data 2019
meltStat <- as.data.table(melt(stat, id.vars=c("date", "timeRange", "Physical", "Telephone"))[!is.na(value)])
meltStat[,date := as.POSIXct(date, format="%d/%m/%Y")]

# data 2020
meltStat2020=copy(meltStat)
meltStat2020[, date:=date+364*24*3600 ]
meltStat2020<-meltStat2020[format(date,"%Y-%m-%d") != "2020-01-01"]
meltStat2020[,mult := rnorm(.N,1.1,0.1)]
meltStat2020[, Physical := floor(Physical*mult)]
meltStat2020[, Telephone := floor(Telephone*mult)]
meltStat2020[, value := floor(value*mult)]
meltStat2020[,mult:=NULL]

# data 2021
meltStat2021=copy(meltStat)
meltStat2021[, date:=date+(365+363)*24*3600 ]
meltStat2021<-meltStat2021[!(format(date,"%Y-%m-%d") %in% c("2021-01-01", "2020-12-30", "2020-12-31"))]
meltStat2021[,mult := rnorm(.N,1.3,0.1)]
meltStat2021[, Physical := floor(Physical*mult)]
meltStat2021[, Telephone := floor(Telephone*mult)]
meltStat2021[, value := floor(value*mult)]
meltStat2021[,mult:=NULL]

# total
meltStat[date == "2021-11-29"]
meltStat2020[date == "2021-11-29"]
meltStat2021[date == "2021-11-29"]

meltStat <- rbind(meltStat, meltStat2020, meltStat2021)[order(date)]
meltStat[date == "2021-12-28"]
meltStat2021[date == "2021-12-28"]
meltStat2020[date == "2021-11-29"]



randomHour = function(timeRange, date) {
  if(timeRange == "15h30 - 18h00") {
    return(as.POSIXct(paste0(date, " 15:30"), format='%Y-%m-%d %H:%M') + runif(1, 0, 60*150))
  }
  if(timeRange == "13h00 - 15h30") {
    return(as.POSIXct(paste0(date, " 13:00"), format='%Y-%m-%d %H:%M') + runif(1, 0, 60*150))
  }
  if(timeRange == "18h00 - 20h00") {
    return(as.POSIXct(paste0(date, " 18:00"), format='%Y-%m-%d %H:%M') + runif(1, 0, 60*120))
  }
}

# Create JSON

res = c()
titi <- 1

for (dateValue in unique(meltStat[, format(date,"%Y-%m-%d" )])) {
  print(dateValue)
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
  titi <- titi+1
}


res <- res[,list(variable, V2, V3)]
colnames(res) <- c("category", "date", "contactType")

res[, seed := paste0('{"category":"',category,'", "date":"',date,'", "contactType":"',contactType,'"}')]

querySeed <- paste0("[", paste(res[,seed], collapse=",\n"), "]")
write.csv(querySeed, "seed.txt", quote=FALSE)



# data viz example

barplot(res[,.N, by = format(date, "%Y-%m")][, N])
res[,]










