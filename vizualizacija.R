################################
# 1.) VIZUALIZACIJA PODATKOV   #
################################

# Branje podatkov iz tekstovne datoteke:

regular <- read.table(file="regular.txt", sep=",", header=TRUE)
regular
head(regular)
summary(regular)
str(regular)
names(regular)

# Spremenljivki, ki hranita ime zaporedne zmagovalne / porazene ekipe

winns <- ifelse(regular$HPTS > regular$APTS, as.character(regular$HOME), as.character(regular$AWAY))
losses <- ifelse(regular$HPTS > regular$APTS, as.character(regular$AWAY), as.character(regular$HOME))

winns
losses

# Dva podatkovna okvirja za zmage in poraze

dfW <- data.frame(team=winns) 
dfW$winns <- table(dfW)[as.character(dfW$team)]
dfW <- unique(dfW)

dfL <- data.frame(team=losses) 
dfL$losses <- table(dfL)[as.character(dfL$team)]
dfL <- unique(dfL)

dfW
dfL

# Oba podatkovna okvirja zdru?imo

total <- merge(dfW, dfL)
total

# Izris diagrama najuspesnejsih in najslabsih ekip glede na zmage in poraze

pie(total$winns, labels = total$team)
lbls <- total$team
pct <- round(total$winns/sum(total$winns)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(main="Krozni diagram porazdelitve zmag", total$winns, labels = lbls, cex=0.75)

barplot(main="Histogram zmag po ekipah", total$winns, names.arg=total$team, cex.names=0.75, col="blue")

pie(total$losses, labels = total$team)
lbls <- total$team
pct <- round(total$losses/sum(total$losses)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(main="Krozni diagram porazdelitve porazov", total$losses, labels = lbls, cex=0.75)

barplot(main="Histogram porazov po ekipah", total$losses, names.arg=total$team, cex.names=0.75, col="red")


# Dodamo razmerje zmag in porazov

total$ratio <- total$winns / total$losses
total

pie(total$ratio, labels=total$team, main="Tortni diagram za razmerje zmag in porazov", cex = 0.75)
plot(xlab="Ekipa", ylab="Razmerje", main="Graf razmerja zmag in porazov", total$team, total$ratio, col=rainbow(20), las=2)

# Izpis vrednosti po posameznih sezonah (1., 2. in 3. sezona)

names(regular)
levels(regular$SEASON)
regular$SEASON
as.integer(regular$SEASON)

regular[as.integer(regular$SEASON) == 1,]
regular[as.integer(regular$SEASON) == 2,]
regular[as.integer(regular$SEASON) == 3,]

# Razmerje zmag in porazov ekip v sezoni 2014-2015

regular1415 <- regular[as.integer(regular$SEASON) == 1,]

winns1415 <- ifelse(regular1415$HPTS > regular1415$APTS, as.character(regular1415$HOME), as.character(regular1415$AWAY))
losses1415 <- ifelse(regular1415$HPTS > regular1415$APTS, as.character(regular1415$AWAY), as.character(regular1415$HOME))

dfW1415 <- data.frame(team=winns1415) 
dfW1415$winns <- table(dfW1415)[as.character(dfW1415$team)]
dfW1415 <- unique(dfW1415)

dfL1415 <- data.frame(team=losses1415) 
dfL1415$losses <- table(dfL1415)[as.character(dfL1415$team)]
dfL1415 <- unique(dfL1415)

dfW1415
dfL1415

total415 <- merge(dfW1415, dfL1415)
total415$ratio <- total415$winns / total415$losses
total415

plot(total415$team, total415$ratio, main="Razmerje zmag in porazov v sezoni 2014-2015")


# Razmerje zmag in porazov ekip v sezoni 2015-2016

regular1516 <- regular[as.integer(regular$SEASON) == 2,]

winns1516 <- ifelse(regular1516$HPTS > regular1516$APTS, as.character(regular1516$HOME), as.character(regular1516$AWAY))
losses1516 <- ifelse(regular1516$HPTS > regular1516$APTS, as.character(regular1516$AWAY), as.character(regular1516$HOME))

dfW1516 <- data.frame(team=winns1516) 
dfW1516$winns <- table(dfW1516)[as.character(dfW1516$team)]
dfW1516 <- unique(dfW1516)

dfL1516 <- data.frame(team=losses1516) 
dfL1516$losses <- table(dfL1516)[as.character(dfL1516$team)]
dfL1516 <- unique(dfL1516)

dfW1516
dfL1516

tota1516 <- merge(dfW1516, dfL1516)
tota1516$ratio <- tota1516$winns / tota1516$losses
tota1516

plot(tota1516$team, tota1516$ratio, main="Razmerje zmag in porazov v sezoni 2015-2016")

# Razmerje zmag in porazov ekip v sezoni 2016-2017

regular1617 <- regular[as.integer(regular$SEASON) == 3,]

winns1617 <- ifelse(regular1617$HPTS > regular1617$APTS, as.character(regular1617$HOME), as.character(regular1617$AWAY))
losses1617 <- ifelse(regular1617$HPTS > regular1617$APTS, as.character(regular1617$AWAY), as.character(regular1617$HOME))

dfW1617 <- data.frame(team=winns1617) 
dfW1617$winns <- table(dfW1617)[as.character(dfW1617$team)]
dfW1617 <- unique(dfW1617)

dfL1617 <- data.frame(team=losses1617) 
dfL1617$losses <- table(dfL1617)[as.character(dfL1617$team)]
dfL1617 <- unique(dfL1617)

dfW1617
dfL1617

total617 <- merge(dfW1617, dfL1617)
total617$ratio <- total617$winns / total617$losses
total617

plot(total617$team, total617$ratio, main="Razmerje zmag in porazov v sezoni 2016-2017")


# ------------------------------------------

# Vektor sezon v stringu
sezone <- c(as.character(unique(regular$SEASON)))
sezone

# Izbira vsake sezone posebej
sezona1415 <- regular[as.integer(regular$SEASON) == 1,]
sezona1516 <- regular[as.integer(regular$SEASON) == 2,]
sezona1617 <- regular[as.integer(regular$SEASON) == 3,]

# Izracun povprecnega stevila metov za tri tocke na sezono
povprecje <- c(mean(sezona1415$H3PA) + mean(sezona1415$A3PA) / 2, 
               mean(sezona1516$H3PA) + mean(sezona1516$A3PA) / 2, 
               mean(sezona1617$H3PA) + mean(sezona1617$A3PA) / 2)
povprecje

# Ustvarjanje podatkovnega okvirja
poSezonah <- data.frame(season=sezone, average=povprecje)
poSezonah

# Izris kolaca za povprecno stevilo metov za tri tocke
pie(poSezonah$average, labels=poSezonah$season, main = "Povprecno stevilo metov za tri tocke po posameznih sezonah")

#for (i in 1:3) {
#  sezona1415 <- regular[as.integer(regular$SEASON) == i,]
#  mean(sezona1415$H3PA) + mean(sezona1415$A3PA) / 2
#}


# Stevilo zadetih prostih metov na ekipo v vseh treh sezonah

a <- aggregate(regular$HFTM, by=list(regular$HOME), FUN=sum)
names(a) <- c("team", "ft")
a

b <- aggregate(regular$AFTM, by=list(regular$AWAY), FUN=sum)
names(b) <- c("team", "ft")
b

c <- data.frame(a$team, a$ft + b$ft)
names(c) <- c("team", "ft")
c

head(a)
head(b)
head(c)

barplot(main="Histogram stevila zadetih prostih metov po ekipah", c$ft, names.arg=c$team, cex.names=0.75, col="darkgreen", ylim=c(0,5000))

# Graf izrise skatle z brki za domace ekipe
plot(regular$HOME, regular$HPTS, main="Povprecno doseganje tock ekipe na tekmo (domace tekme)", ylab="Stevilo tock", xlab="Ekipa", col="lightblue")

# Graf izrise skatle z brki za gostujoce ekipe
plot(regular$AWAY, regular$APTS, main="Povprecno doseganje tock ekipe na tekmo (gostujoce tekme)", ylab="Stevilo tock", xlab="Ekipa", col="red")


# ------------------------------------------





