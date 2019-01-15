################################
# 2.) KLASIFIKACIJA - POPRAVEK #
################################

# Branje podatkov iz tekstovne datoteke:

regular <- read.table(file="regular.txt", sep=",", header=TRUE)
regular
head(regular)
summary(regular)
str(regular)
names(regular)

# Za ucno mnozico vzamemo prvi dve sezoni (2014-2015 in 2015-2016), 
# za testno pa zadnjo sezono (2015-2016)

regular[as.integer(regular$SEASON) == 1,]
regular[as.integer(regular$SEASON) == 2,]
regular[as.integer(regular$SEASON) == 3,]

ucnaMnozica <- regular[as.integer(regular$SEASON) < 3, ]
summary(ucnaMnozica)

testnaMnozica <- regular[as.integer(regular$SEASON) == 3, ]
summary(testnaMnozica)

# Spremenljivki, ki hranita ime zaporedne zmagovalne / porazene ekipe

winns <- ifelse(ucnaMnozica$HPTS > ucnaMnozica$APTS, as.character(ucnaMnozica$HOME), as.character(ucnaMnozica$AWAY))
losses <- ifelse(ucnaMnozica$HPTS > ucnaMnozica$APTS, as.character(ucnaMnozica$AWAY), as.character(ucnaMnozica$HOME))

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

total$ratio <- total$winns / total$losses
total

# Stevilo zadetih prostih metov na ekipo v vseh treh sezonah

a <- aggregate(ucnaMnozica$HFTM, by=list(ucnaMnozica$HOME), FUN=sum)
names(a) <- c("team", "ft")
a

b <- aggregate(ucnaMnozica$AFTM, by=list(ucnaMnozica$AWAY), FUN=sum)
names(b) <- c("team", "ft")
b

c <- data.frame(a$team, a$ft + b$ft)
names(c) <- c("team", "ft")
c

total <- merge(total, c)
total

# Uspešnost metanja trojk:
a <- aggregate(ucnaMnozica$H3PM / ucnaMnozica$H3PA, by=list(ucnaMnozica$HOME), FUN=mean)
names(a) <- c("team", "threesP")
a

b <- aggregate(ucnaMnozica$A3PM / ucnaMnozica$A3PA, by=list(ucnaMnozica$AWAY), FUN=mean)
names(b) <- c("team", "threesP")
b

c <- data.frame(a$team, (a$threesP + b$threesP) / 2)
names(c) <- c("team", "threesP")
c

total <- merge(total, c)
total

# Uspešnost metanja prostih metov:
a <- aggregate(ucnaMnozica$HFTM / ucnaMnozica$HFTA, by=list(ucnaMnozica$HOME), FUN=mean)
names(a) <- c("team", "ftP")
a

b <- aggregate(ucnaMnozica$AFTM / ucnaMnozica$AFTA, by=list(ucnaMnozica$AWAY), FUN=mean)
names(b) <- c("team", "ftP")
b

c <- data.frame(a$team, (a$ftP + b$ftP) / 2)
names(c) <- c("team", "ftP")
c

total <- merge(total, c)
total

# Uspešnost metov za 2 točki:
a <- aggregate(ucnaMnozica$H2PM / ucnaMnozica$H2PA, by=list(ucnaMnozica$HOME), FUN=mean)
names(a) <- c("team", "twoPP")
a

b <- aggregate(ucnaMnozica$A2PM / ucnaMnozica$A2PA, by=list(ucnaMnozica$AWAY), FUN=mean)
names(b) <- c("team", "twoPP")
b

c <- data.frame(a$team, (a$twoPP + b$twoPP) / 2)
names(c) <- c("team", "twoPP")
c

total <- merge(total, c)
total

# Stevilo blokad

a <- aggregate(ucnaMnozica$HBLK, by=list(ucnaMnozica$HOME), FUN=sum)
names(a) <- c("team", "bl")
a

b <- aggregate(ucnaMnozica$ABLK, by=list(ucnaMnozica$AWAY), FUN=sum)
names(b) <- c("team", "bl")
b

c <- data.frame(a$team, a$bl + b$bl)
names(c) <- c("team", "bl")
c

total <- merge(total, c)
total

# Stevilo skokov v napadu

a <- aggregate(ucnaMnozica$HORB, by=list(ucnaMnozica$HOME), FUN=sum)
names(a) <- c("team", "atJ")
a

b <- aggregate(ucnaMnozica$AORB, by=list(ucnaMnozica$AWAY), FUN=sum)
names(b) <- c("team", "atJ")
b

c <- data.frame(a$team, a$atJ + b$atJ)
names(c) <- c("team", "atJ")
c

total <- merge(total, c)
total

# Št. skokov v obrambi:
a <- aggregate(ucnaMnozica$HDRB, by=list(ucnaMnozica$HOME), FUN=sum)
names(a) <- c("team", "defJ")
a

b <- aggregate(ucnaMnozica$ADRB, by=list(ucnaMnozica$AWAY), FUN=sum)
names(b) <- c("team", "defJ")
b

c <- data.frame(a$team, a$defJ + b$defJ)
names(c) <- c("team", "defJ")
c

total <- merge(total, c)
total

# Št. asistenc:
a <- aggregate(ucnaMnozica$HAST, by=list(ucnaMnozica$HOME), FUN=sum)
names(a) <- c("team", "ass")
a

b <- aggregate(ucnaMnozica$AAST, by=list(ucnaMnozica$AWAY), FUN=sum)
names(b) <- c("team", "ass")
b

c <- data.frame(a$team, a$ass + b$ass)
names(c) <- c("team", "ass")
c

total <- merge(total, c)
total

# Št. ukradenih žog:
a <- aggregate(ucnaMnozica$HSTL, by=list(ucnaMnozica$HOME), FUN=sum)
names(a) <- c("team", "stl")
a

b <- aggregate(ucnaMnozica$ASTL, by=list(ucnaMnozica$AWAY), FUN=sum)
names(b) <- c("team", "stl")
b

c <- data.frame(a$team, a$stl + b$stl)
names(c) <- c("team", "stl")
c

total <- merge(total, c)
total

# Št.izgubljenih:
a <- aggregate(ucnaMnozica$HTOV, by=list(ucnaMnozica$HOME), FUN=sum)
names(a) <- c("team", "lost")
a

b <- aggregate(ucnaMnozica$ATOV, by=list(ucnaMnozica$AWAY), FUN=sum)
names(b) <- c("team", "lost")
b

c <- data.frame(a$team, a$lost + b$lost)
names(c) <- c("team", "lost")
c

total <- merge(total, c)
total

# Osebne napake:
a <- aggregate(ucnaMnozica$HPF, by=list(ucnaMnozica$HOME), FUN=sum)
names(a) <- c("team", "osebne")
a

b <- aggregate(ucnaMnozica$APF, by=list(ucnaMnozica$AWAY), FUN=sum)
names(b) <- c("team", "osebne")
b

c <- data.frame(a$team, a$osebne + b$osebne)
names(c) <- c("team", "osebne")
c

total <- merge(total, c)
total

ucnaMnozica <- total

names(total)

#_________

# Spremenljivki, ki hranita ime zaporedne zmagovalne / porazene ekipe

winns <- ifelse(testnaMnozica$HPTS > testnaMnozica$APTS, as.character(testnaMnozica$HOME), as.character(testnaMnozica$AWAY))
losses <- ifelse(testnaMnozica$HPTS > testnaMnozica$APTS, as.character(testnaMnozica$AWAY), as.character(testnaMnozica$HOME))

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

total$ratio <- total$winns / total$losses
total

# Stevilo zadetih prostih metov na ekipo v vseh treh sezonah

a <- aggregate(testnaMnozica$HFTM, by=list(testnaMnozica$HOME), FUN=sum)
names(a) <- c("team", "ft")
a

b <- aggregate(testnaMnozica$AFTM, by=list(testnaMnozica$AWAY), FUN=sum)
names(b) <- c("team", "ft")
b

c <- data.frame(a$team, a$ft + b$ft)
names(c) <- c("team", "ft")
c

total <- merge(total, c)
total

# Uspešnost metanja trojk:
a <- aggregate(testnaMnozica$H3PM / testnaMnozica$H3PA, by=list(testnaMnozica$HOME), FUN=mean)
names(a) <- c("team", "threesP")
a

b <- aggregate(testnaMnozica$A3PM / testnaMnozica$A3PA, by=list(testnaMnozica$AWAY), FUN=mean)
names(b) <- c("team", "threesP")
b

c <- data.frame(a$team, (a$threesP + b$threesP) / 2)
names(c) <- c("team", "threesP")
c

total <- merge(total, c)
total

# Uspešnost metanja prostih metov:
a <- aggregate(testnaMnozica$HFTM / testnaMnozica$HFTA, by=list(testnaMnozica$HOME), FUN=mean)
names(a) <- c("team", "ftP")
a

b <- aggregate(testnaMnozica$AFTM / testnaMnozica$AFTA, by=list(testnaMnozica$AWAY), FUN=mean)
names(b) <- c("team", "ftP")
b

c <- data.frame(a$team, (a$ftP + b$ftP) / 2)
names(c) <- c("team", "ftP")
c

total <- merge(total, c)
total

# Uspešnost metov za 2 točki:
a <- aggregate(testnaMnozica$H2PM / testnaMnozica$H2PA, by=list(testnaMnozica$HOME), FUN=mean)
names(a) <- c("team", "twoPP")
a

b <- aggregate(testnaMnozica$A2PM / testnaMnozica$A2PA, by=list(testnaMnozica$AWAY), FUN=mean)
names(b) <- c("team", "twoPP")
b

c <- data.frame(a$team, (a$twoPP + b$twoPP) / 2)
names(c) <- c("team", "twoPP")
c

total <- merge(total, c)
total

# Stevilo blokad

a <- aggregate(testnaMnozica$HBLK, by=list(testnaMnozica$HOME), FUN=sum)
names(a) <- c("team", "bl")
a

b <- aggregate(testnaMnozica$ABLK, by=list(testnaMnozica$AWAY), FUN=sum)
names(b) <- c("team", "bl")
b

c <- data.frame(a$team, a$bl + b$bl)
names(c) <- c("team", "bl")
c

total <- merge(total, c)
total

# Stevilo skokov v napadu

a <- aggregate(testnaMnozica$HORB, by=list(testnaMnozica$HOME), FUN=sum)
names(a) <- c("team", "atJ")
a

b <- aggregate(testnaMnozica$AORB, by=list(testnaMnozica$AWAY), FUN=sum)
names(b) <- c("team", "atJ")
b

c <- data.frame(a$team, a$atJ + b$atJ)
names(c) <- c("team", "atJ")
c

total <- merge(total, c)
total

# Št. skokov v obrambi:
a <- aggregate(testnaMnozica$HDRB, by=list(testnaMnozica$HOME), FUN=sum)
names(a) <- c("team", "defJ")
a

b <- aggregate(testnaMnozica$ADRB, by=list(testnaMnozica$AWAY), FUN=sum)
names(b) <- c("team", "defJ")
b

c <- data.frame(a$team, a$defJ + b$defJ)
names(c) <- c("team", "defJ")
c

total <- merge(total, c)
total

# Št. asistenc:
a <- aggregate(testnaMnozica$HAST, by=list(testnaMnozica$HOME), FUN=sum)
names(a) <- c("team", "ass")
a

b <- aggregate(testnaMnozica$AAST, by=list(testnaMnozica$AWAY), FUN=sum)
names(b) <- c("team", "ass")
b

c <- data.frame(a$team, a$ass + b$ass)
names(c) <- c("team", "ass")
c

total <- merge(total, c)
total

# Št. ukradenih žog:
a <- aggregate(testnaMnozica$HSTL, by=list(testnaMnozica$HOME), FUN=sum)
names(a) <- c("team", "stl")
a

b <- aggregate(testnaMnozica$ASTL, by=list(testnaMnozica$AWAY), FUN=sum)
names(b) <- c("team", "stl")
b

c <- data.frame(a$team, a$stl + b$stl)
names(c) <- c("team", "stl")
c

total <- merge(total, c)
total

# Št.izgubljenih:
a <- aggregate(testnaMnozica$HTOV, by=list(testnaMnozica$HOME), FUN=sum)
names(a) <- c("team", "lost")
a

b <- aggregate(testnaMnozica$ATOV, by=list(testnaMnozica$AWAY), FUN=sum)
names(b) <- c("team", "lost")
b

c <- data.frame(a$team, a$lost + b$lost)
names(c) <- c("team", "lost")
c

total <- merge(total, c)
total

# Osebne napake:
a <- aggregate(testnaMnozica$HPF, by=list(testnaMnozica$HOME), FUN=sum)
names(a) <- c("team", "osebne")
a

b <- aggregate(testnaMnozica$APF, by=list(testnaMnozica$AWAY), FUN=sum)
names(b) <- c("team", "osebne")
b

c <- data.frame(a$team, a$osebne + b$osebne)
names(c) <- c("team", "osebne")
c

total <- merge(total, c)
total

testnaMnozica <- total

names(total)


testnaMnozica

#__________

learn <- data.frame(matrix(ncol = 31, nrow = 0))
names(learn) <- c("teamH", "winnsH","lossesH","ratioH","ftH","threesPH", "ftPH", "twoPPH","blH","atJH",  "defJH", "assH",  "stlH",  "lostH", "osebneH", 
                  "teamA", "winnsA","lossesA","ratioA","ftA","threesPA", "ftPA", "twoPPA","blA","atJA",  "defJA", "assA",  "stlA", "lostA", "osebneA", "WINNER")

for (row1 in 1:nrow(testnaMnozica)) {
  r1 <- testnaMnozica[row1, ]
  agg1 <- sum(r1[,-1]) / ncol(r1[, -1])
  names(r1) <- c("teamH", "winnsH","lossesH","ratioH","ftH","threesPH", "ftPH", "twoPPH","blH","atJH",  "defJH", "assH",  "stlH",  "lostH", "osebneH")
  for (row2 in 2:nrow(testnaMnozica) - 1) {
    r2 <- testnaMnozica[row2, ]
    agg2 <- sum(r2[,-1]) / ncol(r2[, -1])
    names(r2) <- c("teamA", "winnsA","lossesA","ratioA","ftA","threesPA", "ftPA", "twoPPA","blA","atJA",  "defJA", "assA",  "stlA", "lostA", "osebneA")
    for (col in 1:ncol(r2)) {
      r1 <- cbind(r1, r2[col])
    }
    ag <- ifelse(agg1 >= agg2, 1, 0)
    r1 <- cbind(r1, WINNER=ag)
    learn[nrow(learn) + 1,] <- r1
    r1 <- testnaMnozica[row1, ]
    names(r1) <- c("teamH", "winnsH","lossesH","ratioH","ftH","threesPH", "ftPH", "twoPPH","blH","atJH",  "defJH", "assH",  "stlH",  "lostH", "osebneH")
  }
}

testnaMnozica <- learn
testnaMnozica$WINNER <- as.factor(testnaMnozica$WINNER)
testnaMnozica$teamA <- as.factor(testnaMnozica$teamA)
testnaMnozica$teamH <- as.factor(testnaMnozica$teamH)

#__________

learn <- data.frame(matrix(ncol = 31, nrow = 0))
names(learn) <- c("teamH", "winnsH","lossesH","ratioH","ftH","threesPH", "ftPH", "twoPPH","blH","atJH",  "defJH", "assH",  "stlH",  "lostH", "osebneH", 
                  "teamA", "winnsA","lossesA","ratioA","ftA","threesPA", "ftPA", "twoPPA","blA","atJA",  "defJA", "assA",  "stlA", "lostA", "osebneA", "WINNER")

for (row1 in 1:nrow(ucnaMnozica)) {
  r1 <- ucnaMnozica[row1, ]
  agg1 <- sum(r1[,-1]) / ncol(r1[, -1])
  names(r1) <- c("teamH", "winnsH","lossesH","ratioH","ftH","threesPH", "ftPH", "twoPPH","blH","atJH",  "defJH", "assH",  "stlH",  "lostH", "osebneH")
  for (row2 in 2:nrow(ucnaMnozica) - 1) {
    r2 <- ucnaMnozica[row2, ]
    agg2 <- sum(r2[,-1]) / ncol(r2[, -1])
    names(r2) <- c("teamA", "winnsA","lossesA","ratioA","ftA","threesPA", "ftPA", "twoPPA","blA","atJA",  "defJA", "assA",  "stlA", "lostA", "osebneA")
    for (col in 1:ncol(r2)) {
      r1 <- cbind(r1, r2[col])
    }
    ag <- ifelse(agg1 >= agg2, 1, 0)
    r1 <- cbind(r1, WINNER=ag)
    learn[nrow(learn) + 1,] <- r1
    r1 <- ucnaMnozica[row1, ]
    names(r1) <- c("teamH", "winnsH","lossesH","ratioH","ftH","threesPH", "ftPH", "twoPPH","blH","atJH",  "defJH", "assH",  "stlH",  "lostH", "osebneH")
  }
}

ucnaMnozica <- learn
ucnaMnozica$WINNER <- as.factor(ucnaMnozica$WINNER)
ucnaMnozica$teamA <- as.factor(ucnaMnozica$teamA)
ucnaMnozica$teamH <- as.factor(ucnaMnozica$teamH)

ucnaMnozica
testnaMnozica

# Vecinski razred je prvi
majorityClass <- which.max(ucnaMnozica$WINNER)
majorityClass

# Tocnost vecinskega klasifikatorja
# (delez pravilnih napovedi, ce bi vse testne primere klasificirali v vecinski razred)

tocnost <- sum(ucnaMnozica$WINNER == majorityClass) / nrow(ucnaMnozica)
tocnost

# b) ODLOCITVENO DREVO

#Nalozimo knjiznico "rpart".
library(rpart)

# Zgradimo odlocitveno drevo
dt <- rpart(WINNER ~ winnsH+lossesH+ratioH+ftH+threesPH+ftPH+ twoPPH+blH+atJH+defJH+assH+stlH+lostH+osebneH+ 
            winnsA+lossesA+ratioA+ftA+threesPA+ ftPA+ twoPPA+blA+atJA+defJA+assA+stlA+lostA+osebneA, data = ucnaMnozica)
dt

# Izris odlocitvenega drevesa
plot(dt)
text(dt, pretty = 0, cex = 0.8)

# Prave vrednosti testnih primerov

observed <- testnaMnozica$WINNER
observed

# Napovedane vrednosti modela
# Uporabimo funkcijo "predict", ki potrebuje model, testne primere in obliko, 
# v kateri naj poda svoje napovedi. Nastavitev "class" pomeni, da nas zanimajo
# samo razredi, v katere je model klasificiral testne primere.
predicted <- predict(dt, testnaMnozica, type = "class")
predicted

# Zgradimo tabelo napacnih klasifikacij
t <- table(observed, predicted)
t

# Klasifikacijska tocnost modela
sum(diag(t)) / sum(t)

# Funkcija za izracun klasifikacijske tocnosti modela (CA)
CA <- function(prave, napovedane)
{
  t <- table(prave, napovedane)
  
  sum(diag(t)) / sum(t)
}

# Klic funkcije za klasifikacijsko tocnost
CA(observed, predicted)

#______

# Druga oblika napovedi modela (nastavitev "prob") vraca verjetnosti, 
# da posamezni testni primer pripada dolocenemu razredu.

# Napovedane verjetnosti pripadnosti razredom (dobimo matriko)
predMat <- predict(dt, testnaMnozica, type = "prob")
predMat

# Prave verjetnosti pripadnosti razredom (dejanski razred ima verjetnost 1.0 ostali pa 0.0)
obsMat <- model.matrix( ~ WINNER-1, testnaMnozica)
obsMat

# Funkcija za izracun Brierjeve mere
brier.score <- function(observedMatrix, predictedMatrix)
{
  sum((observedMatrix - predictedMatrix) ^ 2) / nrow(predictedMatrix)
}

# Izracun Brierjeve mere za napovedi drevesa
brier.score(obsMat, predMat)

# c) PRECNO PREVERJANJE

install.packages(c("ipred", "prodlim", "CORElearn"))
library(ipred)

# funkcija "errorest" potrebuje poleg formule modela in ucnih primerov se dva vhodna parametra: 
# funkcijo za gradnjo modela ter funkcijo za generiranje napovedi tega modela.
# Funkcijo za generiranje napovedi napisemo tako, da klicemo obicajno funkcijo "predict" 
# s parametrom type="class", ter na ta nacin zahtevamo napovedi v obliki vektorja oznak razredov.

mypredict <- function(object, newdata){predict(object, newdata, type = "class")}

res <- errorest(WINNER~ winnsH+lossesH+ratioH+ftH+threesPH+ftPH+ twoPPH+blH+atJH+defJH+assH+stlH+lostH+osebneH+ 
                  winnsA+lossesA+ratioA+ftA+threesPA+ ftPA+ twoPPA+blA+atJA+defJA+assA+stlA+lostA+osebneA, data=testnaMnozica, model = rpart, predict = mypredict)
res

# klasifikacijska tocnost
1-res$error

# metoda "izloci enega"
res <- errorest(WINNER~ winnsH+lossesH+ratioH+ftH+threesPH+ftPH+ twoPPH+blH+atJH+defJH+assH+stlH+lostH+osebneH+ 
                  winnsA+lossesA+ratioA+ftA+threesPA+ ftPA+ twoPPA+blA+atJA+defJA+assA+stlA+lostA+osebneA, data=testnaMnozica, model = rpart, predict = mypredict, est.para=control.errorest(k = nrow(regular)))
res

# klasifikacijska tocnost
1-res$error

#
# odlocitveno drevo (CORElearn)
#

library(CORElearn)

# Model bomo zgradili s pomocjo funkcije "CoreModel", ki potrebuje informacijo o tem, kateri tip modela naj zgradi.
# Funkcijo za gradnjo modela napisemo tako, da klicu funkcije "CoreModel" dodamo parameter za izbiro tipa modela.
mymodel.coremodel <- function(formula, data, target.model){CoreModel(formula, data, model=target.model)}

# Funkcijo za generiranje napovedi napisemo tako, da iz dobljenih napovedi modela obdrzimo samo oznake razredov.
# Ko model vrne zahtevane napovedi, ga ne potrebujemo vec - zato ga odstranimo iz pomnilnika.
mypredict.coremodel <- function(object, newdata) {pred <- predict(object, newdata)$class; destroyModels(object); pred}

# 10-kratno precno preverjanje 
res <- errorest(WINNER~winnsH+lossesH+ratioH+ftH+threesPH+ftPH+ twoPPH+blH+atJH+defJH+assH+stlH+lostH+osebneH+ 
                  winnsA+lossesA+ratioA+ftA+threesPA+ ftPA+ twoPPA+blA+atJA+defJA+assA+stlA+lostA+osebneA, data=testnaMnozica, model = mymodel.coremodel, predict = mypredict.coremodel, target.model = "tree")
1-res$error

# metoda "izloci enega"
res <- errorest(WINNER~winnsH+lossesH+ratioH+ftH+threesPH+ftPH+ twoPPH+blH+atJH+defJH+assH+stlH+lostH+osebneH+ 
                  winnsA+lossesA+ratioA+ftA+threesPA+ ftPA+ twoPPA+blA+atJA+defJA+assA+stlA+lostA+osebneA, data=regular, model = mymodel.coremodel, predict = mypredict.coremodel, target.model = "tree", est.para=control.errorest(k = nrow(regular)))
1-res$error








# Funkcija za izracun Brierjeve mere
brier.score <- function(observedMatrix, predictedMatrix)
{
  sum((observedMatrix - predictedMatrix) ^ 2) / nrow(predictedMatrix)
}

# Funkcija za izracun klasifikacijske tocnosti
CA <- function(observed, predicted)
{
  t <- table(observed, predicted)
  
  sum(diag(t)) / sum(t)
}

# metoda precnega preverjanja je implementirana v knjiznici "ipred"
library(ipred)

# pomozne funkcije, ki jih potrebujemo za izvedbo precnega preverjanja
mypredict.generic <- function(object, newdata){predict(object, newdata, type = "class")}
mymodel.coremodel <- function(formula, data, target.model){CoreModel(formula, data, model=target.model)}
mypredict.coremodel <- function(object, newdata) {pred <- predict(object, newdata)$class; destroyModels(object); pred}




#
#
# NAIVNI BAYESOV KLASIFIKATOR
#
#

# gradnja modela s pomocjo knjiznice "e1071"

library(e1071)

nb <- naiveBayes(WINNER ~ winnsH+lossesH+ratioH+ftH+threesPH+ftPH+ twoPPH+blH+atJH+defJH+assH+stlH+lostH+osebneH+ 
                   winnsA+lossesA+ratioA+ftA+threesPA+ ftPA+ twoPPA+blA+atJA+defJA+assA+stlA+lostA+osebneA, data = ucnaMnozica)
predicted <- predict(nb, testnaMnozica, type="class")
CA(observed, predicted)

predMat <- predict(nb, testnaMnozica, type = "raw")
obsMat <- model.matrix(~WINNER-1, testnaMnozica)
brier.score(obsMat, predMat)

errorest(WINNER~ winnsH+lossesH+ratioH+ftH+threesPH+ftPH+ twoPPH+blH+atJH+defJH+assH+stlH+lostH+osebneH+ 
           winnsA+lossesA+ratioA+ftA+threesPA+ ftPA+ twoPPA+blA+atJA+defJA+assA+stlA+lostA+osebneA, data=ucnaMnozica, model = naiveBayes, predict = mypredict.generic)


# gradnja modela s pomocjo knjiznice "CORElearn"

library(CORElearn)
cm.nb <- CoreModel(WINNER ~ winnsH+lossesH+ratioH+ftH+threesPH+ftPH+ twoPPH+blH+atJH+defJH+assH+stlH+lostH+osebneH+ 
                     winnsA+lossesA+ratioA+ftA+threesPA+ ftPA+ twoPPA+blA+atJA+defJA+assA+stlA+lostA+osebneA, data = ucnaMnozica, model="bayes")
predicted <- predict(cm.nb, testnaMnozica, type="class")
CA(observed, predicted)

predMat <- predict(cm.nb, testnaMnozica, type = "probability")
brier.score(obsMat, predMat)

errorest(WINNER~winnsH+lossesH+ratioH+ftH+threesPH+ftPH+ twoPPH+blH+atJH+defJH+assH+stlH+lostH+osebneH+ 
           winnsA+lossesA+ratioA+ftA+threesPA+ ftPA+ twoPPA+blA+atJA+defJA+assA+stlA+lostA+osebneA, data=ucnaMnozica, model = mymodel.coremodel, predict = mypredict.coremodel, target.model="bayes")




#
#
# K-NAJBLIZJIH SOSEDOV
#
#

# gradnja modela s pomocjo knjiznice "CORElearn"

library(CORElearn)
cm.knn <- CoreModel(WINNER ~ winnsH+lossesH+ratioH+ftH+threesPH+ftPH+ twoPPH+blH+atJH+defJH+assH+stlH+lostH+osebneH+ 
                      winnsA+lossesA+ratioA+ftA+threesPA+ ftPA+ twoPPA+blA+atJA+defJA+assA+stlA+lostA+osebneA, data = ucnaMnozica, model="knn", kInNN = 5)
predicted <- predict(cm.knn, testnaMnozica, type="class")
CA(observed, predicted)

predMat <- predict(cm.knn, testnaMnozica, type = "probability")
brier.score(obsMat, predMat)

errorest(WINNER~winnsH+lossesH+ratioH+ftH+threesPH+ftPH+ twoPPH+blH+atJH+defJH+assH+stlH+lostH+osebneH+ 
           winnsA+lossesA+ratioA+ftA+threesPA+ ftPA+ twoPPA+blA+atJA+defJA+assA+stlA+lostA+osebneA, data=ucnaMnozica, model = mymodel.coremodel, predict = mypredict.coremodel, target.model="knn")




#
#
# NAKLJUCNI GOZD
#
#

# gradnja modela s pomocjo knjiznice "randomForest"

library(randomForest)

rf <- randomForest(WINNER ~ winnsH+lossesH+ratioH+ftH+threesPH+ftPH+ twoPPH+blH+atJH+defJH+assH+stlH+lostH+osebneH+ 
                     winnsA+lossesA+ratioA+ftA+threesPA+ ftPA+ twoPPA+blA+atJA+defJA+assA+stlA+lostA+osebneA, data = ucnaMnozica)
predicted <- predict(rf, testnaMnozica, type="class")
CA(observed, predicted)

predMat <- predict(rf, testnaMnozica, type = "prob")
brier.score(obsMat, predMat)

mypredict.rf <- function(object, newdata){predict(object, newdata, type = "class")}
errorest(WINNER~ winnsH+lossesH+ratioH+ftH+threesPH+ftPH+ twoPPH+blH+atJH+defJH+assH+stlH+lostH+osebneH+ 
           winnsA+lossesA+ratioA+ftA+threesPA+ ftPA+ twoPPA+blA+atJA+defJA+assA+stlA+lostA+osebneA, data=ucnaMnozica, model = randomForest, predict = mypredict.generic)



# gradnja modela s pomocjo knjiznice "CORElearn"

library(CORElearn)
cm.rf <- CoreModel(WINNER ~ winnsH+lossesH+ratioH+ftH+threesPH+ftPH+ twoPPH+blH+atJH+defJH+assH+stlH+lostH+osebneH+ 
                     winnsA+lossesA+ratioA+ftA+threesPA+ ftPA+ twoPPA+blA+atJA+defJA+assA+stlA+lostA+osebneA, data = ucnaMnozica, model="rf")
predicted <- predict(cm.rf, testnaMnozica, type="class")
CA(observed, predicted)

predMat <- predict(cm.rf, testnaMnozica, type = "probability")
brier.score(obsMat, predMat)

errorest(WINNER~., data=ucnaMnozica, model = mymodel.coremodel, predict = mypredict.coremodel, target.model="rf")




#
#
# SVM
#
#

# gradnja modela s pomocjo knjiznice "e1071"

library(e1071)

sm <- svm(WINNER ~ winnsH+lossesH+ratioH+ftH+threesPH+ftPH+ twoPPH+blH+atJH+defJH+assH+stlH+lostH+osebneH+ 
            winnsA+lossesA+ratioA+ftA+threesPA+ ftPA+ twoPPA+blA+atJA+defJA+assA+stlA+lostA+osebneA, data = ucnaMnozica)
predicted <- predict(sm, testnaMnozica, type="class")
CA(observed, predicted)

sm <- svm(WINNER ~ winnsH+lossesH+ratioH+ftH+threesPH+ftPH+ twoPPH+blH+atJH+defJH+assH+stlH+lostH+osebneH+ 
            winnsA+lossesA+ratioA+ftA+threesPA+ ftPA+ twoPPA+blA+atJA+defJA+assA+stlA+lostA+osebneA, ucnaMnozica, probability = T)
pred <- predict(sm, testnaMnozica, probability = T)
predMat <- attr(pred, "probabilities")

# v tem konkretnem primeru, vrstni red razredov (stolpcev) v matriki predMat je 
# obraten kot v matriki obsMat. 
colnames(obsMat)
colnames(predMat)

# Iz tega razloga zamenjemo vrstni red stolpcev v matriki predMat
brier.score(obsMat, predMat[,c(2,1)])

errorest(WINNER~., data=ucnaMnozica, model = svm, predict = mypredict.generic)


# gradnja modela s pomocjo knjiznice "kernlab"

library(kernlab)

model.svm <- ksvm(WINNER ~ winnsH+lossesH+ratioH+ftH+threesPH+ftPH+ twoPPH+blH+atJH+defJH+assH+stlH+lostH+osebneH+ 
                    winnsA+lossesA+ratioA+ftA+threesPA+ ftPA+ twoPPA+blA+atJA+defJA+assA+stlA+lostA+osebneA, data = ucnaMnozica, kernel = "rbfdot")
predicted <- predict(model.svm, testnaMnozica, type = "response")
CA(observed, predicted)

model.svm <- ksvm(WINNER ~ winnsH+lossesH+ratioH+ftH+threesPH+ftPH+ twoPPH+blH+atJH+defJH+assH+stlH+lostH+osebneH+ 
                    winnsA+lossesA+ratioA+ftA+threesPA+ ftPA+ twoPPA+blA+atJA+defJA+assA+stlA+lostA+osebneA, data = ucnaMnozica, kernel = "rbfdot", prob.model = T)
predMat <- predict(model.svm, testnaMnozica, type = "prob")
brier.score(obsMat, predMat)

mypredict.ksvm <- function(object, newdata){predict(object, newdata, type = "response")}
errorest(WINNER~., data=ucnaMnozica, model = ksvm, predict = mypredict.ksvm)

#
#
# UMETNE NEVRONSKE MREZE
#
#

# gradnja modela s pomocjo knjiznice "nnet"

library(nnet)

# implementacija funkcije za ucenje nevronske mreze daje boljse rezultate v primeru,
# ko so ucni primeri normalizirani. 

scale.data <- function(data)
{
  norm.data <- data
  
  for (i in 1:ncol(data))
  {
    if (!is.factor(data[,i]))
      norm.data[,i] <- scale(data[,i])
  }
  
  norm.data
}

# ce normaliziramo ucne primere, je potrebno na enak nacin normalizirati 
# tudi testne primere!

norm.data <- scale.data(rbind(ucnaMnozica,testnaMnozica))
norm.learn <- norm.data[1:nrow(ucnaMnozica),]
norm.test <- norm.data[-(1:nrow(ucnaMnozica)),]

nn <- nnet(WINNER ~ winnsH+lossesH+ratioH+ftH+threesPH+ftPH+ twoPPH+blH+atJH+defJH+assH+stlH+lostH+osebneH+ 
             winnsA+lossesA+ratioA+ftA+threesPA+ ftPA+ twoPPA+blA+atJA+defJA+assA+stlA+lostA+osebneA, data = norm.learn, size = 5, decay = 0.0001, maxit = 10000)
predicted <- predict(nn, norm.test, type = "class")
CA(observed, predicted)

# v primeru binarne klasifikacije bo funkcija predict vrnila verjetnosti samo enega razreda.
# celotno matriko moramo rekonstruirati sami

pm <- predict(nn, norm.test, type = "raw")
predMat <- cbind(1-pm, pm)
brier.score(obsMat, predMat)

mypredict.nnet <- function(object, newdata){as.factor(predict(object, newdata, type = "class"))}
errorest(WINNER~., data=norm.learn, model = nnet, predict = mypredict.nnet, size = 5, decay = 0.0001, maxit = 10000)



