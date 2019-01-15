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


# Stevilo tock:
a <- aggregate(ucnaMnozica$HPTS, by=list(ucnaMnozica$HOME), FUN=sum)
names(a) <- c("team", "pts")
a

b <- aggregate(ucnaMnozica$APTS, by=list(ucnaMnozica$AWAY), FUN=sum)
names(b) <- c("team", "pts")
b

c <- data.frame(a$team, a$pts + b$pts)
names(c) <- c("team", "pts")
c

total <- merge(total, c)
total

# Povprecno tock na tekmo:

total$apts <- total$pts / (total$winns + total$losses)
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

# UspeĹˇnost metanja trojk:
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

# UspeĹˇnost metanja prostih metov:
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

# UspeĹˇnost metov za 2 toÄŤki:
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

# Ĺ t. skokov v obrambi:
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

# Ĺ t. asistenc:
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

# Ĺ t. ukradenih Ĺľog:
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

# Ĺ t.izgubljenih:
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

names(ucnaMnozica)

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

# Stevilo tock:
a <- aggregate(testnaMnozica$HPTS, by=list(testnaMnozica$HOME), FUN=sum)
names(a) <- c("team", "pts")
a

b <- aggregate(testnaMnozica$APTS, by=list(testnaMnozica$AWAY), FUN=sum)
names(b) <- c("team", "pts")
b

c <- data.frame(a$team, a$pts + b$pts)
names(c) <- c("team", "pts")
c

total <- merge(total, c)
total

# Povprecno tock na tekmo:

total$apts <- total$pts / (total$winns + total$losses)
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

# UspeĹˇnost metanja trojk:
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

# UspeĹˇnost metanja prostih metov:
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

# UspeĹˇnost metov za 2 toÄŤki:
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

# Ĺ t. skokov v obrambi:
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

# Ĺ t. asistenc:
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

# Ĺ t. ukradenih Ĺľog:
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

# Ĺ t.izgubljenih:
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

names(testnaMnozica)

#___________

learn <- data.frame(matrix(ncol = 35, nrow = 0))
names(learn) <- c("teamH", "winnsH","lossesH","ratioH", "ptsH", "aptsH", "ftH","threesPH", "ftPH", "twoPPH","blH","atJH",  "defJH", "assH",  "stlH",  "lostH", "osebneH", 
                  "teamA", "winnsA","lossesA","ratioA", "ptsA", "aptsA", "ftA","threesPA", "ftPA", "twoPPA","blA","atJA",  "defJA", "assA",  "stlA", "lostA", "osebneA", "PDIFF")

for (row1 in 1:nrow(ucnaMnozica)) {
  r1 <- ucnaMnozica[row1, ]
  agg1 <- r1$apts
  names(r1) <- c("teamH", "winnsH","lossesH","ratioH","ptsH", "aptsH", "ftH","threesPH", "ftPH", "twoPPH","blH","atJH",  "defJH", "assH",  "stlH",  "lostH", "osebneH")
  for (row2 in 2:nrow(ucnaMnozica) - 1) {
    r2 <- ucnaMnozica[row2, ]
    agg2 <- r2$apts
    names(r2) <- c("teamA", "winnsA","lossesA","ratioA","ptsA", "aptsA", "ftA","threesPA", "ftPA", "twoPPA","blA","atJA",  "defJA", "assA",  "stlA", "lostA", "osebneA")
    for (col in 1:ncol(r2)) {
      r1 <- cbind(r1, r2[col])
    }
    ag <- (agg1 - agg2)
    r1 <- cbind(r1, PDIFF=ag)
    learn[nrow(learn) + 1,] <- r1
    r1 <- ucnaMnozica[row1, ]
    names(r1) <- c("teamH", "winnsH","lossesH","ratioH","ptsH", "aptsH","ftH","threesPH", "ftPH", "twoPPH","blH","atJH",  "defJH", "assH",  "stlH",  "lostH", "osebneH")
  }
}

ucnaMnozica <- learn
ucnaMnozica
ucnaMnozica$teamA <- as.factor(ucnaMnozica$teamA)
ucnaMnozica$teamH <- as.factor(ucnaMnozica$teamH)

#___________

learn <- data.frame(matrix(ncol = 35, nrow = 0))
names(learn) <- c("teamH", "winnsH","lossesH","ratioH", "ptsH", "aptsH", "ftH","threesPH", "ftPH", "twoPPH","blH","atJH",  "defJH", "assH",  "stlH",  "lostH", "osebneH", 
                  "teamA", "winnsA","lossesA","ratioA", "ptsA", "aptsA", "ftA","threesPA", "ftPA", "twoPPA","blA","atJA",  "defJA", "assA",  "stlA", "lostA", "osebneA", "PDIFF")

for (row1 in 1:nrow(testnaMnozica)) {
  r1 <- testnaMnozica[row1, ]
  agg1 <- r1$apts
  names(r1) <- c("teamH", "winnsH","lossesH","ratioH","ptsH", "aptsH", "ftH","threesPH", "ftPH", "twoPPH","blH","atJH",  "defJH", "assH",  "stlH",  "lostH", "osebneH")
  for (row2 in 2:nrow(testnaMnozica) - 1) {
    r2 <- testnaMnozica[row2, ]
    agg2 <- r2$apts
    names(r2) <- c("teamA", "winnsA","lossesA","ratioA","ptsA", "aptsA", "ftA","threesPA", "ftPA", "twoPPA","blA","atJA",  "defJA", "assA",  "stlA", "lostA", "osebneA")
    for (col in 1:ncol(r2)) {
      r1 <- cbind(r1, r2[col])
    }
    ag <- (agg1 - agg2)
    r1 <- cbind(r1, PDIFF=ag)
    learn[nrow(learn) + 1,] <- r1
    r1 <- testnaMnozica[row1, ]
    names(r1) <- c("teamH", "winnsH","lossesH","ratioH","ptsH", "aptsH","ftH","threesPH", "ftPH", "twoPPH","blH","atJH",  "defJH", "assH",  "stlH",  "lostH", "osebneH")
  }
}

testnaMnozica <- learn
testnaMnozica
testnaMnozica$teamA <- as.factor(testnaMnozica$teamA)
testnaMnozica$teamH <- as.factor(testnaMnozica$teamH)

#_________________________________

####################################################################
#
# Mere za ocenjevanje ucenja v regresiji
#
####################################################################

mae <- function(observed, predicted)
{
  mean(abs(observed - predicted))
}

rmae <- function(observed, predicted, mean.val) 
{  
  sum(abs(observed - predicted)) / sum(abs(observed - mean.val))
}

mse <- function(observed, predicted)
{
  mean((observed - predicted)^2)
}

rmse <- function(observed, predicted, mean.val) 
{  
  sum((observed - predicted)^2)/sum((observed - mean.val)^2)
}

#
# linearni model
#

observed <- testnaMnozica$PDIFF
lm.model <- lm(PDIFF ~ winnsH+lossesH+ratioH+ftH+threesPH+ftPH+ twoPPH+blH+atJH+defJH+assH+stlH+lostH+osebneH+ 
                 winnsA+lossesA+ratioA+ftA+threesPA+ ftPA+ twoPPA+blA+atJA+defJA+assA+stlA+lostA+osebneA, data = ucnaMnozica)
lm.model

predicted <- predict(lm.model, testnaMnozica)
mae(observed, predicted)
rmae(observed, predicted, mean(ucnaMnozica$PDIFF))


#
# regresijsko drevo
#

library(rpart)

rt.model <- rpart(PDIFF ~ winnsH+lossesH+ratioH+ftH+threesPH+ftPH+ twoPPH+blH+atJH+defJH+assH+stlH+lostH+osebneH+ 
                    winnsA+lossesA+ratioA+ftA+threesPA+ ftPA+ twoPPA+blA+atJA+defJA+assA+stlA+lostA+osebneA, ucnaMnozica)
predicted <- predict(rt.model, testnaMnozica)
rmae(observed, predicted, mean(ucnaMnozica$PDIFF))

mae(observed, predicted)

plot(rt.model);text(rt.model, pretty = 0)


# nastavitve za gradnjo drevesa
rpart.control()

# zgradimo drevo z drugimi parametri
rt <- rpart(PDIFF ~ ., ucnaMnozica, minsplit = 100)
plot(rt);text(rt, pretty = 0)

# parameter cp kontrolira rezanje drevesa
rt.model <- rpart(PDIFF ~ winnsH+lossesH+ratioH+ftH+threesPH+ftPH+ twoPPH+blH+atJH+defJH+assH+stlH+lostH+osebneH+ 
                    winnsA+lossesA+ratioA+ftA+threesPA+ ftPA+ twoPPA+blA+atJA+defJA+assA+stlA+lostA+osebneA, ucnaMnozica, cp = 0)
plot(rt.model);text(rt.model, pretty = 0)

# drugacno drevo
rt <- rpart(PDIFF ~ H2PM+H2PA+H3PM+H3PA+HFTM+HFTA+HORB+HDRB+HAST+HSTL+HTOV+
              HBLK+HPF+A2PM+A2PA+A3PM+A3PA+AFTM+AFTA+AORB+
              ADRB+AAST+ASTL+ATOV+ABLK+APF, ucnaMnozica, minsplit = 100)
plot(rt);text(rt, pretty = 0)


# izpisemo ocenjene napake drevesa za razlicne vrednosti parametra cp
printcp(rt.model)

# drevo porezemo z ustrezno vrednostjo cp, pri kateri je bila minimalna napaka
rt.model2 <- prune(rt.model, cp = 0.02)
plot(rt.model2);text(rt.model2, pretty = 0)
predicted <- predict(rt.model2, testna)
rmae(observed, predicted, mean(ucnaMnozica$PDIFF))


# regresijska drevesa lahko gradimo tudi s pomocjo knjiznice CORElearn
library(CORElearn)

rt.core <- CoreModel(PDIFF ~ winnsH+lossesH+ratioH+ftH+threesPH+ftPH+ twoPPH+blH+atJH+defJH+assH+stlH+lostH+osebneH+ 
                       winnsA+lossesA+ratioA+ftA+threesPA+ ftPA+ twoPPA+blA+atJA+defJA+assA+stlA+lostA+osebneA, data=ucnaMnozica, model="regTree", modelTypeReg = 1)
plot(rt.core, ucnaMnozica)
predicted <- predict(rt.core, testnaMnozica)
rmae(observed, predicted, mean(ucnaMnozica$PDIFF))



modelEval(rt.core, testnaMnozica$PDIFF, predicted)

# preveliko drevo se prevec prilagodi podatkom in slabse napoveduje odvisno spremeljivko
rt.core2 <- CoreModel(PDIFF ~ winnsH+lossesH+ratioH+ftH+threesPH+ftPH+ twoPPH+blH+atJH+defJH+assH+stlH+lostH+osebneH+ 
                        winnsA+lossesA+ratioA+ftA+threesPA+ ftPA+ twoPPA+blA+atJA+defJA+assA+stlA+lostA+osebneA, data=ucnaMnozica, model="regTree",  modelTypeReg = 1, minNodeWeightTree = 1, selectedPrunerReg = 0)
plot(rt.core2, ucnaMnozica)
predicted <- predict(rt.core2, testnaMnozica)
rmae(observed, predicted, mean(ucnaMnozica$PDIFF))

# drevo z linearnim modelom v listih se lahko prevec prilagodi ucnim primerom
rt.core3 <- CoreModel(PDIFF ~ winnsH+lossesH+ratioH+ftH+threesPH+ftPH+ twoPPH+blH+atJH+defJH+assH+stlH+lostH+osebneH+ 
                        winnsA+lossesA+ratioA+ftA+threesPA+ ftPA+ twoPPA+blA+atJA+defJA+assA+stlA+lostA+osebneA, data=ucnaMnozica, model="regTree",  modelTypeReg = 3, selectedPrunerReg = 2)
plot(rt.core3, ucnaMnozica)
predicted <- predict(rt.core3, testnaMnozica)
rmae(observed, predicted, mean(ucnaMnozica$PDIFF))

# model se prilega ucnim podatkom, ker ima prevec parametrov.
# rezultat lahko izboljsamo tako, da poenostavimo model (npr. uporabimo manj atributov)

rt.core4 <- CoreModel(PDIFF ~ winnsH+lossesH+ratioH+ftH+threesPH+ftPH+ twoPPH+blH+atJH+defJH+assH+stlH+lostH+osebneH+ 
                        winnsA+lossesA+ratioA+ftA+threesPA+ ftPA+ twoPPA+blA+atJA+defJA+assA+stlA+lostA+osebneA, data=ucnaMnozica, model="regTree", modelTypeReg = 3)
plot(rt.core4, testnaMnozica)
predicted <- predict(rt.core4, testnaMnozica)
rmae(observed, predicted, mean(ucnaMnozica$PDIFF))

#
# nakljucni gozd
#

library(randomForest)

rf.model <- randomForest(PDIFF ~ winnsH+lossesH+ratioH+ftH+threesPH+ftPH+ twoPPH+blH+atJH+defJH+assH+stlH+lostH+osebneH+ 
                           winnsA+lossesA+ratioA+ftA+threesPA+ ftPA+ twoPPA+blA+atJA+defJA+assA+stlA+lostA+osebneA, ucnaMnozica)
predicted <- predict(rf.model, testnaMnozica)
rmae(observed, predicted, mean(ucnaMnozica$PDIFF))

mae(observed, predicted)

# Vsi atributi

rf.model <- randomForest(PDIFF ~ ., ucnaMnozica)
predicted <- predict(rf.model, testnaMnozica)
rmae(observed, predicted, mean(ucnaMnozica$PDIFF))

mae(observed, predicted)

#
# svm
#

library(e1071)

svm.model <- svm(PDIFF ~ winnsH+lossesH+ratioH+ftH+threesPH+ftPH+ twoPPH+blH+atJH+defJH+assH+stlH+lostH+osebneH+ 
                   winnsA+lossesA+ratioA+ftA+threesPA+ ftPA+ twoPPA+blA+atJA+defJA+assA+stlA+lostA+osebneA, ucnaMnozica)
predicted <- predict(svm.model, testnaMnozica)
rmae(observed, predicted, mean(ucnaMnozica$PDIFF))

mae(observed, predicted)

# Vsi atributi

svm.model <- svm(PDIFF ~ ., ucnaMnozica)
predicted <- predict(svm.model, testnaMnozica)
rmae(observed, predicted, mean(ucnaMnozica$PDIFF))

mae(observed, predicted)


#
# k-najblizjih sosedov
#

library(kknn)

knn.model <- kknn(PDIFF ~ winnsH+lossesH+ratioH+ftH+threesPH+ftPH+ twoPPH+blH+atJH+defJH+assH+stlH+lostH+osebneH+ 
                    winnsA+lossesA+ratioA+ftA+threesPA+ ftPA+ twoPPA+blA+atJA+defJA+assA+stlA+lostA+osebneA, ucnaMnozica, testnaMnozica, k = 5)
predicted <- fitted(knn.model)
rmae(observed, predicted, mean(ucnaMnozica$PDIFF))

mae(observed, predicted)

# Vsi atributi

knn.model <- kknn(PDIFF ~ ., ucnaMnozica, testnaMnozica, k = 5)
predicted <- fitted(knn.model)
rmae(observed, predicted, mean(ucnaMnozica$PDIFF))

mae(observed, predicted)


#
# nevronska mreza
#

library(nnet)

#
# pomembno!!! 
# za regresijo je potrebno nastaviti linout = T

# zaradi nakljucne izbire zacetnih utezi bo vsakic nekoliko drugacen rezultat
# zato je dobro, da veckrat naucimo mrezo in zadrzimo model, ki se je najboljse obnesel

#set.seed(6789)
nn.model <- nnet(PDIFF ~ winnsH+lossesH+ratioH+ftH+threesPH+ftPH+ twoPPH+blH+atJH+defJH+assH+stlH+lostH+osebneH+ 
                   winnsA+lossesA+ratioA+ftA+threesPA+ ftPA+ twoPPA+blA+atJA+defJA+assA+stlA+lostA+osebneA, ucnaMnozica, size = 5, decay = 0.0001, maxit = 10000, linout = T)
predicted <- predict(nn.model, testnaMnozica)
rmae(observed, predicted, mean(ucnaMnozica$PDIFF))

mae(observed, predicted)


# Vsi atributi

nn.model <- nnet(PDIFF ~ ., ucnaMnozica, size = 5, decay = 0.0001, maxit = 10000, linout = T)
predicted <- predict(nn.model, testnaMnozica)
rmae(observed, predicted, mean(ucnaMnozica$PDIFF))

mae(observed, predicted)


#######################################################################################
#
# Izbira podmnozice atributov
#
#######################################################################################

#
# ocena kvalitete atributov pri regresijskih problemih
#

sort(attrEval(PDIFF ~ ., ucnaMnozica, "MSEofMean"), decreasing = TRUE)
sort(attrEval(PDIFF ~ ., ucnaMnozica, "RReliefFexpRank"), decreasing = TRUE)


# model lahko dodatno izboljsamo z izbiro ustrezne podmnozice atributov

rt.core <- CoreModel(PDIFF ~ ., data=ucnaMnozica, model="regTree", selectionEstimatorReg="MSEofMean")
plot(rt.core, ucnaMnozica)
predicted <- predict(rt.core, testnaMnozica)
rmae(observed, predicted, mean(ucnaMnozica$PDIFF))

