#################
##Libraries#####
################
library(plotrix)
library(reshape2)
library(psych)
library(dplyr)
library(ggplot2)
library(stringr)
library(RColorBrewer)
library("ggpubr")
coul <- brewer.pal(5, "Set2")
################

###Read CSV Data###
##################
data = read.csv("~/Desktop/new/data.csv", stringsAsFactors = FALSE)

names(data) <- substring(names(data),1,4)

######################
#Feedback
##############
drops <- c("s5..")
feedback <- data[drops]

drops <- c("s5..")
data <- data[ , !(names(data) %in% drops)]

######################
#Game Decision and
####################
myvars <- c('s1q0')
decision <- data[myvars]

##Split sequence of decisions and final score, round final score
decision$sequence <- substr(decision$s1q0, 1, 39)
decision$score <- substr(decision$s1q0, 40, length(decision$s1q0)-1)
decision$score <- round(as.numeric(decision$score), 2)

combD <-decision

drops <- c("s1q0")
combD <- combD[ , !(names(combD) %in% drops)]

combD$m1 <- substr(combD$sequence, 1, 2)
combD$d1 <- substr(combD$sequence, 3, 3)
combD$m2 <- substr(combD$sequence, 4, 5)
combD$d2 <- substr(combD$sequence, 6, 6)
combD$m3 <- substr(combD$sequence, 7, 8)
combD$d3 <- substr(combD$sequence, 9, 9)
combD$m4 <- substr(combD$sequence, 10, 11)
combD$d4<- substr(combD$sequence, 12, 12)
combD$m5 <- substr(combD$sequence, 13, 14)
combD$d5 <- substr(combD$sequence, 15, 15)
combD$m6 <- substr(combD$sequence, 16, 17)
combD$d6 <- substr(combD$sequence, 18, 18)
combD$m7 <- substr(combD$sequence, 19, 20)
combD$d7 <- substr(combD$sequence, 21, 21)
combD$m8 <- substr(combD$sequence, 22, 23)
combD$d8 <- substr(combD$sequence, 24, 24)
combD$m9 <- substr(combD$sequence, 25, 26)
combD$d9 <- substr(combD$sequence, 27, 27)
combD$m10 <- substr(combD$sequence, 28, 29)
combD$d10 <- substr(combD$sequence, 30, 30)
combD$m11 <- substr(combD$sequence, 31, 32)
combD$d11 <- substr(combD$sequence, 33, 33)
combD$m12 <- substr(combD$sequence, 34, 35)
combD$d12 <- substr(combD$sequence, 36, 36)
combD$m13<- substr(combD$sequence, 37, 38)
combD$d13<- substr(combD$sequence, 39, 39)
combW1 <- table(combD$m1, combD$d1)
combW2 <- table(combD$m2, combD$d2)
combW3 <- table(combD$m3, combD$d3)
combW4 <- table(combD$m4, combD$d4)
combW5 <- table(combD$m5, combD$d5)
combW6 <- table(combD$m6, combD$d6)
combW7 <- table(combD$m7, combD$d7)
combW8 <- table(combD$m8, combD$d8)
combW9 <- table(combD$m9, combD$d9)
combW10 <- table(combD$m10, combD$d10)
combW11 <- table(combD$m11, combD$d11)
combW12 <- table(combD$m12, combD$d12)
combW13 <- table(combD$m13, combD$d13)

combD_ <- c(combD$d1,combD$d2,combD$d3,combD$d4,combD$d5,combD$d6,combD$d7,combD$d8,combD$d9,combD$d10,combD$d11,combD$d12,combD$d13)
combM_ <- c(combD$m1,combD$m2,combD$m3,combD$m4,combD$m5,combD$m6,combD$m7,combD$m8,combD$m9,combD$m10,combD$m11,combD$m12,combD$m13)
combMD_ <- data.frame(combM_, combD_)
combMD_$combM_[combMD_$combM_ == "LA"] <- "AL"
combMD_$combM_[combMD_$combM_ == "TA"] <- "AT"
combMD_$combM_[combMD_$combM_ == "SA"] <- "AS"
combMD_$combM_[combMD_$combM_ == "TL"] <- "LT"
combMD_$combM_[combMD_$combM_ == "SL"] <- "LS"
combMD_$combM_[combMD_$combM_ == "TS"] <- "ST"
comMD_M <- table(combMD_$combM_, combMD_$combD_)
comMD_M<-comMD_M[-c(4, 7, 8, 10, 11, 12), ]

comMD_M<-t(comMD_M)
# Transform this data in %
data_percentage <- apply(comMD_M, 2, function(x){x*100/sum(x,na.rm=T)})
data_percentage <- data.frame(round(data_percentage,1))
data_percentage<-data_percentage[rev(rownames(data_percentage)), ]
data_pmatrix <- as.matrix(data_percentage)

theplot<-barplot(data_pmatrix,
                 main = "Players Decision on Measure Combinations",
                 xlab = "Measure Combinations",col = coul)
legend(x=ncol(data_pmatrix),
       y=max(colSums(data_pmatrix)),
       bty = "n",
       c("Resist","Accept"),
       fill = coul)
text(theplot, 15, labels=data_pmatrix[1,], col="black")
text(theplot, 60, labels=data_pmatrix[2,], col="black")
chisq.test(combMD_$combM_, combMD_$combD_)


##Player's Sequence of Decisions A for accept, R for Resist
decision$w1d <- substr(decision$sequence, 3, 3)
decision$w2d <- substr(decision$sequence, 6, 6)
decision$w3d <- substr(decision$sequence, 9, 9)
decision$w4d <- substr(decision$sequence, 12, 12)
decision$w5d <- substr(decision$sequence, 15, 15)
decision$w6d <- substr(decision$sequence, 18, 18)
decision$w7d <- substr(decision$sequence, 21, 21)
decision$w8d <- substr(decision$sequence, 24, 24)
decision$w9d <- substr(decision$sequence, 27, 27)
decision$w9ud <- substr(decision$sequence, 30, 30)
decision$w9xd <- substr(decision$sequence, 33, 33)
decision$w9yd <- substr(decision$sequence, 36, 36)
decision$w9zd <- substr(decision$sequence, 39, 39)

##Summ up player decisions
decision$accept <- rowSums(decision=="A")
decision$resist <- rowSums(decision=="R")

###Government Measures sequence of measures, T for testing, A for awareness, S for surveillance, L for lockdown
###the decisions of weach week is split into two separate columns for each week
decision$w1m1 <- substr(decision$sequence, 1, 1)
decision$w2m1 <- substr(decision$sequence, 4, 4)
decision$w3m1 <- substr(decision$sequence, 7, 7)
decision$w4m1 <- substr(decision$sequence, 10, 10)
decision$w5m1 <- substr(decision$sequence, 13, 13)
decision$w6m1 <- substr(decision$sequence, 16, 16)
decision$w7m1 <- substr(decision$sequence, 19, 19)
decision$w8m1 <- substr(decision$sequence, 22, 22)
decision$w9m1 <- substr(decision$sequence, 25, 25)
decision$w9um1 <- substr(decision$sequence, 28, 28)
decision$w9xm1 <- substr(decision$sequence, 31, 31)
decision$w9ym1 <- substr(decision$sequence, 34, 34)
decision$w9zm1 <- substr(decision$sequence, 37, 37)

decision$w1m2 <- substr(decision$sequence, 2, 2)
decision$w2m2 <- substr(decision$sequence, 5, 5)
decision$w3m2 <- substr(decision$sequence, 8, 8)
decision$w4m2 <- substr(decision$sequence, 11, 11)
decision$w5m2 <- substr(decision$sequence, 14, 14)
decision$w6m2 <- substr(decision$sequence, 17, 17)
decision$w7m2 <- substr(decision$sequence, 20, 20)
decision$w8m2 <- substr(decision$sequence, 23, 23)
decision$w9m2 <- substr(decision$sequence, 26, 26)
decision$w9um2 <- substr(decision$sequence, 29, 29)
decision$w9xm2 <- substr(decision$sequence, 32, 32)
decision$w9ym2 <- substr(decision$sequence, 35, 35)
decision$w9zm2 <- substr(decision$sequence, 38, 38)

##Remove the code, not needed anymore, remove sequence of players decision to sum up measures
## Because A of Accept interfers with A of Awareness
drops <- c("s1q0", "w1d","w2d","w3d","w4d","w5d","w6d","w7d","w8d","w9d","w9ud","w9xd","w9yd","w9zd")
decision <- decision[ , !(names(decision) %in% drops)]

##Sum up governments Measures
decision$surveillance <- rowSums(decision=="S")
decision$lockdown <- rowSums(decision=="L")
decision$testing <- rowSums(decision=="T")
decision$awareness <- rowSums(decision=="A")

##Bring back Player's Sequence of Decisions
decision$w1d <- substr(decision$sequence, 3, 3)
decision$w2d <- substr(decision$sequence, 6, 6)
decision$w3d <- substr(decision$sequence, 9, 9)
decision$w4d <- substr(decision$sequence, 12, 12)
decision$w5d <- substr(decision$sequence, 15, 15)
decision$w6d <- substr(decision$sequence, 18, 18)
decision$w7d <- substr(decision$sequence, 21, 21)
decision$w8d <- substr(decision$sequence, 24, 24)
decision$w9d <- substr(decision$sequence, 27, 27)
decision$w9ud <- substr(decision$sequence, 30, 30)
decision$w9xd <- substr(decision$sequence, 33, 33)
decision$w9yd <- substr(decision$sequence, 36, 36)
decision$w9zd <- substr(decision$sequence, 39, 39)

##remove sequence column, not needed anymore, create sub data sets
## sequence (has the score also), playerD (player decision), governmentM (government's measures)
drops <- c("sequence")
decision <- decision[ , !(names(decision) %in% drops)]
playerD <- decision[2:3]
governmentM <- decision[30:33]
drops <- c("accept","resist","lockdown","testing","awareness","surveillance")
sequence <- decision[ , !(names(decision) %in% drops)]


playerDS=data.frame(value=apply(playerD,2,sum))
playerDS$key=rownames(playerDS)
ggplot(data=playerDS, aes(x=key, y=value, fill=key)) +
        geom_bar(colour="black", stat="identity")

governmentMS=data.frame(value=apply(governmentM,2,sum))
governmentMS$key=rownames(governmentMS)
ggplot(data=governmentMS, aes(x=key, y=value, fill=key)) +
        geom_bar(colour="black", stat="identity")
################
# Boxplot of Decisions and Measures
###############
boxplot(playerD$accept,data=playerD, main="Player's Accept Decision",
        xlab="Decision",col = ("Red"), ylab="Number of Decisions",ylim=c(0,13))
boxplot(playerD$resist,data=playerD, main="Player's Resist Decision",
        xlab="Decision", ylab="Number of Measures",col = coul, ylim=c(0,13))
boxplot(governmentM$surveillance,data=governmentM, main="Government Surveillance Measure",
        xlab="Measure",col = ("Red"), ylab="Number of Measures", ylim=c(0,13))
boxplot(governmentM$lockdown,data=governmentM, main="Government Lockdown Measure",
        xlab="Measure", ylab="Number of Measures",col = ("Red"), ylim=c(0,13))
boxplot(governmentM$testing,data=governmentM, main="Government Testing Measure",
        xlab="Measure",col = coul, ylab="Number of Measures", ylim=c(0,13))
boxplot(governmentM$awareness,data=governmentM, main="Government Awarness Measure",
        xlab="Measure", ylab="Number of Measures",col = coul, ylim=c(0,13))
#############
# Boxplot of Scores
##############
boxplot(sequence$score,data=playerD, main="Player's Scores",
        xlab="Score",col = ("Yellow"), ylab="Number of Decisions")
summary(sequence$score)
###########
#Coorelations
###########
cor(governmentM, playerD,  method = "pearson", use = "complete.obs")

cor.test(governmentM$surveillance, playerD$accept, method = c("pearson"))
cor.test(governmentM$surveillance, playerD$resist, method = c("pearson"))
cor.test(governmentM$lockdown, playerD$accept, method = c("pearson"))
cor.test(governmentM$lockdown, playerD$resist, method = c("pearson"))
cor.test(governmentM$testing, playerD$accept, method = c("pearson"))
cor.test(governmentM$testing, playerD$resist, method = c("pearson"))
cor.test(governmentM$awareness, playerD$accept, method = c("pearson"))
cor.test(governmentM$awareness, playerD$resist, method = c("pearson"))

dandm <- cbind(governmentM, playerD)
##########################
#shapiro.test(sequence$score)
##Shapiro test, consider removing it
#####################
shapiro.test(dandm$resist)
qqnorm(dandm$resist)
ggqqplot(dandm$resist, ylab = "Resist")

shapiro.test(dandm$accept)
qqnorm(dandm$accept)
ggqqplot(dandm$accept, ylab = "Accept")

shapiro.test(dandm$surveillance)
qqnorm(dandm$surveillance)
ggqqplot(dandm$surveillance, ylab = "Surveillance")

shapiro.test(dandm$lockdown)
qqnorm(dandm$lockdown)
ggqqplot(dandm$lockdown, ylab = "Lockdown")

shapiro.test(dandm$testing)
qqnorm(dandm$testing)
ggqqplot(dandm$testing, ylab = "Testing")

shapiro.test(dandm$awareness)
qqnorm(dandm$awareness)
ggqqplot(dandm$awareness, ylab = "Awareness")

ggscatter(dandm, x = "surveillance", y = "resist",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Number of Surveillance", ylab = "Number of Resist")
ggscatter(dandm, x = "lockdown", y = "resist",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Number of Lockdown", ylab = "Number of Resist")
ggscatter(dandm, x = "testing", y = "resist",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Number of Testing", ylab = "Number of Resist")
ggscatter(dandm, x = "awareness", y = "resist",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Number of Awareness", ylab = "Number of Resist")

######################
#Age Category
########
myvars <- c('s1q1')
age <- data[myvars]
age$s1q1[age$s1q1 == "20-27"] <- "Less than 28"
age$s1q1[age$s1q1 == "28-35"] <- "More than 27"
age$s1q1[age$s1q1 == "Less than 20"] <- "Less than 28"
age$s1q1[age$s1q1 == "36-45"] <- "More than 27"
age_count <- table(age)

par(mar = rep(2, 4))
theplot<-barplot(age_count,
                 main="Players Age Categories",
                 xlab="Age Range",
                 ylab="Count",
                 ylim=c(0,30),
                 col = coul)
text(theplot, age_count-5 , paste(round(age_count/sum(age_count)*100,1), "%") ,cex=1)
##########
#Countries
##########
myvars <- c('Coun')
countries <- data[myvars]

countries$Coun[countries$Coun=="Palestine "] <-"Levant and Egypt"
countries$Coun[countries$Coun=='"Israel" '] <-"Levant and Egypt"
countries$Coun[countries$Coun=="United States"] <-"Europe and NA"
countries$Coun[countries$Coun=="Jordan "] <-"Levant and Egypt"
countries$Coun[countries$Coun=="Egypt "] <-"Levant and Egypt"
countries$Coun[countries$Coun=="United Kingdom"] <-"Europe and NA"
countries$Coun[countries$Coun=="Occupied Ireland"] <-"Europe and NA"
countries$Coun[countries$Coun=="United Arab Emirates"] <-"Asia"
countries$Coun[countries$Coun=="Falastin"] <-"Levant and Egypt"
countries$Coun[countries$Coun=="Egypt"] <-"Levant and Egypt"
countries$Coun[countries$Coun=="Lebanon"] <-"Levant and Egypt"
countries$Coun[countries$Coun=="Jordan"] <-"Levant and Egypt"
countries$Coun[countries$Coun=="Palestine"] <-"Levant and Egypt"
countries$Coun[countries$Coun=="Belgium"] <-"Europe and NA"
countries$Coun[countries$Coun=="France"] <-"Europe and NA"
countries$Coun[countries$Coun=="Germany"] <-"Europe and NA"
countries$Coun[countries$Coun=="Italy "] <-"Europe and NA"
countries$Coun[countries$Coun=="Malta"] <-"Europe and NA"
countries$Coun[countries$Coun=="Sweden "] <-"Europe and NA"
countries$Coun[countries$Coun=="UK"] <-"Europe and NA"
countries$Coun[countries$Coun=="Canada"] <-"Europe and NA"
countries$Coun[countries$Coun=="Spain"] <-"Europe and NA"
countries$Coun[countries$Coun=="USA"] <-"Europe and NA"
countries$Coun[countries$Coun=="UAE"] <-"Asia"
countries$Coun[countries$Coun=="Singapore"] <-"Asia"
countries$Coun[countries$Coun=="India"] <-"Asia"

countries_count <- table(countries)

par(mar = rep(2, 4))
theplot<-barplot(countries_count,
                 main="Players Countries",
                 xlab="Age Range",
                 ylab="Count",
                 ylim=c(0,30),
                 col = coul)
text(theplot, countries_count-2 , paste(round(countries_count/sum(countries_count)*100,1), "%") ,cex=1)


table(countries$Coun, sequence$score)
#######IMPORTANT: calculate average score of regions#####
test_table <- cbind(countries$Coun, as.numeric(sequence$score))
chisq.test(table(countries$Coun, sequence$score))

######################
#Resoponse
#############
myvars <- c('s1q2')
response <- data[myvars]
response$s1q2[response$s1q2=="Bad"] <-"5_Bad"
response$s1q2[response$s1q2=="Less than average"] <-"4_Less than average"
response$s1q2[response$s1q2=="Average"] <-"3_Average"
response$s1q2[response$s1q2=="Good"] <-"2_Good"
response$s1q2[response$s1q2=="Excellent"] <-"1_Excellent"
response_count<-response[(response$s1q2!=""), ]

response_count<- table(response_count)
par(mar = rep(2, 4))
theplot<-barplot(response_count,
                 main="How is the COVID-19 response in your country?",
                 xlab="Response",
                 ylab="Count",
                 ylim=c(0,30),
                 col = coul)
text(theplot, response_count-2 , paste(round(response_count/sum(response_count)*100,1), "%") ,cex=1)
table(countries$Coun, response$s1q2)
chisq.test(table(countries$Coun, response$s1q2))
######################
#Situation
################
myvars <- c('s1q3')
situation <- data[myvars]
situation$s1q3[situation$s1q3=="Very Bad"] <-"5_Very Bad"
situation$s1q3[situation$s1q3=="Bad"] <-"4_Bad"
situation$s1q3[situation$s1q3=="Not Bad"] <-"3_Not Bad"
situation$s1q3[situation$s1q3=="Manageable"] <-"2_Manageable"
situation$s1q3[situation$s1q3=="Under full control"] <-"1_Under full control"
situation_count<- table(situation)

par(mar = rep(2, 4))
theplot<-barplot(situation_count,
                 main="How is the situation in your country?",
                 xlab="Situation",
                 ylab="Count",
                 ylim=c(0,30),
                 col = coul)
text(theplot, situation_count-1 , paste(round(situation_count/sum(situation_count)*100,1), "%") ,cex=1)
table(countries$Coun, situation$s1q3)
chisq.test(table(countries$Coun, situation$s1q3))
######################
#Blame
##############
myvars <- c('s1q4')
blame <- data[myvars]
blame_count<- table(blame)
par(mar = rep(2, 4))
theplot<-barplot(blame_count,
                 main="Who do you blame for the crisis?",
                 xlab="Blame",
                 ylab="Count",
                 ylim=c(0,30),
                 col = coul)
text(theplot, blame_count-2 , paste(round(blame_count/sum(blame_count)*100,1), "%") ,cex=1)
table(countries$Coun, blame$s1q4)
chisq.test(table(countries$Coun, blame$s1q4))
###################
#Game Story (Playability) Experience
#############################
vectors <- c('s2q1','s2q3','s4q3','s4q4')

experienceP <- data[vectors]
experienceP$s2q3[experienceP$s2q3==4] <-'zero'
experienceP$s2q3[experienceP$s2q3==3] <-'one'
experienceP$s2q3[experienceP$s2q3==1] <-3
experienceP$s2q3[experienceP$s2q3==0] <-4
experienceP$s2q3[experienceP$s2q3=='zero'] <-0
experienceP$s2q3[experienceP$s2q3=='one'] <-1

experienceP$s4q4[experienceP$s4q4==4] <-'zero'
experienceP$s4q4[experienceP$s4q4==3] <-'one'
experienceP$s4q4[experienceP$s4q4==1] <-3
experienceP$s4q4[experienceP$s4q4==0] <-4
experienceP$s4q4[experienceP$s4q4=='zero'] <-0
experienceP$s4q4[experienceP$s4q4=='one'] <-1
experienceP$s2q3<-as.numeric(experienceP$s2q3)
experienceP$s4q4 <-as.numeric(experienceP$s4q4)
experienceP$Average <- round(ave(experienceP$s2q1, experienceP$s2q3, experienceP$s4q3, experienceP$s4q4),2)

boxplot(experienceP[,1:5], data=experienceP, main="Playability Experience",
        xlab="Game Story Experience", col = coul, ylab="Experience Score", ylim=c(0,4))
summary(experienceP)

vectors <- c('s2q1','s2q3', 's4q3','s4q4')
experienceAlpha <- experienceP[vectors]
psych::alpha(experienceAlpha)$total$std.alpha

vectors <- c('s2q1','s2q3', 's4q4')
experienceAlpha <- experienceP[vectors]
psych::alpha(experienceAlpha)$total$std.alpha


###################
#Game Mechanics (Learnability)
###############################
vectors <- c('s2q2','s2q6')
learnability <- data[vectors]
learnability$Average <- round(ave(learnability$s2q2, learnability$s2q6),2)

boxplot(learnability[,1:3], data=learnability, main="Game Mechanics (Learnability)",
        xlab="Learnability", col = coul, ylab="Learnability Score", ylim=c(0,4))
summary(learnability)

learnabilityAlpha <- learnability[vectors]
psych::alpha(learnabilityAlpha)$total$std.alpha

###################
#Game Play (Difficulty)
########################
vectors <- c('s2q7','s2q8')

difficulty <- data[vectors]
difficulty$s2q8[difficulty$s2q8==4] <-'zero'
difficulty$s2q8[difficulty$s2q8==3] <-'one'
difficulty$s2q8[difficulty$s2q8==1] <-3
difficulty$s2q8[difficulty$s2q8==0] <-4
difficulty$s2q8[difficulty$s2q8=='zero'] <-0
difficulty$s2q8[difficulty$s2q8=='one'] <-1
difficulty$s2q8 <- as.numeric(difficulty$s2q8)
difficulty$Average <- round(ave(difficulty$s2q7, difficulty$s2q8),2)

boxplot(difficulty[,1:3], data=difficulty, main="Game play (Difficulty)",
        xlab="Game Play", col = coul, ylab="Difficulty Score", ylim=c(0,4))
summary(difficulty)


###################
#Seriousness Experience
#######################
vectors <- c('s2q4','s2q5','s3q1','s3q4', 's3q6', 's4q1', 's4q2')

experienceS <- data[vectors]
experienceS$s2q4[experienceS$s2q4==4] <-'zero'
experienceS$s2q4[experienceS$s2q4==3] <-'one'
experienceS$s2q4[experienceS$s2q4==1] <-3
experienceS$s2q4[experienceS$s2q4==0] <-4
experienceS$s2q4[experienceS$s2q4=='zero'] <-0
experienceS$s2q4[experienceS$s2q4=='one'] <-1
experienceS$s2q4 <- as.numeric(experienceS$s2q4)
experienceS$s2q5[experienceS$s2q5==4] <-'zero'
experienceS$s2q5[experienceS$s2q5==3] <-'one'
experienceS$s2q5[experienceS$s2q5==1] <-3
experienceS$s2q5[experienceS$s2q5==0] <-4
experienceS$s2q5[experienceS$s2q5=='zero'] <-0
experienceS$s2q5[experienceS$s2q5=='one'] <-1
experienceS$s2q5 <- as.numeric(experienceS$s2q5)
experienceS$s4q2[experienceS$s4q2==4] <-'zero'
experienceS$s4q2[experienceS$s4q2==3] <-'one'
experienceS$s4q2[experienceS$s4q2==1] <-3
experienceS$s4q2[experienceS$s4q2==0] <-4
experienceS$s4q2[experienceS$s4q2=='zero'] <-0
experienceS$s4q2[experienceS$s4q2=='one'] <-1
experienceS$s4q2 <- as.numeric(experienceS$s4q2)
experienceS$Average <- round(ave(experienceS$s2q4, experienceS$s2q5, experienceS$s3q1, experienceS$s3q4, experienceS$s3q6, experienceS$s4q1,experienceS$s4q2),2)

boxplot(experienceS[,1:8], data=experienceS, main="Seriousness Experience",
        xlab="Seriousness Experience", col = coul, ylab="Experience Score", ylim=c(0,4))
summary(experienceS)
vectors <- c('s2q4','s2q5')
experienceSAlpha <- experienceS[vectors]
psych::alpha(experienceSAlpha)$total$std.alpha
###################
#Efficiency
##########
vectors <- c('s3q2','s3q3', 's3q5','s4q5', 's4q6')
efficiency <- data[vectors]
efficiency$Average <- round(ave(efficiency$s3q2, efficiency$s3q3, efficiency$s3q5, efficiency$s4q5, efficiency$s4q6),2)

boxplot(efficiency[,1:6], data=efficiency, main="Message Efficiency",
        xlab="Efficiency", col = coul, ylab="Efficiency Score", ylim=c(0,4))
summary(efficiency)
##############
###Relate intersting variances to:
########
##Age group
#############
AgeGroup <- cbind(age, experienceP$Average, experienceS$Average, learnability$Average, efficiency$Average, difficulty$Average)
boxplot(experienceP$Average~s1q1, AgeGroup, main = "Playability experience per age group",
        xlab = "Age", ylim = c(0, 4), col = coul )

boxplot(experienceS$Average~s1q1, AgeGroup, main = "Seriousness experience per age group",
        xlab = "Age", ylim = c(0, 4), col = coul )

boxplot(difficulty$Average~s1q1, AgeGroup, main = "Difficulty per age group",
        xlab = "Age", ylim = c(0, 4), col = coul )

boxplot(efficiency$Average~s1q1, AgeGroup, main = "Efficiency per age group",
        xlab = "Age", ylim = c(0, 4), col = coul )

boxplot(learnability$Average~s1q1, AgeGroup, main = "Learnability per age group",
        xlab = "Age", ylim = c(0, 4), col = coul )

##########
##Blame
###########
BlameGroup <- cbind(blame, experienceP$Average, experienceS$Average, learnability$Average, efficiency$Average, difficulty$Average)
boxplot(experienceP$Average~s1q4, BlameGroup, main = "Playability experience per Blame",
        xlab = "Blame", ylim = c(0, 4), col = coul )

boxplot(experienceS$Average~s1q4, BlameGroup, main = "Seriousness experience per Blame",
        xlab = "Blame", ylim = c(0, 4), col = coul )

boxplot(difficulty$Average~s1q4, BlameGroup, main = "Difficulty per Blame",
        xlab = "Blame", ylim = c(0, 4), col = coul )

boxplot(efficiency$Average~s1q4, BlameGroup, main = "Efficiency per Blame",
        xlab = "Blame", ylim = c(0, 4), col = coul )

boxplot(learnability$Average~s1q4, BlameGroup, main = "Learnability per Blame",
        xlab = "Blame", ylim = c(0, 4), col = coul )
#############
##Response
##############
ResponseGroup <- cbind(response, experienceP$Average, experienceS$Average, learnability$Average, efficiency$Average, difficulty$Average)
boxplot(experienceP$Average~s1q2, ResponseGroup, main = "Playability experience per Country Response",
        xlab = "Response", ylim = c(0, 4), col = coul )

boxplot(experienceS$Average~s1q2, ResponseGroup, main = "Seriousness experience per Country Response",
        xlab = "Response", ylim = c(0, 4), col = coul )

boxplot(difficulty$Average~s1q2, ResponseGroup, main = "Difficulty per Country Response",
        xlab = "Response", ylim = c(0, 4), col = coul )

boxplot(efficiency$Average~s1q2, ResponseGroup, main = "Efficiency per Country Response",
        xlab = "Response", ylim = c(0, 4), col = coul )

boxplot(learnability$Average~s1q2, ResponseGroup, main = "Learnability per Country Response",
        xlab = "Response", ylim = c(0, 4), col = coul )

#############
##Situation
###############
SituationGroup <- cbind(situation, experienceP$Average, experienceS$Average, learnability$Average, efficiency$Average, difficulty$Average)
boxplot(experienceP$Average~s1q3, SituationGroup, main = "Playability experience per Country Situation",
        xlab = "Situation", ylim = c(0, 4), col = coul )

boxplot(experienceS$Average~s1q3, SituationGroup, main = "Seriousness experience per Country Situation",
        xlab = "Situation", ylim = c(0, 4), col = coul )

boxplot(difficulty$Average~s1q3, SituationGroup, main = "Difficulty per Country Situation",
        xlab = "Situation", ylim = c(0, 4), col = coul )

boxplot(efficiency$Average~s1q3, SituationGroup, main = "Efficiency per Country Situation",
        xlab = "Situation", ylim = c(0, 4), col = coul )

boxplot(learnability$Average~s1q3, SituationGroup, main = "Learnability per Country Situation",
        xlab = "Situation", ylim = c(0, 4), col = coul )

###############
##Country
###########
CountryGroup <- cbind(countries, experienceP$Average, experienceS$Average, learnability$Average, efficiency$Average, difficulty$Average)
boxplot(experienceP$Average~Coun, CountryGroup, main = "Playability experience per Region",
        xlab = "Region", ylim = c(0, 4), col = coul )

boxplot(experienceS$Average~Coun, CountryGroup, main = "Seriousness experience per Region",
        xlab = "Region", ylim = c(0, 4), col = coul )

boxplot(difficulty$Average~Coun, CountryGroup, main = "Difficulty per Region",
        xlab = "Region", ylim = c(0, 4), col = coul )

boxplot(efficiency$Average~Coun, CountryGroup, main = "Efficiency per Region",
        xlab = "Region", ylim = c(0, 4), col = coul )

boxplot(learnability$Average~Coun, CountryGroup, main = "Learnability per Region",
        xlab = "Region", ylim = c(0, 4), col = coul )

################
##Player Decision
##################
playerD$accept[playerD$accept>6] <-"Between 7 and 13"
playerD$accept[playerD$accept<7] <-"Between 0 and 6"
playerD$resist[playerD$resist>6] <-"Between 7 and 13"
playerD$resist[playerD$resist<7] <-"Between 0 and 6"
AcceptGroup <- cbind(playerD, experienceP$Average, experienceS$Average, learnability$Average, efficiency$Average, difficulty$Average)
boxplot(experienceP$Average~accept, AcceptGroup, main = "Playability experience per Accept",
        xlab = "Accept rate", ylim = c(0, 4), col = coul )

boxplot(experienceS$Average~accept, AcceptGroup, main = "Seriousness experience per Accept",
        xlab = "Accept rate", ylim = c(0, 4), col = coul )

boxplot(difficulty$Average~accept, AcceptGroup, main = "Difficulty per Accept",
        xlab = "Accept rate", ylim = c(0, 4), col = coul )

boxplot(efficiency$Average~accept, AcceptGroup, main = "Efficiency per Accept",
        xlab = "Accept rate", ylim = c(0, 4), col = coul )

boxplot(learnability$Average~accept, AcceptGroup, main = "Learnability per Accept",
        xlab = "Accept rate", ylim = c(0, 4), col = coul )

boxplot(experienceP$Average~resist, AcceptGroup, main = "Playability experience per Resist",
        xlab = "Resist rate", ylim = c(0, 4), col = coul )

boxplot(experienceS$Average~resist, AcceptGroup, main = "Seriousness experience per Resist",
        xlab = "Resist rate", ylim = c(0, 4), col = coul )

boxplot(difficulty$Average~resist, AcceptGroup, main = "Difficulty per Resist",
        xlab = "Resist rate", ylim = c(0, 4), col = coul )

boxplot(efficiency$Average~resist, AcceptGroup, main = "Efficiency per Resist",
        xlab = "Resist rate", ylim = c(0, 4), col = coul )

boxplot(learnability$Average~resist, AcceptGroup, main = "Learnability per Resist",
        xlab = "Resist rate", ylim = c(0, 4), col = coul )
##############
##Government Measure
##############
governmentM$surveillance[governmentM$surveillance>6] <-"Between 7 and 13"
governmentM$surveillance[governmentM$surveillance<7] <-"Between 0 and 6"
governmentM$lockdown[governmentM$lockdown>6] <-"Between 7 and 13"
governmentM$lockdown[governmentM$lockdown<7] <-"Between 0 and 6"
governmentM$testing[governmentM$testing>6] <-"Between 7 and 13"
governmentM$testing[governmentM$testing<7] <-"Between 0 and 6"
governmentM$awareness[governmentM$awareness>6] <-"Between 7 and 13"
governmentM$awareness[governmentM$awareness<7] <-"Between 0 and 6"
MeasureGroup <- cbind(governmentM, experienceP$Average, experienceS$Average, learnability$Average, efficiency$Average, difficulty$Average)
boxplot(experienceP$Average~surveillance, MeasureGroup, main = "Playability experience per Surveillance",
        xlab = "Surveillance rate", ylim = c(0, 4), col = coul )

boxplot(experienceS$Average~surveillance, MeasureGroup, main = "Seriousness experience per Surveillance",
        xlab = "Surveillance rate", ylim = c(0, 4), col = coul )

boxplot(difficulty$Average~surveillance, MeasureGroup, main = "Difficulty per Surveillance",
        xlab = "Surveillance rate", ylim = c(0, 4), col = coul )

boxplot(efficiency$Average~surveillance, MeasureGroup, main = "Efficiency per Surveillance",
        xlab = "Surveillance rate", ylim = c(0, 4), col = coul )

boxplot(learnability$Average~surveillance, MeasureGroup, main = "Learnability per Surveillance",
        xlab = "Surveillance rate", ylim = c(0, 4), col = coul )

boxplot(experienceP$Average~lockdown, MeasureGroup, main = "Playability experience per Lockdown",
        xlab = "Lockdown rate", ylim = c(0, 4), col = coul )

boxplot(experienceS$Average~lockdown, MeasureGroup, main = "Seriousness experience per Lockdown",
        xlab = "Lockdown rate", ylim = c(0, 4), col = coul )

boxplot(difficulty$Average~lockdown, MeasureGroup, main = "Difficulty per Lockdown",
        xlab = "Lockdown rate", ylim = c(0, 4), col = coul )

boxplot(efficiency$Average~lockdown, MeasureGroup, main = "Efficiency per Lockdown",
        xlab = "Lockdown rate", ylim = c(0, 4), col = coul )

boxplot(learnability$Average~lockdown, MeasureGroup, main = "Learnability per Lockdown",
        xlab = "Lockdown rate", ylim = c(0, 4), col = coul )
boxplot(experienceP$Average~testing, MeasureGroup, main = "Playability experience per Testing",
        xlab = "Testing rate", ylim = c(0, 4), col = coul )

boxplot(experienceS$Average~testing, MeasureGroup, main = "Seriousness experience per Testing",
        xlab = "Testing rate", ylim = c(0, 4), col = coul )

boxplot(difficulty$Average~testing, MeasureGroup, main = "Difficulty per Testing",
        xlab = "Testing rate", ylim = c(0, 4), col = coul )

boxplot(efficiency$Average~testing, MeasureGroup, main = "Efficiency per Testing",
        xlab = "Testing rate", ylim = c(0, 4), col = coul )

boxplot(learnability$Average~testing, MeasureGroup, main = "Learnability per Testing",
        xlab = "Testing rate", ylim = c(0, 4), col = coul )

boxplot(experienceP$Average~awareness, MeasureGroup, main = "Playability experience per Awareness",
        xlab = "Awareness rate", ylim = c(0, 4), col = coul )

boxplot(experienceS$Average~awareness, MeasureGroup, main = "Seriousness experience per Awareness",
        xlab = "Awareness rate", ylim = c(0, 4), col = coul )

boxplot(difficulty$Average~awareness, MeasureGroup, main = "Difficulty per Awareness",
        xlab = "Awareness rate", ylim = c(0, 4), col = coul )

boxplot(efficiency$Average~awareness, MeasureGroup, main = "Efficiency per Awareness",
        xlab = "Awareness rate", ylim = c(0, 4), col = coul )

boxplot(learnability$Average~awareness, MeasureGroup, main = "Learnability per Awareness",
        xlab = "Awareness rate", ylim = c(0, 4), col = coul )
############
##Score
##########
score <- sequence['score']
score[score<40] <-"Bad (<40)"
score[(score>=40 & score<70) & score!="Bad (<40)"] <-"Okay (40<=x<70)"
score[score>=70 & score!="Bad (<40)" & score != "Okay (40<=x<70)"] <-"Excellent (>70)"

ScoreGroup <- cbind(score, experienceP$Average, experienceS$Average, learnability$Average, efficiency$Average, difficulty$Average)
ScoreG_count<- table(ScoreGroup$score)
par(mar = rep(2, 4))
theplot<-barplot(ScoreG_count,
                 main="Score Groups",
                 xlab="Game Result",
                 ylab="Count",
                 ylim=c(0,50),
                 col = coul)
text(theplot, ScoreG_count-5 , paste(round(ScoreG_count/sum(ScoreG_count)*100,1), "%") ,cex=1)
boxplot(experienceP$Average~score, ScoreGroup, main = "Playability experience per Score",
        xlab = "Score rate", ylim = c(0, 4), col = coul )

boxplot(experienceS$Average~score, ScoreGroup, main = "Seriousness experience per Score",
        xlab = "Score rate", ylim = c(0, 4), col = coul )

boxplot(difficulty$Average~score, ScoreGroup, main = "Difficulty per Score",
        xlab = "Score rate", ylim = c(0, 4), col = coul )

boxplot(efficiency$Average~score, ScoreGroup, main = "Efficiency per Score",
        xlab = "Score rate", ylim = c(0, 4), col = coul )

boxplot(learnability$Average~score, ScoreGroup, main = "Learnability per Score",
        xlab = "Score rate", ylim = c(0, 4), col = coul )

##########
##Reload playerD and governmentM for coorelations ##
#############################
drops <- c("sequence")
decision <- decision[ , !(names(decision) %in% drops)]
playerD <- decision[2:3]
governmentM <- decision[30:33]

cor(learnability, playerD,  method = "pearson", use = "complete.obs")
cor(efficiency, playerD,  method = "pearson", use = "complete.obs")
cor(difficulty, playerD,  method = "pearson", use = "complete.obs")
cor(experienceP, playerD,  method = "pearson", use = "complete.obs")
cor(experienceS, playerD,  method = "pearson", use = "complete.obs")

cor(learnability, governmentM,  method = "pearson", use = "complete.obs")
cor(efficiency, governmentM,  method = "pearson", use = "complete.obs")
cor(difficulty, governmentM,  method = "pearson", use = "complete.obs")
cor(experienceP, governmentM,  method = "pearson", use = "complete.obs")
cor(experienceS, governmentM,  method = "pearson", use = "complete.obs")

cor.test(learnability$s2q2, playerD$accept, method = c("pearson"))
##############
####Nortmality test
####################
qqnorm(learnability$Average)
qqnorm(learnability$s2q2)
qqnorm(learnability$s2q6)

qqnorm(efficiency$Average)
qqnorm(efficiency$s3q2)
qqnorm(efficiency$s3q3)
qqnorm(efficiency$s3q5)
qqnorm(efficiency$s4q5)
qqnorm(efficiency$s4q6)

qqnorm(difficulty$Average)
qqnorm(difficulty$s2q7)
qqnorm(difficulty$s2q8)

qqnorm(experienceP$Average)
qqnorm(experienceP$s2q1)
qqnorm(experienceP$s2q3)
qqnorm(experienceP$s4q3)
qqnorm(experienceP$s4q4)

qqnorm(experienceS$Average)
qqnorm(experienceS$s2q4)
qqnorm(experienceS$s2q5)
qqnorm(experienceS$s3q1)
qqnorm(experienceS$s3q4)
qqnorm(experienceS$s3q6)
qqnorm(experienceS$s4q1)
qqnorm(experienceS$s4q2)

shapiro.test(learnability$Average)
shapiro.test(learnability$s2q2)
shapiro.test(learnability$s2q6)

shapiro.test(efficiency$Average)
shapiro.test(efficiency$s3q2)
shapiro.test(efficiency$s3q3)
shapiro.test(efficiency$s3q5)
shapiro.test(efficiency$s4q5)
shapiro.test(efficiency$s4q6)

shapiro.test(difficulty$Average)
shapiro.test(difficulty$s2q7)
shapiro.test(difficulty$s2q8)

shapiro.test(experienceP$Average)
shapiro.test(experienceP$s2q1)
shapiro.test(experienceP$s2q3)
shapiro.test(experienceP$s4q3)
shapiro.test(experienceP$s4q4)

shapiro.test(experienceS$Average)
shapiro.test(experienceS$s2q4)
shapiro.test(experienceS$s2q5)
shapiro.test(experienceS$s3q1)
shapiro.test(experienceS$s3q4)
shapiro.test(experienceS$s3q6)
shapiro.test(experienceS$s4q1)
shapiro.test(experienceS$s4q2)

##################
##Creat Chisquare p.value matrix###
########################
s2q2A = chisq.test(table(learnability$s2q2,playerD$accept))
s2q6A = chisq.test(table(learnability$s2q6,playerD$accept))

s3q2A = chisq.test(table(efficiency$s3q2,playerD$accept))
s3q3A = chisq.test(table(efficiency$s3q3,playerD$accept))
s3q5A = chisq.test(table(efficiency$s3q5,playerD$accept))
s4q5A = chisq.test(table(efficiency$s4q5,playerD$accept))
s4q6A = chisq.test(table(efficiency$s4q6,playerD$accept))

s2q7A = chisq.test(table(difficulty$s2q7,playerD$accept))
s2q8A = chisq.test(table(difficulty$s2q8,playerD$accept))

s2q1A = chisq.test(table(experienceP$s2q1,playerD$accept))
s2q3A = chisq.test(table(experienceP$s2q3,playerD$accept))
s4q3A = chisq.test(table(experienceP$s4q3,playerD$accept))
s4q4A = chisq.test(table(experienceP$s4q4,playerD$accept))

s2q4A = chisq.test(table(experienceS$s2q4,playerD$accept))
s2q5A = chisq.test(table(experienceS$s2q5,playerD$accept))
s3q1A = chisq.test(table(experienceS$s3q1,playerD$accept))
s3q4A = chisq.test(table(experienceS$s3q4,playerD$accept))
s3q6A = chisq.test(table(experienceS$s3q6,playerD$accept))
s4q1A = chisq.test(table(experienceS$s4q1,playerD$accept))
s4q2A = chisq.test(table(experienceS$s4q2,playerD$accept))

Accept_<- c(s2q2A$p.value, s2q6A$p.value, s3q2A$p.value,
            s3q3A$p.value, s3q5A$p.value, s4q5A$p.value,
            s4q6A$p.value, s2q7A$p.value, s2q8A$p.value,
            s2q1A$p.value, s2q3A$p.value, s4q3A$p.value,
            s4q4A$p.value, s2q4A$p.value, s2q5A$p.value,
            s3q1A$p.value, s3q4A$p.value, s3q6A$p.value,
            s4q1A$p.value, s4q2A$p.value)

s2q2Age = chisq.test(table(learnability$s2q2,age$s1q1))
s2q6Age = chisq.test(table(learnability$s2q6,age$s1q1))

s3q2Age = chisq.test(table(efficiency$s3q2,age$s1q1))
s3q3Age = chisq.test(table(efficiency$s3q3,age$s1q1))
s3q5Age = chisq.test(table(efficiency$s3q5,age$s1q1))
s4q5Age = chisq.test(table(efficiency$s4q5,age$s1q1))
s4q6Age = chisq.test(table(efficiency$s4q6,age$s1q1))

s2q7Age = chisq.test(table(difficulty$s2q7,age$s1q1))
s2q8Age = chisq.test(table(difficulty$s2q8,age$s1q1))

s2q1Age = chisq.test(table(experienceP$s2q1,age$s1q1))
s2q3Age = chisq.test(table(experienceP$s2q3,age$s1q1))
s4q3Age = chisq.test(table(experienceP$s4q3,age$s1q1))
s4q4Age = chisq.test(table(experienceP$s4q4,age$s1q1))

s2q4Age = chisq.test(table(experienceS$s2q4,age$s1q1))
s2q5Age = chisq.test(table(experienceS$s2q5,age$s1q1))
s3q1Age = chisq.test(table(experienceS$s3q1,age$s1q1))
s3q4Age = chisq.test(table(experienceS$s3q4,age$s1q1))
s3q6Age = chisq.test(table(experienceS$s3q6,age$s1q1))
s4q1Age = chisq.test(table(experienceS$s4q1,age$s1q1))
s4q2Age = chisq.test(table(experienceS$s4q2,age$s1q1))

Age_<- c(s2q2Age$p.value, s2q6Age$p.value, s3q2Age$p.value,
         s3q3Age$p.value, s3q5Age$p.value, s4q5Age$p.value,
         s4q6Age$p.value, s2q7Age$p.value, s2q8Age$p.value,
         s2q1Age$p.value, s2q3Age$p.value, s4q3Age$p.value,
         s4q4Age$p.value, s2q4Age$p.value, s2q5Age$p.value,
         s3q1Age$p.value, s3q4Age$p.value, s3q6Age$p.value,
         s4q1Age$p.value, s4q2Age$p.value)

s2q2Res = chisq.test(table(learnability$s2q2,response$s1q2))
s2q6Res = chisq.test(table(learnability$s2q6,response$s1q2))

s3q2Res = chisq.test(table(efficiency$s3q2,response$s1q2))
s3q3Res = chisq.test(table(efficiency$s3q3,response$s1q2))
s3q5Res = chisq.test(table(efficiency$s3q5,response$s1q2))
s4q5Res = chisq.test(table(efficiency$s4q5,response$s1q2))
s4q6Res = chisq.test(table(efficiency$s4q6,response$s1q2))

s2q7Res = chisq.test(table(difficulty$s2q7,response$s1q2))
s2q8Res = chisq.test(table(difficulty$s2q8,response$s1q2))

s2q1Res = chisq.test(table(experienceP$s2q1,response$s1q2))
s2q3Res = chisq.test(table(experienceP$s2q3,response$s1q2))
s4q3Res = chisq.test(table(experienceP$s4q3,response$s1q2))
s4q4Res = chisq.test(table(experienceP$s4q4,response$s1q2))

s2q4Res = chisq.test(table(experienceS$s2q4,response$s1q2))
s2q5Res = chisq.test(table(experienceS$s2q5,response$s1q2))
s3q1Res = chisq.test(table(experienceS$s3q1,response$s1q2))
s3q4Res = chisq.test(table(experienceS$s3q4,response$s1q2))
s3q6Res = chisq.test(table(experienceS$s3q6,response$s1q2))
s4q1Res = chisq.test(table(experienceS$s4q1,response$s1q2))
s4q2Res = chisq.test(table(experienceS$s4q2,response$s1q2))

Res_<- c(s2q2Res$p.value, s2q6Res$p.value, s3q2Res$p.value,
         s3q3Res$p.value, s3q5Res$p.value, s4q5Res$p.value,
         s4q6Res$p.value, s2q7Res$p.value, s2q8Res$p.value,
         s2q1Res$p.value, s2q3Res$p.value, s4q3Res$p.value,
         s4q4Res$p.value, s2q4Res$p.value, s2q5Res$p.value,
         s3q1Res$p.value, s3q4Res$p.value, s3q6Res$p.value,
         s4q1Res$p.value, s4q2Res$p.value)

s2q2Sit = chisq.test(table(learnability$s2q2,situation$s1q3))
s2q6Sit = chisq.test(table(learnability$s2q6,situation$s1q3))

s3q2Sit = chisq.test(table(efficiency$s3q2,situation$s1q3))
s3q3Sit = chisq.test(table(efficiency$s3q3,situation$s1q3))
s3q5Sit = chisq.test(table(efficiency$s3q5,situation$s1q3))
s4q5Sit = chisq.test(table(efficiency$s4q5,situation$s1q3))
s4q6Sit = chisq.test(table(efficiency$s4q6,situation$s1q3))

s2q7Sit = chisq.test(table(difficulty$s2q7,situation$s1q3))
s2q8Sit = chisq.test(table(difficulty$s2q8,situation$s1q3))

s2q1Sit = chisq.test(table(experienceP$s2q1,situation$s1q3))
s2q3Sit = chisq.test(table(experienceP$s2q3,situation$s1q3))
s4q3Sit = chisq.test(table(experienceP$s4q3,situation$s1q3))
s4q4Sit = chisq.test(table(experienceP$s4q4,situation$s1q3))

s2q4Sit = chisq.test(table(experienceS$s2q4,situation$s1q3))
s2q5Sit = chisq.test(table(experienceS$s2q5,situation$s1q3))
s3q1Sit = chisq.test(table(experienceS$s3q1,situation$s1q3))
s3q4Sit = chisq.test(table(experienceS$s3q4,situation$s1q3))
s3q6Sit = chisq.test(table(experienceS$s3q6,situation$s1q3))
s4q1Sit = chisq.test(table(experienceS$s4q1,situation$s1q3))
s4q2Sit = chisq.test(table(experienceS$s4q2,situation$s1q3))

Sit_<- c(s2q2Sit$p.value, s2q6Sit$p.value, s3q2Sit$p.value,
         s3q3Sit$p.value, s3q5Sit$p.value, s4q5Sit$p.value,
         s4q6Sit$p.value, s2q7Sit$p.value, s2q8Sit$p.value,
         s2q1Sit$p.value, s2q3Sit$p.value, s4q3Sit$p.value,
         s4q4Sit$p.value, s2q4Sit$p.value, s2q5Sit$p.value,
         s3q1Sit$p.value, s3q4Sit$p.value, s3q6Sit$p.value,
         s4q1Sit$p.value, s4q2Sit$p.value)


s2q2Blame = chisq.test(table(learnability$s2q2,blame$s1q4))
s2q6Blame = chisq.test(table(learnability$s2q6,blame$s1q4))

s3q2Blame = chisq.test(table(efficiency$s3q2,blame$s1q4))
s3q3Blame = chisq.test(table(efficiency$s3q3,blame$s1q4))
s3q5Blame = chisq.test(table(efficiency$s3q5,blame$s1q4))
s4q5Blame = chisq.test(table(efficiency$s4q5,blame$s1q4))
s4q6Blame = chisq.test(table(efficiency$s4q6,blame$s1q4))

s2q7Blame = chisq.test(table(difficulty$s2q7,blame$s1q4))
s2q8Blame = chisq.test(table(difficulty$s2q8,blame$s1q4))

s2q1Blame = chisq.test(table(experienceP$s2q1,blame$s1q4))
s2q3Blame = chisq.test(table(experienceP$s2q3,blame$s1q4))
s4q3Blame = chisq.test(table(experienceP$s4q3,blame$s1q4))
s4q4Blame = chisq.test(table(experienceP$s4q4,blame$s1q4))

s2q4Blame = chisq.test(table(experienceS$s2q4,blame$s1q4))
s2q5Blame = chisq.test(table(experienceS$s2q5,blame$s1q4))
s3q1Blame = chisq.test(table(experienceS$s3q1,blame$s1q4))
s3q4Blame = chisq.test(table(experienceS$s3q4,blame$s1q4))
s3q6Blame = chisq.test(table(experienceS$s3q6,blame$s1q4))
s4q1Blame = chisq.test(table(experienceS$s4q1,blame$s1q4))
s4q2Blame = chisq.test(table(experienceS$s4q2,blame$s1q4))

Blame_<- c(s2q2Blame$p.value, s2q6Blame$p.value, s3q2Blame$p.value,
           s3q3Blame$p.value, s3q5Blame$p.value, s4q5Blame$p.value,
           s4q6Blame$p.value, s2q7Blame$p.value, s2q8Blame$p.value,
           s2q1Blame$p.value, s2q3Blame$p.value, s4q3Blame$p.value,
           s4q4Blame$p.value, s2q4Blame$p.value, s2q5Blame$p.value,
           s3q1Blame$p.value, s3q4Blame$p.value, s3q6Blame$p.value,
           s4q1Blame$p.value, s4q2Blame$p.value)

s2q2Country = chisq.test(table(learnability$s2q2,countries$Coun))
s2q6Country = chisq.test(table(learnability$s2q6,countries$Coun))

s3q2Country = chisq.test(table(efficiency$s3q2,countries$Coun))
s3q3Country = chisq.test(table(efficiency$s3q3,countries$Coun))
s3q5Country = chisq.test(table(efficiency$s3q5,countries$Coun))
s4q5Country = chisq.test(table(efficiency$s4q5,countries$Coun))
s4q6Country = chisq.test(table(efficiency$s4q6,countries$Coun))

s2q7Country = chisq.test(table(difficulty$s2q7,countries$Coun))
s2q8Country = chisq.test(table(difficulty$s2q8,countries$Coun))

s2q1Country = chisq.test(table(experienceP$s2q1,countries$Coun))
s2q3Country = chisq.test(table(experienceP$s2q3,countries$Coun))
s4q3Country = chisq.test(table(experienceP$s4q3,countries$Coun))
s4q4Country = chisq.test(table(experienceP$s4q4,countries$Coun))

s2q4Country = chisq.test(table(experienceS$s2q4,countries$Coun))
s2q5Country = chisq.test(table(experienceS$s2q5,countries$Coun))
s3q1Country = chisq.test(table(experienceS$s3q1,countries$Coun))
s3q4Country = chisq.test(table(experienceS$s3q4,countries$Coun))
s3q6Country = chisq.test(table(experienceS$s3q6,countries$Coun))
s4q1Country = chisq.test(table(experienceS$s4q1,countries$Coun))
s4q2Country = chisq.test(table(experienceS$s4q2,countries$Coun))

Country_<- c(s2q2Country$p.value, s2q6Country$p.value, s3q2Country$p.value,
             s3q3Country$p.value, s3q5Country$p.value, s4q5Country$p.value,
             s4q6Country$p.value, s2q7Country$p.value, s2q8Country$p.value,
             s2q1Country$p.value, s2q3Country$p.value, s4q3Country$p.value,
             s4q4Country$p.value, s2q4Country$p.value, s2q5Country$p.value,
             s3q1Country$p.value, s3q4Country$p.value, s3q6Country$p.value,
             s4q1Country$p.value, s4q2Country$p.value)

chiTestP <- data.frame(Country_, Blame_, Sit_, Res_, Age_, Accept_)

rownames(chiTestP) <- c("s2q2", "s2q6", "s3q2", "s3q3", "s3q5", "s4q5",
                        "s4q6", "s2q7", "s2q8", "s2q1", "s2q3", "s4q3",
                        "s4q4", "s2q4", "s2q5", "s3q1", "s3q4", "s3q6",
                        "s4q1", "s4q2")
chiTestP