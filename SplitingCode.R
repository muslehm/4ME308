###Read CSV Data###
##################
data = read.csv("~/Desktop/new/data.csv", stringsAsFactors = FALSE)
names(data) <- substring(names(data),1,4)

######################
#Game Decision and 
myvars <- c('s1q0')
decision <- data[myvars]

##Split sequence of decisions and final score, round final score
decision$sequence <- substr(decision$s1q0, 1, 39)
decision$score <- substr(decision$s1q0, 40, length(decision$s1q0)-1)
decision$score <- round(as.numeric(decision$score), 2)

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