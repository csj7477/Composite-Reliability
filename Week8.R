f2structure <- read.csv(file="C:/R_DATA/F2structure.csv",head=TRUE)

#descriptive stastics
mydesc <- round(rbind(apply(f2structure,2,FUN=mean),
                      apply(f2structure,2,FUN=sd)),2)
mycor <- round(cor(f2structure),2)
myresult <- rbind(mycor, mydesc)
rownames(myresult)[7:8] <- c('M','SD')
myresult

#Explorative Factor Analysis
myefa <- factanal(f2structure,factors=2,rotation='varimax')
myefa

#Loading values
round(myefa$loadings[,],2)
round(apply(myefa$loadings[,]^2,2,sum),3)


library(psych)

#Cronbach Alpha
summary(alpha(f2structure[,1:3]))
summary(alpha(f2structure[,4:6]))

#Composite Reliability
(F1.SS.L2 <- sum(myefa$loadings[1:3,2])^2)
(F1.SS.error <- sum(myefa$uniquenesses[1:3]))
(CR.F1 <- F1.SS.L2/(F1.SS.L2+F1.SS.error))
(F2.SS.L2 <- sum(myefa$loadings[4:6,1])^2)
(F2.SS.error <- sum(myefa$uniquenesses[4:6]))
(CR.F2 <- F2.SS.L2/(F2.SS.L2+F2.SS.error))

#Avearege Variance Extraction
(F1.SS.Lsq <- sum(myefa$loadings[1:3,2]^2))
(F1.SS.error <- sum(myefa$uniquenesses[1:3]))
(AVE.F1 <- F1.SS.Lsq/(F1.SS.Lsq+F1.SS.error))
(F2.SS.Lsq <- sum(myefa$loadings[4:6,1]^2))
(F2.SS.error <- sum(myefa$uniquenesses[4:6]))
(AVE.F2 <- F2.SS.Lsq/(F2.SS.Lsq+F2.SS.error))
