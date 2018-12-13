write.csv2(xxevents, file="data/test/events-f5.csv", row.names=FALSE)

xxdistinct <- distinct_(xxevents, "number")

toString(xxdistinct$number[1])

xxnewt <- NULL
for(i in 1:nrow(xxdistinct))
{
   n <- xxdistinct$number[i]
   set <- paste(xxevents$set_state_id[xxevents$number==n],collapse = ",")
   c <- cbind(toString(xxdistinct$number[i]),xset)
   mset <- paste(xxevents$mset_state_id[xxevents$number==n],collapse = ",")
   c <- cbind(c,mset)
   seq <- paste(xxevents$seq_state_id[xxevents$number==n],collapse = ",")
   c <- cbind(c,seq)
   xxnewt <- rbind(xxnewt,c)
}

xxnewtdt <- as.data.frame(xxnewt)

colnames(xxnewtdt) <- c("number","set","mset","seq")

xxnewtdt2 <- setDT(xxnewtdt)

counter <- xxnewtdt2[, .(`Number of rows` = .N), by = set]
write.csv2(counter, file="data/test/events-f5-counter-set.csv", row.names=FALSE)

counter <- xxnewtdt2[, .(`Number of rows` = .N), by = mset]
write.csv2(counter, file="data/test/events-f5-counter-mset.csv", row.names=FALSE)

counter <- xxnewtdt2[, .(`Number of rows` = .N), by = seq]
write.csv2(counter, file="data/test/events-f5-counter-seq.csv", row.names=FALSE)


# teste de média e desvio padrão

#av1 <- sample(10)
#av2 <- sample(10) # sample apenas reordena

av1 <- sample(5:10, 100, replace=TRUE)
av2 <- sample(1:5, 100, replace=TRUE)

am1 <- mean(av1)
am2 <- mean(av2)

avar1 <- var(av1)
avar2 <- var(av2)

asd1 <- sd(av1)
asd2 <- sd(av2)

am3 <- (mean(c(am1,am2)))
asd3 <- mean(c(asd1,asd2))

av4 <- c(av1,av2)
am4 <- mean(av4)
asd4 <- sd(av4)




asdv <- (avar1 - avar2)^2

rm(am1,am2,am3,am4,asd1,asd2,asd3,asd4,asdv,av1,av2,av4,avar1,avar2)


# ddply(av1, .fun = mean())
#
# datac <- ddply(data, groupvars, .drop=.drop,
#                .fun = function(xx, col) {
#                   c(N    = length2(xx[[col]], na.rm=na.rm),
#                     mean = ceiling(mean   (xx[[col]], na.rm=na.rm)),
#                     sd   = ceiling(sd     (xx[[col]], na.rm=na.rm)),
#                     median   = ceiling(median (xx[[col]], na.rm=na.rm)),
#                     min   = ceiling(min     (xx[[col]], na.rm=na.rm)),
#                     max   = ceiling(max     (xx[[col]], na.rm=na.rm))
#                   )
#                },
#                measurevar




x <- sample(1:10, 100, replace=TRUE)
y <- sample(1:10, 100, replace=TRUE)

xerr <- abs(x-y)

xmape <- MAPE(y,x)

xm <- NULL
for(i in 1:length(x)) {
   xm[i] <- MAPE(y[i],x[i])
}

mean(xm)

mean(y)-mean(x)/mean(x)

Accuracy(y,x) # Accuracy <- mean(y_true == y_pred)

o erro é relativo: não espero acertar exatamente nos segundos


xt1 <- as.POSIXct(strptime("01/12/2018 22:10:15", "%d/%m/%Y %H:%M:%S"))
xt2 <- as.POSIXct(strptime("01/12/2018 22:11:15", "%d/%m/%Y %H:%M:%S"))
xi1 <- as.integer(xt1)
xi2 <- as.integer(xt2)
xi2 - xi1
