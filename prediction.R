#load data
load("approved.RData")
load("rejected.RData")

#select variables(amount,score,dti,employment)
rej <- rejected[,c("amount","score","dti","employment")]
app <- approved[,c("amount","score","dti","employment")]

#remove NA and score==0
rej <- na.omit(rej)
rej <- subset(rej, score!=0)
app <- na.omit(app)
app <- subset(app, score!=0)

#add lables (approved:1, rejected:0)
app <- data.frame(app, label=1)
rej <- data.frame(rej, label=0)

#create training set (800000 approved and 800000 rejected) and testing set (40000 approved and 40000 rejected)
t <- sample(842557, 800000)
apptrain <- app[t,]
apptest <- app[-t,][sample(42557,40000),]

t <- sample(3503417, 800000)
rejtrain <- rej[t,]
rejtest <- rej[-t,][sample(2000000,40000),]

training <- rbind(apptrain, rejtrain)
testing <- rbind(apptest, rejtest)

#standarize data (0-1 scaling)
maxvalue <- c(max(training$amount),max(training$score),max(training$dti),max(training$employment))
minvalue <- c(min(training$amount),min(training$score),min(training$dti),min(training$employment))

training <- transform(training, amount=(amount-minvalue[1])/(maxvalue[1]-minvalue[1]),
                                score=(score-minvalue[2])/(maxvalue[2]-minvalue[2]),
                                dti=(dti-minvalue[3])/(maxvalue[3]-minvalue[3]),
                                employment=(employment-minvalue[4])/(maxvalue[4]-minvalue[4]))
testing <- transform(testing, amount=(amount-minvalue[1])/(maxvalue[1]-minvalue[1]),
                                score=(score-minvalue[2])/(maxvalue[2]-minvalue[2]),
                                dti=(dti-minvalue[3])/(maxvalue[3]-minvalue[3]),
                                employment=(employment-minvalue[4])/(maxvalue[4]-minvalue[4]))

#knn classifer
library(class)
result <- knn(training[,1:4],testing[,1:4],training[,5],k=10)
comp <- table(result, testing[,5])
print(comp)
accuracy <- (comp[1,1]+comp[2,2])/nrow(testing)
print(accuracy)
