library(ggplot2)
customer <- read.csv(file="F:/MySQL/R/DATA/DQLAB/Mall_Customers.csv")
print(customer)
names(customer)
colnames(customer) <- c("Customer_ID","Gender","Age","Annual_Income_$K","Spending_Score_1-100")
customer$Gender <- as.factor(customer$Gender)
summary(customer)
#VISUALISASI DATA
gender <- table(customer$Gender)
barplot(gender, main="Gender Comparison", xlab="Gender", ylab="count",col=rainbow(2),legend=rownames(gender))
hist(customer$Age,main="Age Class", xlab="Age Class", ylab="Frequency",col="green", labels=TRUE)
hist(customer$`Annual_Income_$K`,main="Annual Income($K)",xlab="Annual Income",ylab="Frequency",labels=TRUE,col="green")
hist(customer$`Spending_Score_1-100`,main="Spending_Score",xlab="Spending_Score",ylab="Frequency",labels=TRUE,col="green")

#Selecting Data
customer_fix <- customer[,c(4,5)]
set.seed(100)
segmentasi <- kmeans(x=customer_fix, centers=5, nstart=25)
sse <- sapply(1:10, function(param_k){kmeans(customer_fix, param_k, nstart=25)$tot.withinss})
jumlah_cluster_max <- 10
ssdata = data.frame(cluster=c(1:jumlah_cluster_max),sse)
ggplot(ssdata, aes(x=cluster,y=sse)) +
  geom_line(color="red") + geom_point() +
  ylab("Within Cluster Sum of Squares") + xlab("Jumlah Cluster") +
  geom_text(aes(label=format(round(sse, 2), nsmall = 2)),hjust=-0.2, vjust=-0.5) +
  scale_x_discrete(limits=c(1:jumlah_cluster_max))
model=kmeans(customer_fix,5)
model
segments=model$cluster
final_data=cbind(customer_fix,segments)
write.csv(final_data,"final_data.csv")
model$centers
model$withinss
model$tot.withinss