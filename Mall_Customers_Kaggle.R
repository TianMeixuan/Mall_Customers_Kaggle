
library(ggplot2)
library(cluster)
library(dplyr)
library(funModeling)
library(MASS)
library(mclust)
library(tidyr)
library(arulesViz)
###########Data Exploration##############
#Reading data
mall = read.csv("Mall_Customers.csv", header = T)
names(mall) <- c("CustomerID","Gender","Age","Annual_Income", "Spending_Score")
dat = mall
head(dat)
#Checking the null values 
colSums(is.na(dat))

# Data Visualization
# Distribution of age
summary(dat$Age)
Age_dis=ggplot(dat, aes(x=Age)) + 
  geom_histogram(aes(y=..density..),  
                 binwidth=5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") + 
  geom_vline(aes(xintercept=mean(Age, na.rm=T)),   
             color="red", linetype="dashed", size=1) +
  scale_x_continuous(breaks = round(seq(min(dat$Age), max(dat$Age), by = 5),1))
Age_dis
# Distribution of Income
summary(dat$Annual_Income)
Income_dis=ggplot(dat, aes(x=Annual_Income)) + 
  geom_histogram(aes(y=..density..), 
                 binwidth=5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  geom_vline(aes(xintercept=mean(Annual_Income, na.rm=T)),   
             color="red", linetype="dashed", size=1) +
  scale_x_continuous(breaks = round(seq(min(dat$Annual_Income), max(dat$Annual_Income), by = 10),1))
Income_dis
# Distribution of Spend
summary(dat$Spending_Score)
Spend_dis=ggplot(dat, aes(x=Spending_Score)) + 
  geom_histogram(aes(y=..density..),  
                 binwidth=5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  geom_vline(aes(xintercept=mean(Spending_Score, na.rm=T)),   
             color="red", linetype="dashed", size=1) +
  scale_x_continuous(breaks = round(seq(min(dat$Spending_Score), max(dat$Spending_Score), by = 5),1))
Spend_dis
# Distribution of gender
summary(dat$Gender)
freq(dat)

#Pairplot for the data
pairs(dat[-2], lower.panel = NULL)
#The Above Graph for Showing the correlation between the different attributes of the Mall Customer Segementation Dataset.We can clearly see that these attributes do not have good correlation among them, that's why we will proceed with all of the features.

# Distribution of values in Age , Annual Income and Spending Score according to Gender
p1 = ggplot(dat, aes(x=Gender, y=Age, fill=Gender)) + 
  geom_boxplot() + 
  stat_summary(fun=mean, geom="point", shape=5, size=4) + coord_flip()

p2 = ggplot(dat, aes(x=Gender, y=Annual_Income, fill=Gender)) + 
  geom_boxplot() + 
  stat_summary(fun=mean, geom="point", shape=5, size=4) + coord_flip()
#There are more number of males who get paid more than females. But, The number of males and females are equal in number when it comes to low annual income.
p3 = ggplot(dat, aes(x=Gender, y=Spending_Score, fill=Gender)) + 
  geom_boxplot() + 
  stat_summary(fun=mean, geom="point", shape=5, size=4) + coord_flip()
#It is clearly visible that the most of the males have a Spending Score of around 25k US Dollars to 70k US Dollars whereas the Females have a spending score of around 35k US Dollars to 75k US Dollars. which again points to the fact that women are Shopping Leaders.
multiplot <- function(..., plotlist = NULL, cols = 1, layout = NULL, title = NULL, 
                      fontsize = 14, fontfamily = "Helvetica", fontface = "bold") {
  require(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (length(title)>0){
    layout <- rbind(rep(0, ncol(layout)), layout)
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), 
                                               ncol(layout), 
                                               heights = if (length(title)>0) {unit(c(0.5, rep(5,nrow(layout)-1)), "null")}
                                               else {unit(c(rep(5, nrow(layout))), "null")})))
    if(length(title) > 1){
      ncols <- 1:ncol(layout)
      for(i in seq(ncols)){
        grid.text(title[i], 
                  vp = viewport(layout.pos.row = 1, layout.pos.col = i),
                  gp = gpar(fontsize = fontsize, fontfamily = fontfamily, fontface = fontface))
      }
    } else {
      grid.text(title, 
                vp = viewport(layout.pos.row = 1, layout.pos.col = 1:ncol(layout)),
                gp = gpar(fontsize = fontsize, fontfamily = fontfamily, fontface = fontface))
    }
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(p1,p2,p3, cols = 3)


######## K-means clustering ############
#Choosing Annual Income & Spending score for clustering subject and scale the data
dat.sc <-scale(dat[,c(4,5)])

#Finding best K for K mean using Elbow Method.
wss <- function(data, maxCluster = 10) {
  # Initialize within sum of squares
  SSw <- (nrow(data) - 1) * sum(apply(data, 2, var))
  SSw <- vector()
  for (i in 2:maxCluster) {
    SSw[i] <- sum(kmeans(data, centers = i)$withinss)
  }
  plot(1:maxCluster, SSw, type = "o", xlab = "Number of Clusters", ylab = "Within groups sum of squares", pch=19)
}
set.seed(100)
wss(dat.sc)
#  use k=5 as the number of cluster
dat.KM<-kmeans(dat.sc,5)  
dat.KM
# Adding 'Cluster' column 
dat$Cluster <- dat.KM$cluster
dat
#plot the clusters
c_Clust=dat[,c(4,5)]
ggplot(c_Clust, aes(x = Annual_Income, y = Spending_Score)) + 
  geom_point(stat = "identity", aes(color = as.factor(dat.KM$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5")) +
  ggtitle("Customer Cluster")+
  xlab("Annual Income")+ylab("Spending Score")

# summarize the clusering result

dat %>% group_by(Cluster,Gender) %>% 
  summarise(med_age=median(Age),med_income = median(Annual_Income), med_spend = median(Spending_Score))

############EM clustering############
## EM Model including annual income and spending score
mod=Mclust(mall[,4:5])
summary(mod,parameters = TRUE)

## Plot model
par(mfrow = c(2, 2))
plot(mod,what="BIC",cex=0.8)
plot(mod,what="classification")
plot(mod,what="uncertainty")
plot(mod,what="density")

######### Mining data using association rules #######

## Preprocess data
## Divide Gender into 2 groups, which are Male and Female
## Divide Age into 3 groups, which are (0,30] (30,50] (50,70]
## Divide Annual Income into 4 groups, which are (less than 30) [30,60) [60,80) (more than 80)
## Divide Spending Score into 4 groups, which are (0,25] (25,50] (50,75] (more than 75)

mall$gender <- ifelse(
  mall$Gender=='Male',"gender=Male", "gender=Female"
)

mall$age <- ifelse(
  mall$Age<=30, "Age=(0,30]",
  ifelse(
    mall$Age<=50, "Age=(30,50]", "Age=(50,70]"
  )
)

mall$income <- ifelse(
  mall$`Annual_Income`<30,"Annual_Income=(less than 30)",
  ifelse(
    mall$`Annual_Income`<60,"Annual_Income=[30,60)",
    ifelse(
      mall$`Annual_Income`<80,"Annual_Income=[60,80)","Annual_Income=(more than 80)"
    )
  )
)

mall$score <- ifelse(
  mall$`Spending_Score`<=25,"Spending_Score=(0,25]",
  ifelse(
    mall$`Spending_Score`<=50,"Spending_Score=(25,50]",
    ifelse(
      mall$`Spending_Score`<=75,"Spending_Score=(50,75]","Spending_Score=(more than 75)"
    )
  )
)

## Merge four columns (gender,age,income and score)
a <- unite(mall, "mall_association",gender,age,income,score,sep = ";", remove = FALSE)
a <- a[,6]
write.table(a,"association.csv", quote = FALSE, 
            row.names = FALSE,col.names = FALSE)

## Read the data for association rules analysis
mall2 = read.transactions('association.csv',header=FALSE,
                          format = 'basket', sep=";",
                          quote="",rm.duplicates = TRUE)  
summary(mall2)

## Generate assosication rules
association.rules <- apriori(mall2, parameter = list(supp=0.01, conf=0.8,maxlen=10))
summary(association.rules)
## Look at the first five rules
inspect(association.rules[1:5])

## Get interactive plots
rules=head(association.rules, n = 10, by = "confidence")
plot(rules, method = "graph",  engine = "htmlwidget")

## Try to target very-high-spending-score individuals
high_score.association.rules=apriori(mall2, parameter = list(supp=0.01, conf=0.8),
                                     appearance = list(default="lhs",rhs="Spending_Score=(more than 75)"))
inspect(high_score.association.rules)

## Try to target very-high-income individuals
very_high_income.association.rules=apriori(mall2, parameter = list(supp=0.01, conf=0.8),
                                           appearance = list(default="lhs",rhs="Annual_Income=(more than 80)"))
inspect(very_high_income.association.rules)

## Try to target high-income individuals
high_income.association.rules=apriori(mall2, parameter = list(supp=0.01, conf=0.8),
                                      appearance = list(default="lhs",rhs="Annual_Income=[60,80)"))
inspect(high_income.association.rules)
