install.packages("tree")
install.packages("ISLR2")
library("tree")

library("ISLR2")
attach(Carseats)
High <- factor ( ifelse (Sales <= 8, "No", "Yes") )
# create col that shows if our sales r high/low
length(High)
Carseats <- data.frame(Carseats, High) # Merge our dataframes together

tree.carseats <- tree(High ~. -Sales, Carseats)

summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty = 0)

tree.carseats

set.seed(2)
train <- sample(1:nrow(Carseats), 200)
Carseats.test <- Carseats[-train,]
High.test <- High[-train]
tree.carseats <- tree(High ~ . - Sales, Carseats, subset = train)
tree.pred <- predict(tree.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)

set.seed(7)
cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass )
names(cv.carseats)
cv.carseats

par(mfrow = c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")

prune.carseats <- prune.misclass(tree.carseats, best = 9)
plot(prune.carseats)
text(prune.carseats, pretty = 0)

tree.pred <- predict(prune.carseats, Carseats.test, 
                     type = class)
table(tree.pred, High.test)

# Regression Trees Example
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston <- tree(medv ~ ., Boston, subset = train)
summary(tree.boston)
train 
summary(Boston$medv)
plot(tree.boston)
text(tree.boston, pretty = 0)
#check if pruning will improve performance
cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = 'b')

# Pruning Trees
prune.boston <- prune.tree(tree.boston, best = 5)
# Reduces it by 2 splits 
plot(prune.boston)
text(prune.boston, pretty = 0) 
# Use our old tree to make our predictions 
yhat <- predict(tree.boston, newdata = Boston[-train,])
boston.test <- Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0, 1)
mean( (yhat - boston.test)^2 )
# Mean Square Error w/ regression tree = 35.29 
# so sq root of mse = 5.9 so wthn $5.9K homes are accurate by

# Principal Component Analysis

states <- row.names(USArrests)
states
names(USArrests)
apply(USArrests, 2, mean) # Obtain the mean on our 
apply(USArrests, 2, var) # Obtain our variances, can see we need to standardise

pr.out <- prcomp(USArrests, scale = TRUE) #perform our principle comp analysis
# scale = True will auto normalise our variables for this analysis to N(0, 1)
names(pr.out)
#Return our mean and st dev before scaling
pr.out$center
pr.out$scale 
pr.out$rotation # Returns our ϕ1, ϕ2, ... 
dim(pr.out$x) # x is our xij's X has dimensions 50 x 4
biplot(pr.out, scale = 0)
#Flip so that it looks the same as our figure
pr.out$rotation = - pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out, scale = 0) #PCA only differs from a sign flip
pr.out$sdev
pr.var <- pr.out$sdev^2
pr.var
pve <- pr.var/sum(pr.var) #calculate our explained variance
pve
par(mfrow = c(1, 2) )
plot(pve, xlab = 'Princ Comp', ylab = 'Prop of Var Expla', ylim = c(0, 1), type = "b")
plot(cumsum(pve), xlab = 'Princ Comp', ylab = 'Cummla Prop of Var Expla', ylim = c(0, 1), type = 'b')
#Scree plots for optimal Princ Comps (Freq vs Commlative)

#Matrix Completion (Correct Missing Data)
X <- data.matrix(scale(USArrests)) # Scale our data s.t it's N(0,1)
pcob <- prcomp(X) #Perform PCA on X
summary(pcob)
sX <- svd(X) # returns 3 comp u, d, v w/ v = loading matrix of Princ Comp 
names(sX)
round(sX$v, 3) # 4x4 Matrix of our Princ comps & predictors
pcob$rotation # same thing but with pcob func
t(sX$d * t(sX$u)) # this is the same as pcob$x trnsp(d*trnsp(u))
pcob$x
#now we have the function of svd defined we want to omit 20 random values

nomit <- 20
set.seed(15)
ina <- sample(seq(50), nomit)
inb <- sample(1:4, nomit, replace = TRUE)
# Randomly choose 20 values indexes i and then j
Xna <- X
index.na <- cbind(ina, inb) #bind these indexes together 
Xna[index.na] <- NA # find them and replace the act val w/ NA

fit.svd <- function(X, M = 1) {
  svdob <- svd(X)
  with(svdob, 
       u[, 1:M, drop = FALSE] %*% (d[1:M] * t(v[,1:M, drop = FALSE] ) )
       )
} # calculates our sX, with() is just better notation for [] conditions
# mat mult subset sze M of d * v^t
#alt form: svdob$u[, 1:M, drop = FALSE] %*% (svdob$d[1:M]*t(svdob$v[, 1:M, drop = FALSE]))

Xhat <- Xna
xbar <- colMeans(Xna, na.rm = TRUE)
Xhat[index.na] <- xbar[inb]
# Create a Xhat with the means as our "NA Fillers" for comparison
thresh <- 1e-7
rel_err <- 1
iter <- 0
ismiss <- is.na(Xna) #new matrix w. Xna dims but True when vals missing 
mssold <- mean((scale(Xna, xbar, FALSE)[!ismiss])^2)  #store mse of non missing vals 
mss0 <- mean(Xna[!ismiss]^2) # meansq of non missing vals

while(rel_err > thresh){
  iter <- iter + 1
  Xapp <- fit.svd(Xhat, M = 1) #comp our Princ Comp
  Xhat[ismiss] <- Xapp[ismiss] #replace the mean xbars with our new princ comp
  mss <- mean(((Xna - Xapp)[!ismiss])^2)
  rel_err <- (mssold - mss)/mss0 # old mse - new mse/ variance
  mssold <- mss
  cat('Iter:', iter, 'MSS:', mss, 'Rel. Err:', rel_err, '\n')
}
cor(Xapp[ismiss], X[ismiss]) #Correlation 

# Kmeans Clustering 

set.seed(2)
x <- matrix(rnorm(50*2), ncol = 2)
x[1:25, 1] <- x[1:25, 1] + 3 
x[1:25, 2] <- x[1:25, 2] - 4
km.out <- kmeans(x, 2, nstart = 20)
km.out$cluster

par(mfrow = c(1, 2))
plot(x, col = (km.out$cluster + 1), main = 'K-means Cluster results w/ k = 2', xlab = '', ylab = '')

set.seed(4)
km.out <- kmeans(x, 3, nstart = 20)
km.out
plot(x, col = (km.out$cluster + 1), main = 'K-means Cluster results w/ k = 3', xlab = '', ylab = '') 

set.seed(4)
km.out <- kmeans(x, 3, nstart = 1)
km.out$tot.withinss
km.out <- kmeans(x, 3, nstart = 20 )
km.out$tot.withinss
# total w/in clust sum of squares - min
# start with nstart = 20-50 or our local optim will found

hc.complete <- hclust(dist(x), method = 'complete')
hc.average <- hclust(dist(x), method = 'average')
hc.single <- hclust(dist(x), method = 'single')
par(mfrow = c(1, 3))
plot(hc.complete, main = " Complete Linkage ", xlab = "", sub = "", cex = .9)
plot(hc.average , main = " Average Linkage ", xlab = "", sub = "", cex = .9)
plot(hc.single, main = " Single Linkage ", xlab = "", sub = "", cex = .9)

cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)

cutree(hc.single, 4)

xsc <- scale(x)
plot(hclust( dist (xsc), method = "complete") ,
      main = " Hierarchical Clustering with Scaled Features ")

x <-matrix ( rnorm (30 * 3), ncol = 3)
dd <- as.dist(1 - cor (t(x)))
plot ( hclust(dd, method = "complete") ,
         main = " Complete Linkage with Correlation - Based Distance ",
         xlab = "", sub = "")
cluster <- cutree(hclust(dd, method = "complete"), 3)
plot(x, col = (cluster + 1) ) 
# this is really bad whne they aren't correlated will try with all 3 and see whats the best 


# NCI60 Data Example - 
library(ISLR2)
nci.labs <- NCI60$labs
nci.data <- NCI60$data
dim(nci.data)

nci.labs[1:4]
table(nci.labs)
pr.out <- prcomp(nci.data, scale = TRUE)
Cols <- function (vec) {
  cols <- rainbow ( length ( unique (vec)))
  return (cols[as.numeric (as.factor (vec))])
 }

par(mfrow = c(1, 2))
plot (pr.out$x[, 1:2], col = Cols (nci.labs), pch = 19,
      xlab = "Z1", ylab = "Z2")
plot (pr.out$x[, c(1, 3)], col = Cols (nci.labs), pch = 19,
        xlab = "Z1", ylab = "Z3")
summary(pr.out)
par(mfrow = c(1, 2))
plot(pr.out)
pve <- 100*pr.out$sdev^2/sum(pr.out$sdev^2)
plot (pve , type = "o", ylab = "PVE ",
      xlab = " Principal Component ", col = " blue ")
plot ( cumsum (pve), type = "o", ylab = " Cumulative PVE ",
         xlab = " Principal Component ", col = " brown3 ")

sd.data <- scale(nci.data)

par (mfrow = c(1, 3))
data.dist <- dist(sd.data)
plot ( hclust (data.dist), xlab = "", sub = "", ylab = "",
         labels = nci.labs , main = " Complete Linkage ")
plot ( hclust (data.dist , method = "average") ,
         labels = nci.labs , main = " Average Linkage ",
         xlab = "", sub = "", ylab = "")
plot ( hclust (data.dist , method = "single") ,
         labels = nci.labs , main = " Single Linkage ",
         xlab = "", sub = "", ylab = "")
 
hc.out <- hclust(dist(sd.data)) #complete
hc.clusters <- cutree(hc.out, 4)
table(hc.clusters, nci.labs)

par(mfrow = c(1, 1))
plot(hc.out, labels = nci.labs)
abline(h = 139, col = 'red')

hc.out
set.seed(2)
km.out <-kmeans(sd.data, 4, nstart = 20)
km.clusters <- km.out$cluster
table(km.clusters, hc.clusters)

hc.out <- hclust(dist(pr.out$x[, 1:5]))
plot(hc.out, labels = nci.labs, main = 'Hier clust 1st 5 score vecs')
table(cutree(hc.out, 4), nci.labs)

library (gbm)
set.seed (1)
boost.boston <- gbm(medv ∼ ., data = Boston[train , ],
                       distribution = "gaussian", n.trees = 5000,
                       interaction.depth = 4)

boost.credit =gbm((Price)~.,data=data_train,distribution="gaussian", n.trees =1000, interaction.depth =2)


