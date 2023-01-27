Migdata.full <- read.csv(file = "/Users/kelvin/Desktop/Maths/Final Year Project/Migration - Bulgaria&Czech.csv")  # subset of our predictors 
Migdata.full <- read.csv(file = "/Users/kelvin/Desktop/Maths/Final Year Project/Migration - Full Unsup Learning.csv")
# Migdata <- Migdata[!(Migdata$Country.of.asylum %in% c('Germany')),]
# the "Big Movers" from Bulgaria/Czechia They are outliers for this 
# Germany due to it being easy to migrate into & USA/Canada due to distance

Migdata <- Migdata.full[!(Migdata.full$Year %in% c('2000', '2021', '2022', '2001')),]
#Filtering out 2000 & 2001 data (due to 9/11 system may have changed) & 2021-2022 due to covid

# Attempt to remove some years due to difference in systems - need to validate 
#Migdata[(Migdata$Country.of.origin == "Bulgaria") && (Migdata$Year == '2009') && (Migdata$Country.of.asylum == 'Finland')]
#Migdata <- Migdata[!(((Migdata$Country.of.origin == "Bulgaria") && (Migdata$Year == 2009) && (Migdata$Country.of.asylum == 'Finland')) || ((Migdata$Country.of.origin == "Bulgaria") && (Migdata$Year == '2002') && (Migdata$Country.of.asylum == 'Sweden')))]
 
# Linear Model w/ stepwise regression 
Migdata <- Migdata[Migdata$Country.of.origin %in% c('Bulgaria', 'Czechia'),]
Migdata <- Filter(function(x)(length(unique(x))>1), Migdata) # Removes single valued factors for our columns 
dim(Migdata)
n <- length(Migdata[,1])
Train_ind <- sample(1:n, 230)
Migdata_Test <- Migdata[Train_ind,]
(Migdata_Train <- Migdata[-Train_ind,])
dim(Migdata_Test)
names(Migdata_Test)
empmdl <- lm(formula = Total.Per.100k ~ 1, data = Migdata_Test)
flmdl <- lm(Total.Per.100k ~ ., data = Migdata_Test)
step(flmdl, direction = "backward")
step(empmdl, direction = "forward", scope = list(lower = empmdl, upper = flmdl))
fwdmdl <- lm(formula = Total.Per.100k ~ Total + Decisions....O + Life.Expectancy..Year...O + 
               Population..A + Asylumseekers + Gov.Education.Budget....of...A + 
               GDP.LCU..T..Difference + Birth.Rate..Crude..Per.1k..Difference + 
               Country.of.origin + X..Rural.Population..O + X..Crop.Production.to..1416..Difference + 
               X..Male..Pop...A + Homicides..p100k...Diff + X..1.90.a.day.per.100k..A + 
               Maternal.Mortality.per.100k..Difference, data = Migdata_Test)
# Removed something that is directly dependent on our outcome variables 
summary(fwdmdl) #why is this so highhhh, overfitting? 
Migdata_Test$X..Crop.Production.to..1416..A
lomodel <- lm(formula = Total.Per.100k ~ Country.of.asylum + Life.Expectancy..Year...O + 
                GDP.LCU..T..Difference + Life.Expectancy..Year...A + X..Male..Pop...Difference + 
                Gov.Education.Budget....of...A + Population..A  , data = Migdata_Test)
summary(lomodel)
model <- lm(formula = Total.Per.100k ~ Country.of.asylum + Life.Expectancy..Year...O + 
     GDP.LCU..T..Difference + Life.Expectancy..Year...A + X..Male..Pop...Difference + 
       Gov.Education.Budget....of...A + Population..A + X..Crop.Production.to..1416..A,
   data = Migdata_Test)
summary(model)
# Residual Analysis of our linear model
res <- residuals(model)
plot(residuals(model))
plot(fitted(model), residuals(model))
qqnorm(res)
qqline(res)
plot(density(res))
# Look at > 24 & > 1.5 & get our outliers in a table 
outliers.lin <- which(abs(residuals(model)) > 24)
lin.outliers.point <- Migdata_Test[outliers.lin, c('Country.of.asylum', 'Year', 'Total.Per.100k', 'Country.of.origin') ] 
lin.outliers.point <- data.frame(lin.outliers.point, residuals(model)[outliers])
lin.outliers.point

# Basic Regression Trees

#install.packages("tree")
#install.packages("ISLR2")
library("tree")
#logging population cause it's kinda big/hard to visualise w/o 
names(Migdata)
Migdata <- data.frame(Migdata[1:62], Migdata[67], stringsAsFactors=FALSE)
# Remove my other "Potential Predictors" Decided to use Total.Per.100k 
train <- sample(1:nrow(Migdata), nrow(Migdata)/2)
asy.tree <- tree(Total.Per.100k~ log(Population..A) + ., Migdata, subset = train)
summary(Migdata$Total.Per.100k)
summary(asy.tree)
plot(asy.tree)
text(asy.tree, pretty = 0)

# Check if  our tree is worth pruning
cv.asy <- cv.tree(asy.tree)
plot(cv.asy$size, cv.asy$dev, type = 'b')
# our dev is our deviance & size is the no. of branches
# looks like it's worth pruning, past 5-7ish our deviance doesn't go down much
prune.asy <- prune.tree(asy.tree, best = 6)
plot(prune.asy)
text(prune.asy, pretty = 0)
summary(prune.asy)
# pruned table using 5 branches instead of 10
yhat <- predict(asy.tree, newdata = Migdata[-train,])
Migdata.test <- Migdata[-train, "Total.Per.100k"]
plot(yhat, Migdata.test)
abline(0,1)
mse <- mean((yhat - Migdata.test)^2)
mse^0.5
# i.e our mse is 12.8287 so sq rt of our mse is 3.58 so our predictions
# on average is within 3.58 per 100k of the true median value of asylums per 100k 

# this space is for Bagging/RandForrest 

#little bit of residual analysis (Should be using residual tools)
tree.resid <- resid(asy.tree)
plot(tree.resid, fitted(asy.tree))
abline(h = c(4, 8, -4, -8))
# Check which country/year is outlying
outliers.tre <- which(abs(tree.resid) > 8)
#outliers.tre <- which(abs(tree.resid) > 4 & abs(tree.resid) < 8)
# Return our countries
outliers.point <- Migdata[outliers.tre, c('Country.of.asylum', 'Year', 'Total.Per.100k', 'Country.of.origin') ] 
outliers.point <- data.frame(outliers.point, tree.resid[outliers.tre])
outliers.point
outliers.point[!(outliers.point$Country.of.asylum == "Germany"),]
# Clearly I'm missing something for Germany 2002-2005 Canada 2007-09 is worth exploring too
# Asylum Country Politics
#     A lot of middle range residuals result in the 2002-2005/6 migration on Germany, due to former germans moving back
# "Reunification of Germany" was still occuring therefore our "system" was different
#     2006 United States immigration reform protests - Gave Immigrants better working conditions etc. (Sensenbrenner Bill)
# Employers treating Illegal immigrants poorly would no longer be given "amnesty" employer rules would be in place etc. 
#     Migration to Canada - Czech Visa requirements were lifted by joining EU in 2004 reintroduced in 09 for 2-3 years
#             Add variable for Visa-less travel between countries? 
#             Look into - Czech/Bulgarian Population in the world (Dispora of Czech & Bulgarians)
plot(tree.resid[which(Migdata$Country.of.asylum == 'United States of America')])
plot(tree.resid[which(Migdata$Country.of.asylum == 'Canada')])
plot(tree.resid[which(Migdata$Country.of.asylum == 'Germany')])
# All Germany with my deviations (& 1 Canada), 
# need a variable that will help me map it



# Unsupervised Learning - We are taking a step back 
# How did I manage to cluster Czech & Bulgaria etc. why are they similar etc.
# Instead of using judgment will use a unsupervised ML model to find similarities of European Asylum countries instead

# Aim: Data set of migration from country to country and the respective countries economic/social factors in that year using Unsupervised 
# Learning can I group them in a way that they can be modelled through tree learning?

MgrDt <- read.csv(file = "/Users/kelvin/Desktop/Maths/Final Year Project/Migration - Full Unsup Learning.csv")
EuroDt <- MgrDt[MgrDt$Continent.of.Origin == 'Europe',] # Fix to looking at European Asylums moving out 
EuroDt <- EuroDt[!EuroDt$Country.of.origin == 'Unknown',] #Remove asylums of unkown country as we have entire voids we cannot fill 
# (I guess we could take world averages but i'm simplifying for this example)
EuroDt <- Filter(function(x)(length(unique(x))>1), EuroDt) # Removes single valued factors for our columns No use for us as it won't mean much & take space 
names(EuroDt) # I have around 81 pred variables going. Filter doesn't remove any but that's fine will keep in case I get some more filters/more data
dim(EuroDt)
head(EuroDt) 
EuroDt$Country.of.origin[EuroDt$Country.of.origin..ISO. == ''] = 'UKN' # change all the blank unknown iso's to UKN
EuroDt$Country.of.asylum[EuroDt$Country.of.asylum..ISO == ''] = 'UKN'
# Need a format like Example Name: <number> | <number> | <number> | <number> etc. 
# Get the numeric columns 
Orgn.to.Asy.Iso<- paste(EuroDt$Year, EuroDt$Country.of.origin..ISO., 'to' , EuroDt$Country.of.asylum..ISO) #Get our abr asylum moves
length(Orgn.to.Asy.Iso)
length(unique(Orgn.to.Asy.Iso)) # this needs to be same as the length 
Country_Origin <- EuroDt$Country.of.origin
length(Country_Origin) #needs to be 11126
EuroDt.Char <- Filter(function(x)(typeof(x) == 'character'), EuroDt) # keep the ones w/ characters stored
EuroDt <- Filter(function(x)(typeof(x) != 'character'), EuroDt) # Filter out any columns which use characters i.e non-numerical characters
row.names(EuroDt) <- Orgn.to.Asy.Iso
dim(EuroDt) # Reduced my columns to 74, mainly the info cols (for myself)
head(EuroDt)
# data prep complete 
dim(EuroDt)
# using PCA to fill in our missing data 
Scl.EuroDt <- scale(EuroDt, center = TRUE, scale = TRUE) # idk how to make this look good
Scl.EuroDt
# Missing values now, we can start with the missing val analysis then 
fit.svd <- function(X, M = 1) {
  svdob <- svd(X)
  with(svdob, 
       u[, 1:M, drop = FALSE] %*% (d[1:M] * t(v[,1:M, drop = FALSE] ) )
  )
}# calculates our sX, with() is just better notation for [] conditions
# mat mult subset sze M of d * v^t
dt.nas <- Scl.EuroDt # our data with "holes"
index.na <- is.na(dt.nas) # find them and rec their location 
inb <- which(index.na == TRUE, arr.ind= TRUE)[,2] # which column they're in
ina <- which(index.na == TRUE, arr.ind= TRUE)[,1] # which row they're in
Xhat <- dt.nas # what will be our new data 
xbar <- colMeans(dt.nas, na.rm = TRUE) # calc the mean of our columns
Xhat[index.na] <- xbar[inb] # fill the empty vals with col averages
sum(is.na(Xhat)) # null check

# Create a Xhat with the means as our "NA Fillers" for comparison
thresh <- 1e-7 # create our error margin (difference in old mse vs new mse/variance)
rel_err <- 1 # initial 
iter <- 0
ismiss <- is.na(dt.nas) # find them and rec their location 
mssold <- mean((scale(dt.nas, xbar, FALSE)[!ismiss])^2)  #store mse of non missing vals 
mss0 <- mean(dt.nas[!ismiss]^2) # meansq of non missing vals

while(rel_err > thresh){
  iter <- iter + 1
  Xapp <- fit.svd(Xhat, M = 1) #comp our Princ Comp
  Xhat[ismiss] <- Xapp[ismiss] #replace the mean xbars with our new princ comp
  mss <- mean(((dt.nas - Xapp)[!ismiss])^2)
  rel_err <- (mssold - mss)/mss0 # old mse - new mse/ variance
  mssold <- mss
  cat('Iter:', iter, 'MSS:', mss, 'Rel. Err:', rel_err, '\n')
}
Xhat

# PCA on my data
# summarised version of my data 
EuroDt.no_na <- t(apply(Xhat, 1, 
        function(r) r * attr(Xhat, 'scaled:scale') + 
          attr(Xhat, 'scaled:center')))
# now I need to unscale this as it doesn't make sense (for now)
# I have 95k rows, this will be impossible to visualise properly, kinda need around 50ish?
# Need: all my separate countries & their average predictors,& sum of total displacement
# Make it a dataframe again
df.Eurodt <- data.frame(EuroDt.Char, EuroDt.no_na, row.names = Orgn.to.Asy.Iso, stringsAsFactors=FALSE)
# Country | Predictors... (Average over years) | Total migration
dim(df.Eurodt)
Ctry.Dt <- data.frame(df.Eurodt[2], df.Eurodt[7:25], df.Eurodt[74], row.names = NULL, stringsAsFactors=FALSE)
col.nam <- c(names(Ctry.Dt[2:20]), 'Avg.Pop.p100k' )
euro.cntry <- unique(df.Eurodt$Country.of.origin..ISO.) # split our countries 
n <- length(unique(df.Eurodt$Country.of.origin..ISO.))
v <- data.frame(matrix(NA, ncol = 20))
colnames(v) <- col.nam
for (i in 1:n){
  Country <- euro.cntry[i]
  cnrty.in <- Ctry.Dt[Ctry.Dt$Country.of.origin..ISO. == Country,]
  # data[row, col]
  Avg.Pop.p100k <- sum(cnrty.in[20])/nrow(unique(cnrty.in[2]))
  v <- rbind(v, data.frame(t(colMeans(cnrty.in[2:20])), Avg.Pop.p100k) ) #want 100k per decision to be sum of countries but mean of years
}
v <- v[2:(n+1),] # remove our NA's
row.names(v) <- euro.cntry

# Country year | Predictors... | Total migration
euro.cntry <- unique(paste(df.Eurodt$Country.of.origin..ISO., df.Eurodt$Year)) # split our countries 
n <- length(euro.cntry)
w <- data.frame(matrix(NA, ncol = 21))
col.nam <- c('Country', names(Ctry.Dt[2:20]), 'Pop.p100k' )
colnames(w) <- col.nam
for (i in 1:n){
  Country <- substr(euro.cntry[i], 1,3)
  Year <- substr(euro.cntry[i], 5,8)
  cnrty.in <- Ctry.Dt[Ctry.Dt$Country.of.origin..ISO. == Country 
                      & Ctry.Dt$Year == Year,]
  Pop.p100k <- sum(cnrty.in[20])/nrow(unique(cnrty.in[2]))
  w <- rbind(w, data.frame(Country, t(colMeans(cnrty.in[2:20])), Pop.p100k, stringsAsFactors = FALSE) ) #want 100k per decision to be sum of countries but mean of years
}
w <- w[2:(n+1),] # remove our NA's
row.names(w) <- euro.cntry
Cnty_data <- w
# Data is sorted got countries & ~20 variables for analysis
df.Eurodt # full version EuroDt.no_na just numerics

Orgn.co.dt <- v 

pr.out <- prcomp(Orgn.co.dt, scale = TRUE) 
pr.out <- prcomp(EuroDt.no_na, scale = TRUE) 
# scale = True will auto normalise our variables for this analysis to N(0, 1)
names(pr.out)
#Return our mean and st dev before scaling
pr.out$center
pr.out$scale 
pr.out$rotation # Returns our ϕ1, ϕ2, ... 
dim(pr.out$x) # x is our xij's X has dimensions 50 x 4
biplot(pr.out, scale = 0) #PCA only differs from a sign flip
pr.out$sdev
pr.var <- pr.out$sdev^2
pr.var
pve <- pr.var/sum(pr.var) #calculate our explained variance
pve
round(cumsum(pve), digits = 2)
par(mfrow = c(1, 2) )
plot(pve, xlab = 'Princ Comp', ylab = 'Prop of Var Expla', ylim = c(0, 1), type = "b")
plot(cumsum(pve), xlab = 'Princ Comp', ylab = 'Cummla Prop of Var Expla', ylim = c(0, 1), type = 'b')
#Scree plots for optimal Princ Comps (Freq vs Commlative)
# ~80% of our variation can be explained with around ~5 of our principal components
# 2 explains around 52% of our variation so a bi-plot gives rough indicator of similar countries
# We can see Russia is indep of itself , Gibralter, Lichenstein, Andorra, Monaco, San Marino are similarish
# This can show us a few things, removes our outliers (russia, lichenstein, etc) and shows which of our
# predictors is correlated to each other - not sure if russia is ruining the predictors but it looks like
# we have 3 ish groups of correlated variables

boxplot(t(Orgn.co.dt2))

# Plot of our PCA & our countries with missing data that got filled in 
PCAdt <- pr.out$x[,1:2]
PCAdt <- cbind(Orgn.co.dt2$Natural.Increase..Crude..per.100k...O, Orgn.co.dt2$Life.Expectancy..Year....O)
colnames(PCAdt) <- c('Natural Incr per 100k', 'Life Expectancy')
row.names(PCAdt) <- rownames(Orgn.co.dt2)
colour <- rep(1, nrow(PCAdt))
PCAdt <- cbind(PCAdt, colour)
missing <- unique(df.Eurodt[unique(names(ina)),2])
(no_missing <- rowSums(is.na(dt.nas[unique(names(ina)),])))
names(which(no_missing > 2))
missing <- unique(df.Eurodt[names(which(no_missing > 0)), 2])
missing1 <- unique(df.Eurodt[names(which(no_missing > 2)), 2])
if (sum(missing %in% ctry.to.remove) > 0){
missing <- missing[-(which(missing %in% ctry.to.remove, arr.ind=TRUE))] }
if (sum(missing1 %in% ctry.to.remove) > 0){
  missing1 <- missing1[-(which(missing %in% ctry.to.remove, arr.ind=TRUE))] }
PCAdt[missing, 'colour'] <- 4
PCAdt[missing1, 'colour'] <- 2
par(mfrow = c(1, 1) )
plot(PCAdt, col = PCAdt[,3], main = 'Component Analysis of Missing Data')
text(PCAdt[,1], PCAdt[,2], row.names(PCAdt), cex=0.9, pos=4, col = PCAdt[,3])
legend("topright", inset=.02, title="No. of Missing Vals",
       c("1","> 2"), fill=c("blue", "red"), horiz=TRUE, cex=0.8)

# Remove and redo our PCA with the new results
ctry.to.remove <- c('RUS', 'AND', 'GIB', 'MCO', 'SMR', 'LIE', 'VAT')
Orgn.co.dt2 <- v[!(row.names(v) %in% ctry.to.remove),]
Orgn.co.dt2
pr.out <- prcomp(Orgn.co.dt2, scale = TRUE) 
par(mfrow = c(1, 1) )
biplot(pr.out, scale = 0)
pr.var <- pr.out$sdev^2
pve <- pr.var/sum(pr.var) #calculate our explained variance
round(cumsum(pve), digits = 2)
par(mfrow = c(1, 2) )
plot(pve, xlab = 'Princ Comp', ylab = 'Prop of Var Expla', ylim = c(0, 1), type = "b")
plot(cumsum(pve), xlab = 'Princ Comp', ylab = 'Cummla Prop of Var Expla', ylim = c(0, 1), type = 'b')
# Here is a plot removing Russia and other groups, with ~52% of our component variance we can sort of 
# see countries with similarties i.e italy, germany; UK, France, Spain, Greece; Romainia, Bulgaria, Belarus, Serbia
# ; Denmark, Norway, Finnland, Belgium, Malta, Swiss, Netherlands; Vatican (Initially All blanks??), Croatia, Poland, Bosnia etc
# however in our bi-plot most of the variance of our outcome variable - avg pop moving p100k isn't particulary high (shortest arrow)

# Clustering - Hierachial and Kmeans
# Do in 2 ways, Hierachial on all components or use PCA and do a kmeans with our first 2 PCA
# However as our work is used for supervised learning, and our total displacement (outcome for supervised) 
# was low for we may be drawn closer to hierarchial? (for a better rep of pop displacement?)

# PCA with 1 & 2 method will only give us a 22% rep of the total variance
# but can be graphically shown so will go through that first
par(mfrow = c(1, 1))
pr.out <- prcomp(Orgn.co.dt2, scale = TRUE) 
biplot(pr.out)

PCAdt <- pr.out$x[,1:2]
plot(PCAdt)
text(PCAdt[,1], PCAdt[,2], row.names(PCAdt), cex=0.6, pos=4, col="red")

km.out <- kmeans(PCAdt, 2, nstart = 5)
par(mfrow = c(1, 1))
plot(PCAdt, col = (km.out$cluster + 1), main = 'K-means Cluster results w/ k = 2', xlab = '', ylab = '')
text(PCAdt[,1], PCAdt[,2], row.names(PCAdt), cex=0.6, pos=4, col=(km.out$cluster + 1))

km.out <- kmeans(PCAdt, 11, nstart = 20)
plot(PCAdt, col = (km.out$cluster + 1), main = 'K-means Cluster results w/ k = 11', xlab = '', ylab = '') 
text(PCAdt[,1], PCAdt[,2], row.names(PCAdt), cex=0.6, pos=4, col=(km.out$cluster + 1))

set.seed(4)
km.out <- kmeans(PCAdt, 3, nstart = 1)
km.out$tot.withinss
km.out <- kmeans(PCAdt, 3, nstart = 20 )
km.out$tot.withinss

x <- PCAdt
#x <- Orgn.co.dt2
hc.complete <- hclust(dist(x), method = 'complete')
hc.average <- hclust(dist(x), method = 'average')
hc.single <- hclust(dist(x), method = 'single')
par(mfrow = c(1, 3))
plot(hc.complete, main = " Complete Linkage ", xlab = "", sub = "", cex = .9)
plot(hc.average , main = " Average Linkage ", xlab = "", sub = "", cex = .9)
plot(hc.single, main = " Single Linkage ", xlab = "", sub = "", cex = .9)

#cutree(hc.complete, 4)
#cutree(hc.average, 2)
#cutree(hc.single, 2)
#cutree(hc.single, 4)

par(mfrow = c(1, 1))
n <- 10
plot(PCAdt, col = (cutree(hc.complete, n) + 2), main = 'Hier Cluster results w/ k = n', xlab = 'pc1', ylab = 'pc2') 
text(PCAdt[,1], PCAdt[,2], row.names(PCAdt), cex=0.6, pos=4, col=(cutree(hc.complete, n) + 2))
plot(PCAdt, col = (cutree(hc.average, n) + 2), main = 'Hier Cluster results w/ k = n', xlab = 'pc1', ylab = 'pc2') 
text(PCAdt[,1], PCAdt[,2], row.names(PCAdt), cex=0.6, pos=4, col=(cutree(hc.average, n) + 2))
plot(PCAdt, col = (cutree(hc.single, n) + 2), main = 'Hier Cluster results w/ k = n', xlab = 'pc1', ylab = 'pc2') 
text(PCAdt[,1], PCAdt[,2], row.names(PCAdt), cex=0.6, pos=4, col=(cutree(hc.single, n) + 2))

#add arrows
g <- pr.out$rotation[, 1:2]
len <- 6
arrows(0, 0, len * g[, 1],  len * g[, 2], length = 0.01, col = "darkcyan" )
text(len * g[,1], len * g[,2], row.names(g), cex=0.6, pos=4, col = "darkcyan")

plot(hc.complete, main = " Complete Linkage ", xlab = "", sub = "", cex = .9)
abline(h = 2.1, col = 'red')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Begin my tree analysis with these groups - either remove groups with too little data
# or try to do a bad tree diagram? 
(sort(Country_Grp <- cutree(hc.complete, n)))

i<- 3
for (i in c(3, 5, 8)){
(Grp1 <- names(Country_Grp[Country_Grp == i]))
Grp10dt <- df.Eurodt[df.Eurodt$Country.of.origin..ISO. %in% Grp1,]
Grp10dt <- Grp10dt[Grp10dt$Year > 2004,]
Grp1_Ctry <- unique(Grp10dt$Country.of.origin)
(Grp_nm <- paste('Group:', toString(Grp1_Ctry)))
#names(Grp10dt)
Grp10dt <- data.frame(Grp10dt[1:68], Grp10dt[74], stringsAsFactors=FALSE)
# Remove my other "Potential Predictors" Decided to use Total.Per.100k 
train <- sample(1:nrow(Grp10dt), nrow(Grp10dt)/2)
asy.tree <- tree(Total.Per.100k~ log(Population..A) + ., Grp10dt, subset = train)
summary(Grp10dt$Total.Per.100k)
summary(asy.tree)
plot(asy.tree)
text(asy.tree, pretty = 0)
# Check if  our tree is worth pruning
cv.asy <- cv.tree(asy.tree)
plot(cv.asy$size, cv.asy$dev, type = 'b')
# our dev is our deviance & size is the no. of branches
# looks like it's worth pruning, past 5-7ish our deviance doesn't go down much
prune.asy <- prune.tree(asy.tree, best = 3)
plot(prune.asy, main = Grp_nm)
text(prune.asy, pretty = 0)
summary(prune.asy)
# pruned table using 5 branches instead of 10
yhat <- predict(asy.tree, newdata = Grp10dt[-train,])
Grp10dt.test <- Grp10dt[-train, "Total.Per.100k"]
plot(yhat, Grp10dt.test, main = Grp_nm )
abline(0,1)
mse <- mean((yhat - Grp10dt.test)^2)
mse^0.5
cat(mse^0.5, ' vs ', sd(Grp10dt.test),  ' ', Grp_nm, '\n')
#little bit of residual analysis (Should be using residual tools)
tree.resid <- resid(asy.tree)
plot(tree.resid, fitted(asy.tree), ylab = 'Fitted Residuals', main = Grp_nm )
abline(h = c(4, 8, -4, -8))
# Check which country/year is outlying
outliers.tre <- which(abs(tree.resid) > 1000 )
#outliers.tre <- which(abs(tree.resid) > 4, abs(tree.resid) < 8)
# Return our countries
outliers.point <- Grp10dt[outliers.tre, c('Total.Per.100k','Country.of.asylum') ] 
Resid <- tree.resid[outliers.tre]
Model <- Grp10dt.test[outliers.tre]
Yhat <- yhat[outliers.tre]
outliers.point <- data.frame(outliers.point, Resid, Model, Yhat)
outliers.point
}

grps <- data.frame(Country_Grp, Orgn.co.dt2)
write.csv(grps,"/Users/kelvin/Desktop/Maths/Final Year Project/Grpdt.csv")
write.csv(Cnty_data,"/Users/kelvin/Desktop/Maths/Final Year Project/CountryYearData.csv")

