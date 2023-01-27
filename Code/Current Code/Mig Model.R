# Unsupervised Learning - will use a unsupervised ML model to find similarities of European Asylum countries

# Aim: Data set of migration from country to country and the respective countries economic/social factors in that year using Unsupervised 
# Learning can I group them in a way that they can be modelled through tree learning?

?read.csv
MgrDt <- read.csv(file = "/Users/kelvin/Desktop/Maths/Final Year Project/Migration Full Data - Clear Names.csv", blank.lines.skip = FALSE)
colnames(MgrDt)
#Birth <- MgrDt$Birth.Rate.O * MgrDt$Popultion.O
#Population <- MgrDt$Popultion.O
#plot(Birth, Population)
MgrDt$GDP.A <- MgrDt$GDP.A/MgrDt$Popultion.A
MgrDt$GDP.O <- MgrDt$GDP.O/MgrDt$Popultion.O
MgrDt$GDP.D <- MgrDt$GDP.O/MgrDt$GDP.A
#MgrDt <- read.csv(file = "/Users/kelvin/Desktop/Maths/Final Year Project/Migration Full Data Only.csv", blank.lines.skip = FALSE)
#MgrDt2 <- MgrDt
EuroDt <- MgrDt[MgrDt$Continent.O == 'Europe',] # Fix to looking at European Asylums moving out 
EuroDt <- EuroDt[!EuroDt$Country.O == 'Unknown',] #Remove asylums of unkown country as we have entire voids we cannot fill 
# (I guess we could take world averages but i'm simplifying for this example)
EuroDt <- Filter(function(x)(length(unique(x))>1), EuroDt) # Removes single valued factors for our columns No use for us as it won't mean much & take space 
names(EuroDt)
dim(EuroDt)
EuroDt$Country.O[(EuroDt$Country.O == '')] = 'UKN' # change all the blank unknown iso's to UKN
EuroDt$Country.A[EuroDt$Country.A == ''] = 'UKN'
EuroDt$Country.O[(EuroDt$Country.O == 'United Kingdom of Great Britain and Northern Ireland')] = 'United Kingdom' # change all the blank unknown iso's to UKN
EuroDt$Country.A[EuroDt$Country.A == 'United Kingdom of Great Britain and Northern Ireland'] = 'United Kingdom'

# Need a format like Example Name: <number> | <number> | <number> | <number> etc. 
# Get the numeric columns 
Orgn.to.Asy.Iso<- paste(EuroDt$Year, EuroDt$ISO.O, 'to' , EuroDt$ISO.A) #Get our abr asylum moves
#length(Orgn.to.Asy.Iso)
#length(unique(Orgn.to.Asy.Iso)) # this needs to be same as the length "Check on Repeats" 
Country_Origin <- EuroDt$Country.O
#length(Country_Origin) #needs to be 11126
row.names(EuroDt) <- Orgn.to.Asy.Iso
#dim(EuroDt) # Reduced my columns to 74, mainly the info cols (for myself)
#head(EuroDt)
#dim(EuroDt)
#colnames(EuroDt)
# data prep complete 
EuroDt.Char <- Filter(function(x)(typeof(x) == 'character'), EuroDt) # keep the ones w/ characters stored
EuroDt.Num <- Filter(function(x)(typeof(x) != 'character'), EuroDt) # Filter out any columns which use characters i.e non-numerical characters
Orgn.to.Asy.Iso <- row.names(EuroDt)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Box-plots for Outliers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
# Only look at origin countries right now for ease of use
EuroOrigin <- EuroDt
?boxplot
par(mfrow=c(1,1))
par(mar=c(10,4,2,1))
EuroDt.Num2 <- scale(EuroDt.Num, center = FALSE, scale = TRUE)
#boxplot(EuroDt.Num[,2:17], las=2, main = 'Box plots of Origin Country Features')
boxplot(EuroDt.Num2[,c(2:17)], las=2, main = 'Scaled Box plots of Origin Country Features') 
abline(h=0)

features <- c('X..1.90.O', 'Popultion.O', 'Maternal.Mortality.O', 'Homicide.O')
scaler <- attr(EuroDt.Num2, 'scaled:scale')[features] 
summary(EuroDt.Num2[,features])
iqr <- c()
for (i in 1:4){ iqr<-c(iqr,IQR(EuroDt.Num2[,features[i]], na.rm = TRUE)) } 
unique(EuroOrigin[EuroOrigin$X..1.90.O > 4*scaler[1],2]) # Moldova
unique(EuroOrigin[EuroOrigin$Popultion.O > 2*scaler[2],2]) # Russia
unique(EuroOrigin[-(EuroOrigin$Popultion.O < 0.002*scaler[2]) | #Under 108K Pop - SMR AND GIB MCO LIE
                    (EuroOrigin$Popultion.O > 2*scaler[2]) | #Over 100M Pop - RUS
                    is.na(EuroOrigin$Popultion.O), 2]) #No Value - VAT
unique(EuroOrigin[EuroOrigin$Maternal.Mortality.O > 2.6*scaler[3],2]) # RUS, ROU
unique(EuroOrigin[EuroOrigin$Homicide.O > 2.6*scaler[4],2]) # RUS

ctry.to.remove <- c('RUS', 'AND', 'GIB', 'MCO', 'SMR', 'LIE', 'VAT', 'MDA')
EuroDt <- EuroOrigin[!(EuroOrigin$ISO.O %in% ctry.to.remove),]
filter <- rownames(EuroDt)
EuroDt.Char <- EuroDt.Char[filter,]
EuroDt.Num <- EuroDt.Num[filter,]
?boxplot()
colours <- cbind(EuroDt.Num2[,c(2:17)], 1)
colours[!(rownames(colours) %in% filter),17] <- 2
boxplot(EuroDt.Num2[,c(2:17)], las=2,
        main = 'Scaled Box plots of Origin Country Features') 
#points(EuroDt.Num2[filter,c(2:17)])
abline(h=0)
boxplot(scale(EuroDt[,4:19], center = FALSE), las=2
        , main = 'Filtered & Scaled Box plots of Origin Country Features') 
abline(h=0)
par(mar=c(5.1, 4.1, 4.1, 2.1))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Missing Data Analysis`~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# using PCA to fill in our missing data
fit.svd <- function(X, M = 1) { # mat mult subset size M of d * v^t this finds us our zij
  svdob <- svd(X)
  with(svdob, 
       u[, 1:M, drop = FALSE] %*% (d[1:M] * t(v[,1:M, drop = FALSE] ) )
  ) }

Glob.Scl.EuroDt <- scale(EuroDt.Num, center = TRUE, scale = TRUE)  # Create a global scale 
glob_xbar <- colMeans(Glob.Scl.EuroDt, na.rm = TRUE) # Create a full column mean in case our local ones are just NA 
glob_xhat <- EuroDt.Num # glob_xhat will be our end result
for (country in unique(EuroDt$ISO.O)) {  # Look into each country and do a Missing data analysis 
  filter <- rownames(EuroDt[EuroDt$ISO.O==country,])
  Scl.EuroDt <- scale(EuroDt.Num[filter,], center = TRUE, scale = TRUE)  # Scale (1) # This is our data-set with missing data
  dt.nas <- Scl.EuroDt # our data with "holes"
  index.na <- is.na(dt.nas) # find them and rec their location 
  inb <- which(index.na == TRUE, arr.ind= TRUE)[,2] # which column they're in
  ina <- which(index.na == TRUE, arr.ind= TRUE)[,1] # which row they're in
  Xhat <- dt.nas # what will be our new data 
  xbar <- colMeans(dt.nas, na.rm = TRUE) # calc the mean of our columns
  nul_cols <- names(xbar[is.na(xbar)])
  xbar[is.na(xbar)] <- glob_xbar[is.na(xbar)]
  # Fix Scaling
  attr(Xhat, 'scaled:scale')[nul_cols] <- attr(Glob.Scl.EuroDt, 'scaled:scale')[nul_cols]
  attr(Xhat, 'scaled:center')[nul_cols] <- attr(Glob.Scl.EuroDt, 'scaled:center')[nul_cols]
  inb.na <- which(is.na(xbar), arr.ind = TRUE)
  Xhat[index.na] <- xbar[inb] # fill the empty vals with col averages (2)
  sum(is.na(Xhat)) # null check
  thresh <- 1e-7 # create our error margin (difference in old mse vs new mse)
  rel_err <- 1 # initial 
  iter <- 0
  ismiss <- is.na(dt.nas) # Finding the row & column of our missing data
  mssold <- mean((scale(dt.nas, xbar, FALSE)[!ismiss])^2)  #store mse of non missing vals 
  mss0 <- mean(dt.nas[!ismiss]^2) # meansq of non missing vals
  while (rel_err > thresh) { # (5)
    iter <- iter + 1
    Xapp <- fit.svd(Xhat, M = 1) #comp our Princ Comp (3)
    Xhat[ismiss] <- Xapp[ismiss] #replace the mean xbars with our new princ comp (4)
    mss <- mean(((dt.nas - Xapp)[!ismiss])^2)
    rel_err <- (mssold - mss)/mss0 # old mse - new mse/ variance 
    mssold <- mss
    cat('Iter:', iter, 'MSS:', mss, 'Rel. Err:', rel_err, '\n')
  }
  Xhat
  unscl.Xhat <- t(apply(Xhat, 1, 
          function(r) r * attr(Xhat, 'scaled:scale') + 
            attr(Xhat, 'scaled:center')))
  cat(country, ' ', sum(is.na(unscl.Xhat)), '\n' )
  glob_xhat[filter,] <- unscl.Xhat # Check for blanks 
}

#~~~~~~~~~~~~~~~~~~~Example of Missing Data Analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
excols <- c(1:18)
example <- rownames(EuroDt[EuroDt$ISO.O=='SRB',])
dt.nas2 <- Glob.Scl.EuroDt
index.na2 <- is.na(dt.nas2)
inb2 <- which(index.na2 == TRUE, arr.ind= TRUE)[,2]
xbar <- colMeans(dt.nas2, na.rm = TRUE)
dt.nas2[index.na2] <- xbar[inb2]
means <- t(apply(dt.nas2, 1, 
                 function(r) r * attr(dt.nas2, 'scaled:scale') + 
                   attr(dt.nas2, 'scaled:center')))[, excols]
xbar.srb <- colMeans(dt.nas2[example,], na.rm = TRUE)
dt.nas2[index.na2] <- xbar.srb[inb2]
means.loc <- t(apply(dt.nas2, 1, 
                 function(r) r * attr(dt.nas2, 'scaled:scale') + 
                   attr(dt.nas2, 'scaled:center')))[, excols]
iter12 <- glob_xhat[, excols]
Initial <- EuroDt[example,excols] #Example with missing data
Initial[example,excols]
means[example,excols] # after adding column means 
means.loc[example,excols] # after adding local column means 
iter12[example,excols] # 12th PCA Iteration
Initial[Initial$Year == 2015,'Education.Budget.O']

points <- iter12[example,c(1,16)]
Average <- data.frame(matrix(NA, ncol = 2))
colnames(Average) <- c('Year','Education.Budget.O')
for (Year in unique(points[,1])){
  Education.Budget.O <- mean(points[points[,1] == Year, 2])
  Average <- rbind(Average, data.frame(Year, Education.Budget.O))
}
Initial[example,c(1,18)]
par(mfrow = c(1,1))
plot(Initial[example,c(1,18)], type = 'l', main = "Serbia's % Education Budget per Year")
points(means[example,c(1,16)] , col = 2)
lines(means[example,c(1,16)], type = 'l' , col = 2, lty = 2 )
points(means.loc[example,c(1,16)] , col = 4)
lines(means.loc[example,c(1,16)], type = 'l' , col = 4, lty = 2 )
#points(iter12[example,c(1,16)] , col = 4)
#lines(iter12[example,c(1,16)], type = 'l' , col = 4, lty = 2 )
points(Average, col = 3)
lines(Average, type = 'l' , col = 3, lty = 2 )
legend("bottomleft", inset=.02, title="Dataset",
       c("Original","European Mean","Serbia Mean","Matrix Completion"),
       fill=c("black","red","Blue","Green"), horiz=FALSE, cex=1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Summarizing Data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Orgn.to.Asy.Iso <- rownames(glob_xhat)
EuroDt.no_na <- glob_xhat
# now I need to unscale this as it doesn't make sense (for now)
# I have 95k rows, this will be impossible to visualize properly, kinda need around 50ish?
#  Need: all my separate countries & their average predictors,& sum of total displacement
# Make it a dataframe again
df.Eurodt <- data.frame(EuroDt.Char, EuroDt.no_na, row.names = Orgn.to.Asy.Iso, stringsAsFactors=FALSE)
# Country | Predictors... (Average over years) | Total migration
dim(df.Eurodt)
colnames(df.Eurodt)
Ctry.Dt <- data.frame(df.Eurodt[2], df.Eurodt[6:22], df.Eurodt[56], row.names = NULL, stringsAsFactors=FALSE)
col.nam <- c(names(Ctry.Dt[2:18]), 'Avg.Pop.p100k' )
euro.cntry <- unique(df.Eurodt$ISO.O) # split our countries 
n <- length(unique(df.Eurodt$ISO.O))
v <- data.frame(matrix(NA, ncol = 18))
colnames(v) <- col.nam
for (Country in euro.cntry){
  cnrty.in <- Ctry.Dt[Ctry.Dt$ISO.O == Country,]
  # data[row, col]
  Avg.Pop.p100k <- sum(cnrty.in[19])/nrow(unique(cnrty.in[2]))
  v <- rbind(v, data.frame(t(colMeans(cnrty.in[2:18])), Avg.Pop.p100k) ) #want 100k per decision to be sum of countries but mean of years
}
v <- v[2:(n+1),] # remove our NA's
row.names(v) <- euro.cntry

# Same thing but with years too 
# Country year | Predictors... | Total migration
euro.cntry <- unique(paste(df.Eurodt$ISO.O, df.Eurodt$Year)) # split our countries 
n <- length(euro.cntry)
w <- data.frame(matrix(NA, ncol = 18))
col.nam <- c(names(Ctry.Dt[2:18]), 'Pop.p100k' )
colnames(w) <- col.nam
for (i in 1:n){
  Country <- substr(euro.cntry[i], 1,3)
  Year <- substr(euro.cntry[i], 5,8)
  cnrty.in <- Ctry.Dt[Ctry.Dt$ISO.O == Country 
                      & Ctry.Dt$Year == Year,]
  Pop.p100k <- sum(cnrty.in[18])/nrow(unique(cnrty.in[2]))
  w <- rbind(w, data.frame(t(colMeans(cnrty.in[2:18])), Pop.p100k, stringsAsFactors = FALSE) ) #want 100k per decision to be sum of countries but mean of years
}
w <- w[2:(n+1),] # remove our NA's
row.names(w) <- euro.cntry
Cnty_data <- w
# Data is sorted got countries & ~20 variables for analysis
df.Eurodt # full version EuroDt.no_na is just numerics

par(mfrow= c(1,1))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ PCA on my data (Intro)~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Orgn.co.dt <- v 
Cnty_data[,which(is.na(Cnty_data), arr.ind = TRUE)[,2]]
pr.out <- prcomp(Orgn.co.dt, scale = TRUE) 
# scale = True will auto normalise our variables for this analysis to N(0, 1)
names(pr.out)
#Return our mean and st dev before scaling
pr.out$center
pr.out$scale 
pr.out$rotation # Returns our ϕ1, ϕ2, ... 
dim(pr.out$x) # x is our xij's X has dimensions 50 x 4
par(mfrow = c(1, 1) )
biplot(pr.out, scale = 0) #PCA only differs from a sign flip'
PCAdt <- pr.out$x[,1:2]
plot(PCAdt, main = '1st and 2nd Principal Components Plot')
text(PCAdt[,1], PCAdt[,2], row.names(PCAdt), cex=0.6, pos=4, col="red")
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
# 2 explains around 52% of our variation so a bi-plot gives rough indicator of simi

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Clustering - Hierachial and Kmeans
# Do in 2 ways, Hierachial on all components or use PCA and do a kmeans with our first 2 PCA
# However as our work is used for supervised learning, and our total displacement (outcome for supervised) 
# was low for we may be drawn closer to hierarchial? (for a better rep of pop displacement?)

# PCA with 1 & 2 method will only give us a 45% rep of the total variance
# but can be graphically shown so will go through that first
par(mfrow = c(1, 1))
pr.out <- prcomp(Orgn.co.dt, scale = TRUE) 
biplot(pr.out)

PCAdt <- pr.out$x[,1:2]
plot(PCAdt, main = '1st & 2nd Principal Components')
text(PCAdt[,1], PCAdt[,2], row.names(PCAdt), cex=0.6, pos=4, col="red")

km.out <- kmeans(PCAdt, 2, nstart = 5) 
km.out$centers
par(mfrow = c(1, 1))
plot(PCAdt, col = (km.out$cluster + 1), main = 'K-means Cluster results w/ k = 2', xlab = '', ylab = '')
text(PCAdt[,1], PCAdt[,2], row.names(PCAdt), cex=0.6, pos=4, col=(km.out$cluster + 1))
points(km.out$centers, col = (km.out$cluster + 4), pch = 4, cex = 3)

km.out <- kmeans(PCAdt, 6, nstart = 20)
plot(PCAdt, col = (km.out$cluster + 1), main = 'K-means Cluster results w/ k = 6', xlab = 'PC1', ylab = 'PC2') 
text(PCAdt[,1], PCAdt[,2], row.names(PCAdt), cex=0.6, pos=4, col=(km.out$cluster + 1))
points(km.out$centers, col = (km.out$cluster + 9), pch = 4, cex = 3)

km.out <- kmeans(PCAdt, 11, nstart = 20)
plot(PCAdt, col = (km.out$cluster + 1), main = 'K-means Cluster results w/ k = 11', xlab = 'PC1', ylab = 'PC2') 
text(PCAdt[,1], PCAdt[,2], row.names(PCAdt), cex=0.6, pos=4, col=(km.out$cluster + 1))
points(km.out$centers, col = (km.out$cluster + 9), pch = 4, cex = 3)

set.seed(4)
km.out <- kmeans(PCAdt, 3, nstart = 1)
km.out$tot.withinss
km.out <- kmeans(PCAdt, 3, nstart = 20 )
km.out$tot.withinss

x <- PCAdt
x <- Orgn.co.dt
boxplot(Orgn.co.dt[1:5])
rownames(Orgn.co.dt[Orgn.co.dt$GDP.O > 5*exp(15),])
hc.complete <- hclust(dist(x), method = 'complete')
hc.average <- hclust(dist(x), method = 'average')
hc.single <- hclust(dist(x), method = 'single')
par(mfrow = c(1,3))
plot(hc.complete, main = " Complete Linkage ", xlab = "", sub = "", cex = .9)
plot(hc.average , main = " Average Linkage ", xlab = "", sub = "", cex = .9)
plot(hc.single, main = " Single Linkage ", xlab = "", sub = "", cex = .9)

#cutree(hc.complete, 4)
#cutree(hc.average, 2)
#cutree(hc.single, 2)
#cutree(hc.single, 4)

par(mfrow = c(1, 3))
n <- 12
plot(PCAdt, col = (cutree(hc.complete, n) + 2), main = 'Hier Cluster results w/ k = 12', xlab = 'pc1', ylab = 'pc2') 
text(PCAdt[,1], PCAdt[,2], row.names(PCAdt), cex=0.6, pos=4, col=(cutree(hc.complete, n) + 2))
plot(PCAdt, col = (cutree(hc.average, n) + 2), main = 'Hier Cluster results w/ k = 12', xlab = 'pc1', ylab = 'pc2') 
text(PCAdt[,1], PCAdt[,2], row.names(PCAdt), cex=0.6, pos=4, col=(cutree(hc.average, n) + 2))
plot(PCAdt, col = (cutree(hc.single, n) + 2), main = 'Hier Cluster results w/ k = 12', xlab = 'pc1', ylab = 'pc2') 
text(PCAdt[,1], PCAdt[,2], row.names(PCAdt), cex=0.6, pos=4, col=(cutree(hc.single, n) + 2))

#add arrows
g <- pr.out$rotation[, 1:2]
len <- 6
arrows(0, 0, len * g[, 1],  len * g[, 2], length = 0.01, col = "darkcyan" )
text(len * g[,1], len * g[,2], row.names(g), cex=0.6, pos=4, col = "darkcyan")

par(mfrow = c(1, 1))
plot(hc.complete, main = " Complete Linkage ", xlab = "", sub = "", cex = .9)
abline(h = 9000200, col = 'red')



#Tree Analysis, Linear Model etc. (Supervised Learning)
library('tree')
(sort(Country_Grp <- cutree(hc.complete,n))) # will look at 2,3,7
analysis <- c()
for (i in unique(Country_Grp)){
  (Grp1 <- names(Country_Grp[Country_Grp == i]))
  Grp10dt <- df.Eurodt[df.Eurodt$ISO.O %in% Grp1,]
  Grp1_Ctry <- unique(Grp10dt$Country.O)
  Grp_nm <- paste('Group ', i, ':', toString(Grp1_Ctry))
  train <- sample(1:nrow(Grp10dt), nrow(Grp10dt)/2)
  Grp10dt.test <- Grp10dt[-train, "Total"]
  cat(Grp_nm, '\n', 'sd:', sd(Grp10dt.test), '\n', 'mean', mean(Grp10dt.test),'\n')
}
tolook <- c(2, 3, 7)
i = 6
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Modelling~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(Grp1 <- names(Country_Grp[Country_Grp == i]))
Grp10dt <- df.Eurodt[df.Eurodt$ISO.O %in% Grp1,]
#Grp10dt <- Grp10dt[Grp10dt$Year > 2004,]
grpex <- Filter(function(x)(typeof(x) != 'character'), Grp10dt)
grpex <- Filter(function(x)(length(unique(x))>1), grpex)
#biplot(pr.out)
pr.out <- prcomp(grpex, scale = TRUE) 
PCAdt <- pr.out$x[,1:2]
plot(PCAdt)
text(PCAdt[,1], PCAdt[,2], row.names(PCAdt), cex=0.6, pos=4, col="red")
pr.var <- pr.out$sdev^2
pve <- pr.var/sum(pr.var) #calculate our explained variance
round(cumsum(pve), digits = 2)

# Looks like it needs to cluster, do a 2-cluster 
hm.out <- hclust(dist(grpex), method = 'complete')
km.out <- kmeans(PCAdt, 2, nstart = 5) 
par(mfrow = c(1, 1))
plot(PCAdt, col = (km.out$cluster + 1), main = 'K-means Cluster results w/ k = 2', xlab = '', ylab = '')
# 2 Red, 3 Green, 4 Blue
choice <- which(table(km.out$cluster) == max(table(km.out$cluster)))
main_cluster <- names(km.out$cluster[km.out$cluster == choice])
Grp10dt <- Grp10dt[main_cluster,-c(3,4)]

Grp1_Ctry <- unique(Grp10dt$Country.O)
(Grp_nm <- paste('Group:', toString(Grp1_Ctry)))
#names(Grp10dt)
train <- sample(1:nrow(Grp10dt), nrow(Grp10dt)/2)

# Gravity Model (Need distances as a variable first) 

# Linear Model Analysis
flmdl <- lm(Total~ log(Popultion.O) - Country.O  + . , Grp10dt, subset = train)
asy.lm <- step(flmdl, direction = "backward", k = log(n))
summary(asy.lm)
asy.lm$coefficients
library('splines')
# Splines
paste(colnames(Grp10dt[,7:22]), collapse = ' + ')
spflmdl <- lm(Total~ bs(Growth.O + Popultion.O + GDP.O + X..1.90.O 
                        + Malnourished.O + Maternal.Mortality.O 
                        + Suicide.Mortality.O + Life.Expectancy.O 
                        + X..Male.O + X..Rural.O + Death.Rate.O 
                        + Birth.Rate.O + Natural.Increase.O 
                        + X..Crop.Production.O + Education.Budget.O 
                        + Homicide.O), Grp10dt, subset = train)
asy.spl <- step(spflmdl, direction = "backward", k = log(n))
summary(asy.spl)

# Tree Model
asy.tree <- tree(Total~ log(Popultion.A) + ., Grp10dt, subset = train)
summary(Grp10dt$Total)
summary(asy.tree)
#install.packages('rpart')
#install.packages('rpart.plot')
library('rpart')
library('rpart.plot')
#Grp10dt[,c(13,30,38:53)] <- Grp10dt[,c(13,30,38:53)]*100
#colnames(Grp10dt)[c(30,40,49)] <- c('Male.Per.A (%)','GDP.D (%)', 'Birth.Rate.D (%)')
#colnames(Grp10dt)[c(30,40,49)] 
asy.tree <- tree(Total~ log(Popultion.A) + ., Grp10dt, subset = train)
asy.tree <- rpart(Total~ log(Popultion.A) + ., Grp10dt, subset = train)
rpart.plot(asy.tree)
plot(asy.tree)
text(asy.tree, pretty = 0)
# Check if  our tree is worth pruning
cv.asy <- cv.tree(asy.tree)
plot(cv.asy$size, cv.asy$dev, type = 'b')
# our dev is our deviance & size is the no. of branches
# looks like it's worth pruning, past 5-7ish our deviance doesn't go down much
prune.asy <- prune.tree(asy.tree, best = 4)
plot(prune.asy, main = Grp_nm)
text(prune.asy, pretty = 0)
summary(prune.asy)
# pruned table using 5 branches instead of 10

# More Advanced Tree Learning:
# random forest/Bagging
library(randomForest)
Bag.Tree <- randomForest(Total ~ log(Popultion.A) + ., Grp10dt
                         , subset = train, mtry = 8, ntree = 1000,  importance = TRUE)
Bag.Tree
# ~85% var explained

# Boosting
library(gbm)
?gbm
boost.tree <- gbm(Total~.,data=Grp10dt[train,-(1:3)], distribution="gaussian"
                  , n.trees =5000, interaction.depth = 4, shrinkage = 0.2,)
boost.tree
summary(boost.tree)
plot(boost.tree, i = "Birth.Rate.A")

# Model Comparison
Grp10dt.test <- Grp10dt[-train, "Total"]
names(Grp10dt.test) <- rownames(Grp10dt[-train,])
# Lin model 
yhat.lm <- predict(asy.lm, newdata = Grp10dt[-train,])
names(yhat.lm) <- rownames(Grp10dt[-train,])
plot(yhat.lm, Grp10dt.test, main = Grp_nm )
abline(0,1)
mse.lm <- mean((yhat.lm - Grp10dt.test)^2)
mse.lm^0.5

#spline
yhat.spl <- predict(asy.spl, newdata = Grp10dt[-train,])
plot(yhat.spl, Grp10dt.test, main = Grp_nm )
abline(0,1)
mse.spl <- mean((yhat.spl - Grp10dt.test)^2)
mse.spl^0.5

# Tree
yhat <- predict(asy.tree, newdata = Grp10dt[-train,])
plot(yhat, Grp10dt.test, main = Grp_nm )
abline(0,1)
mse <- mean((yhat - Grp10dt.test)^2)
mse^0.5

# Prune
yhat <- predict(prune.asy, newdata = Grp10dt[-train,])
plot(yhat, Grp10dt.test, main = Grp_nm )
abline(0,1)
prune.mse <- mean((yhat - Grp10dt.test)^2)
prune.mse^0.5

#Bagging/Random forest Tree
yhat <- predict(Bag.Tree, newdata = Grp10dt[-train,])
plot(yhat, Grp10dt.test, main = Grp_nm )
abline(0,1)
bag.mse <- mean((yhat - Grp10dt.test)^2)
bag.mse^0.5

#Boosting
yhat <- predict(boost.tree, newdata = Grp10dt[-train,], n.trees = 1000)
plot(yhat, Grp10dt.test, main = Grp_nm )
abline(0,1)
boost.mse <- mean((yhat - Grp10dt.test)^2)
boost.mse^0.5

results <- c(sd(Grp10dt.test), mse.lm^0.5, mse.spl^0.5, mse^0.5, prune.mse^0.5, bag.mse^0.5, boost.mse^0.5, mean(Grp10dt.test))
model_names <- c('sd', 'lm','spline','Tree','Pruning','Bagg','Boost','Mean')
names(results) <- model_names
barplot(results, main = Grp_nm, type = 'h', xlab = 'Model Name', ylab = 'Mean Square Error')

cat(' Sd:', sd(Grp10dt.test),  ' vs LM:', mse.lm^0.5, ' vs Spl:', mse.spl^0.5, '\n'
    ,'Tree: ', mse^0.5, ' vs Prune:', prune.mse^0.5, ' vs Bagging:', bag.mse^0.5, '\n'
    , 'Boosting:', boost.mse^0.5, ' & Mean:', mean(Grp10dt.test), '\n', Grp_nm, '\n')

analysis <- c(analysis, capture.output(
  cat(' Sd:', sd(Grp10dt.test),  ' vs LM:', mse.lm^0.5, ' vs Spl:', mse.spl^0.5, '\n'
      ,'Tree: ', mse^0.5, ' vs Prune:', prune.mse^0.5, ' vs Bagging:', bag.mse^0.5, '\n'
      , 'Boosting:', boost.mse^0.5, ' & Mean:', mean(Grp10dt.test), '\n', Grp_nm, '\n', '\n')
))
analysis


#Resid analysis (Learn to use Resid tools)
tree.resid <- resid(asy.tree)
plot(tree.resid, fitted(asy.tree), ylab = 'Fitted Residuals', main = Grp_nm )
abline(h = c(sd, 8, -sd, -8))
# Check which country/year is outlying
sd <- sd(Grp10dt.test)
outliers.tre <- which(abs(tree.resid) > sd)
#outliers.tre <- which(abs(tree.resid) > 4, abs(tree.resid) < 8)
Resid <- tree.resid[outliers.tre]
Model <- Grp10dt.test[outliers.tre]
Yhat <- yhat[outliers.tre]
outliers.point <- data.frame(Resid, Model, Yhat)
outliers.point    # Return our outlying countries

# Plots for results in Regression trees vs Rand Forest

asy.tree <- tree(Total~ log(Popultion.A) + ., Grp10dt, subset = train)
yhat <- predict(asy.tree, newdata = Grp10dt[-train,])
plot(yhat, Grp10dt.test, main = Grp_nm )
abline(0,1)
mse <- mean((yhat - Grp10dt.test)^2)
mse^0.5

Bag.Tree <- randomForest(Total ~ log(Popultion.A) + ., Grp10dt
                         , subset = train, mtry = 9, ntree = 1000,  importance = TRUE)

yhat <- predict(Bag.Tree, newdata = Grp10dt[-train,])
plot(yhat, Grp10dt.test, main = Grp_nm )
abline(0,1)
bag.mse <- mean((yhat - Grp10dt.test)^2)
bag.mse^0.5
?varImpPlot
varImpPlot(Bag.Tree, type = 1, main = 'Features & Impact on MSE of Migration Model'
           , sub='For: Switzerland, Austria, Bulgaria & Serbia',
           col.main="darkred", col.lab="blue", col.sub="black")

yhat.lm <- predict(asy.lm, newdata = Grp10dt[-train,])
yhat.bag <- predict(Bag.Tree, newdata = Grp10dt[-train,])
yhat.tree <- predict(asy.tree, newdata = Grp10dt[-train,])

Test.Summ <- unique(substr(names(Grp10dt.test), 1, 8))
Test.Actuals <- rep(NA, length(Test.Summ))
names(Test.Actuals) <- Test.Summ
Test.Pred.tree <- rep(NA, length(Test.Summ))
names(Test.Pred.tree) <- Test.Summ
Test.Pred.bag <- rep(NA, length(Test.Summ))
names(Test.Pred.bag) <- Test.Summ
Test.Pred.lm <- rep(NA, length(Test.Summ))
names(Test.Pred.lm) <- Test.Summ
for (i in Test.Summ) {
  Test.Actuals[i] <- sum(Grp10dt.test[which(substr(names(Grp10dt.test), 1, 8) == i )])
  Test.Pred.tree[i] <- sum(yhat.tree[which(substr(names(yhat.tree), 1, 8) == i )])
  Test.Pred.bag[i] <- sum(yhat.bag[which(substr(names(yhat.bag), 1, 8) == i )])
  Test.Pred.lm[i] <- sum(yhat.lm[which(substr(names(yhat.lm), 1, 8) == i )])
}

(Outlying <- which(Grp10dt.test>4000))
plot((Grp10dt.test[-Outlying]), (yhat.tree[-Outlying]), xlab = 'Test Values'
     , ylab = 'Model Values', main = 'Model vs Test: Swistzerland, Austria, Bulgaria & Serbia'
     , sub = 'Country to Country per Year', col.main="darkred", col.lab="blue", col.sub="black")
points(Grp10dt.test,yhat.bag, col = 4)
points(Grp10dt.test,yhat.lm, col = 3)
#text(Grp10dt.test, yhat.bag, names(Grp10dt.test), cex=0.6, pos=4, col="red")
#text(Grp10dt.test, yhat.tree, names(Grp10dt.test), cex=0.6, pos=4, col="red")
abline(0,1)
legend('topleft', inset=.02, title="Model",
       c("Tree","Random Forest","Linear Model"),
       fill=c("black","blue", "darkgreen"), horiz=FALSE, cex=.9)

(Outlying <- which(Test.Actuals>6000))
plot((Test.Actuals[-Outlying]), (Test.Pred.tree[-Outlying]), xlab = 'Test Values'
     , ylab = 'Model Values', main = 'Model vs Test: Swistzerland, Austria, Bulgaria & Serbia'
     , sub = 'Country per Year', col.main="darkred", col.lab="blue", col.sub="black")
points((Test.Actuals),(Test.Pred.bag), col = 4)
points((Test.Actuals),(Test.Pred.lm), col = 'darkred')
#text(Test.Actuals, Test.Pred.tree, names(Test.Actuals), cex=0.6, pos=4, col="black")
#text(Test.Actuals, Test.Pred.bag, names(Test.Actuals), cex=0.6, pos=4, col="blue")
abline(0,1)
legend(800, 600, inset=.02, title="Model",
       c("Tree","Random Forest","Linear Model"),
       fill=c("black","blue", "darkred"), horiz=FALSE, cex=.9)


mean((Test.Actuals[-Outlying] - Test.Pred.lm[-Outlying])^2)^0.5
mean((Test.Actuals[-Outlying] - Test.Pred.tree[-Outlying])^2)^0.5
mean((Test.Actuals[-Outlying] - Test.Pred.bag[-Outlying])^2)^0.5
mean(Test.Actuals)
sd(Test.Actuals)
# Exporting Data to look at 
grps <- data.frame(Country_Grp, Orgn.co.dt2)
write.csv(EuroDt,"/Users/kelvin/Desktop/Maths/Final Year Project/EuroDt.csv")
write.csv(Cnty_data,"/Users/kelvin/Desktop/Maths/Final Year Project/CountryYearData.csv")
write.csv(Grp10dt,"/Users/kelvin/Desktop/Maths/Final Year Project/groupdataBIH_CZE_CRO_POL_POR_SLK.csv")
