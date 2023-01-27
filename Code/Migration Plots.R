# Plot of our PCA & our countries with missing data that got filled in 
PCAdt <- pr.out$x[,1:2]
PCAdt <- cbind(Orgn.co.dt2$Natural.Increase..Crude..per.100k..O, Orgn.co.dt2$Life.Expectancy..Year...O)
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
