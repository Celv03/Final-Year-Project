install.packages("readxl")
library("readxl")
#Didn't really need this

#Migration <- read.csv(file = "/Users/kelvin/Desktop/Maths/Final Year Project/Migration-Combined.csv")
#using this for now cause - will change to migration (Ins & Outs) for 1 country 

MigUK <- read.csv(file = "/Users/kelvin/Desktop/Maths/Final Year Project/Uk-Asylum.csv")
# Change csv to ukraine for the ukraine details

#head(Migration)
#variables <- colnames(Migration)

head(MigUK)
cols <- colnames(MigUK)

cols[32]
#variables[20]
#Using this to find col names

InpVar <- cols[8:22]
RespVar <- cols[32]
# Get our input variables, response variables to get feature var

#summary(Migration)
#hist(MigUK$Natural.Increase..Crude..per.1k, breaks = 10,counts = MigUK$Total.decisions )
#Doesn't really tell us anything, need

#plot(Total.decisions ~ Maternal.Mortality.per.100k, data = Migration)
plot(Total.decisions ~ Maternal.Mortality.per.100k, data = MigUK)
# looking at this to check our linear model

cor(MigUK$Suicide.Mortality.per.100k, MigUK$Natural.Increase..Crude..per.1k)
#summary(Migration)

# Going to assume our most optimal linear model will have alot of variables so using backwards Elim

paste(InpVar, collapse = ' + ')
#using this to paste the lm stuff

MigUK.full <- lm(Total.decisions ~ Economic.Growth..from.prior.year. + Population + X..of.Famine + Maternal.Mortality..Deaths. + Maternal.Mortality.per.100k + Suicide.Mortality.per.100k + Life.Expectancy..Year. + X..Rural.Population + Death.Rate..Crude..per.1k + Birth.Rate..Crude..Per.1k + Natural.Increase..Crude..per.1k + X..Crop.Production.to..14.16 + Gov.Education.Budget....of. , data = MigUK)
MigUK.Empty <- lm(Total.decisions~ 1, data = MigUK)
#Migration.full <- lm(Total.decisions ~ Economic.Growth..from.prior.year. + Population + X..of.Famine + Maternal.Mortality..Deaths. + Maternal.Mortality.per.100k + Suicide.Mortality.per.100k + Life.Expectancy..Year. + X..Rural.Population + Death.Rate..Crude..per.1k + Birth.Rate..Crude..Per.1k + Natural.Increase..Crude..per.1k + X..Crop.Production.to..14.16 + Gov.Education.Budget....of. , data = Migration)

# idk how to do this properly in R without it being so ugly 

# Needed to remove Male Pop % & % on < $1.90 a day due to UK having the same % every year
step(MigUK.full, scale = summary(MigUK.full)$sigma^2)
#step(Migration.full, scale = summary(Migration.full)$sigma^2)

step(MigUK.Empty, direction = "forward", scale = summary(MigUK.Empty)$sigma^2, scope = list(lower = MigUK.Empty, upper = MigUK.full))

