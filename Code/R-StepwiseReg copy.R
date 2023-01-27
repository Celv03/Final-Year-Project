MigUK <- read.csv(file = "/Users/kelvin/Desktop/Maths/Final Year Project/AD-OutOnly.csv")
# Change csv to ukraine for the ukraine details

MigUK <- MigUK[MigUK$Country.of.origin %in% c('France', 'Germamy', 'Netherlands', 'Spain', 'Switzerland', 'United Kingdom of Great Britan and Northern Ireland'), ]

head(MigUK)
cols <- colnames(MigUK)

cols[32]
#Using this to find col names

InpVar <- cols[4:18]
# Get our input variables, response variables to get feature var

MigUK$Economic.Growth..from.prior.year.
MigUK$Total.decisions
MigUK$Maternal.Mortality.per.100k
MigUK$X..Rural.Population

par(mfrow=c(2,2))

plot(Total.decisions ~ Maternal.Mortality.per.100k, data = MigUK)
plot(Total.decisions ~ X..Rural.Population, data = MigUK)
plot(Total.decisions ~ Life.Expectancy..Year., data = MigUK)
plot(Total.decisions ~ Natural.Increase..Crude..per.1k, data = MigUK)

plot(Total.decisions ~ Death.Rate..Crude..per.1k, data = MigUK)
plot(Total.decisions ~ Population, data = MigUK)

# looking at this to check our linear model

# Going to assume our most optimal linear model will have alot of variables so using backwards Elim

paste(InpVar, collapse = ' + ')
#using this to paste the lm stuff

MigUK.full <- lm(Total.decisions ~   Year + Economic.Growth..from.prior.year. + Maternal.Mortality..Deaths. + Maternal.Mortality.per.100k + Suicide.Mortality.per.100k + Life.Expectancy..Year. + X..Rural.Population + Death.Rate..Crude..per.1k + Birth.Rate..Crude..Per.1k + Natural.Increase..Crude..per.1k + X..Crop.Production.to..14.16 + Gov.Education.Budget....of. , data = MigUK)
MigUK.Empty <- lm(Total.decisions~ 1, data = MigUK)
# idk how to do this properly in R without it being so ugly 
# Needed to remove Male Pop % & % on < $1.90 a day due to UK having the same % every year

step(MigUK.full, scale = summary(MigUK.full)$sigma^2)
#step(Migration.full, scale = summary(Migration.full)$sigma^2)

step(MigUK.Empty, direction = "forward", scale = summary(MigUK.Empty)$sigma^2, scope = list(lower = MigUK.Empty, upper = MigUK.full))


