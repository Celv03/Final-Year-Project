Migdata <- read.csv(file = "/Users/kelvin/Desktop/Maths/Final Year Project/Pop Diff - Czechia.csv")
# Change csv to ukraine for the ukraine details

Migdt = Migdata[!(Migdata$Country.of.asylum %in% c('Germany')),]
MigGer = Migdata[(Migdata$Country.of.asylum %in% c('Germany')),]

Migdt = Migdt[!(Migdt$Year %in% c('2020', '2021')),]
# do not have full data & covid

head(Migdt)
cols <- colnames(Migdt)
cols[68]
#Using this to find col names

InpVar <- cols[4:68]
# Get our input variables, response variables to get feature var

par(mfrow=c(2,2))

plot(Decisions.Per.100k ~ Maternal.Mortality..Deaths....A, data = Migdt)
plot(Decisions.Per.100k ~ X....1.90.a.day...A, data = Migdt)
plot(Decisions.Per.100k ~ Life.Expectancy..Year....A, data = Migdt)
plot(Decisions.Per.100k ~ Natural.Increase..Crude..per.1k...A, data = Migdt)

plot(Decisions.Per.100k ~ Death.Rate..Crude..per.1k...A, data = Migdt)
plot(Decisions.Per.100k ~ Population...A, data = Migdt)
plot(Decisions.Per.100k ~ Death.Rate..Crude..per.1k...O, data = Migdt)
plot(Decisions.Per.100k ~ GDP.LCU..T..Difference, data = Migdt)


# looking at this to check our linear model

# Going to assume our most optimal linear model will have alot of variables so using backwards Elim

paste(InpVar, collapse = ' + ')
#using this to paste the lm stuff

MigUK.full <- lm(Decisions.Per.100k ~  Land.Locked.Developing.Countries..LLDC....O + Small.Island.Developing.States..SIDS....O + Economic.Growth..from.prior.year....O + Population...O + GPD.LCU..Trillions....O + X....1.90.a.day...O + X..of.Famine...O + Maternal.Mortality..Deaths....O + Maternal.Mortality.per.100k...O + Suicide.Mortality.per.100k...O + Life.Expectancy..Year....O + X..Male..Pop....O + X..Rural.Population...O + Death.Rate..Crude..per.1k...O + Birth.Rate..Crude..Per.1k..O + Natural.Increase..Crude..per.1k...O + X..Crop.Production.to..14.16...O + Gov.Education.Budget....of....O + Country.of.asylum + Country.of.asylum..ISO. + Continent.of.asylum + Least.Developed.Countries..LDC....A + Land.Locked.Developing.Countries..LLDC....A + Small.Island.Developing.States..SIDS....A + Economic.Growth..from.prior.year....A + Population...A + GPD.LCU..Trillions....A + X....1.90.a.day...A + X..of.Famine...A + Maternal.Mortality..Deaths....A + Maternal.Mortality.per.100k...A + Suicide.Mortality.per.100k...A + Life.Expectancy..Year....A + X..Male..Pop....A + X..Rural.Population...A + Death.Rate..Crude..per.1k...A + Birth.Rate..Crude..Per.1k...A + Natural.Increase..Crude..per.1k...A + X..Crop.Production.to..14.16...A + Gov.Education.Budget....of....A + Continent.Move + LDC.to.LDC + LDC.to.MDC + MDC.to.LDC + MDC.to.MDC + To.LDC + To.MDC + Economic.Growth.difference + Population...Difference + GDP.LCU..T..Difference + X....1.90.a.day...Difference + X..of.Famine...Difference + Maternal.Mortality..Deaths....Difference + Maternal.Mortality.per.100k...Difference + Suicide.Mortality.per.100k...Difference + Life.Expectancy..Year....Difference + X..Male..Pop....Difference + X..Rural.Population...Difference + Death.Rate..Crude..per.1k...Difference + Birth.Rate..Crude..Per.1k...Difference + Natural.Increase..Crude..per.1k...Difference + X..Crop.Production.to..14.16...Difference + Gov.Education.Budget....of....Difference , data = Migdt)
MigUK.Empty <- lm(Decisions.Per.100k~ 1, data = Migdt)
# idk how to do this properly in R without it being so ugly 
# Needed to remove Male Pop % & % on < $1.90 a day due to UK having the same % every year

step(MigUK.full, scale = summary(MigUK.full)$sigma^2)
#step(Migration.full, scale = summary(Migration.full)$sigma^2)

step(MigUK.Empty, direction = "forward", scale = summary(MigUK.Empty)$sigma^2, scope = list(lower = MigUK.Empty, upper = MigUK.full))

upmodel <- lm(formula = Decisions.Per.100k ~ Maternal.Mortality..Deaths....O + 
                Maternal.Mortality.per.100k...O + Suicide.Mortality.per.100k...O + 
                Life.Expectancy..Year....O + X..Rural.Population...O + Death.Rate..Crude..per.1k...O + 
                Birth.Rate..Crude..Per.1k..O + Country.of.asylum + Economic.Growth..from.prior.year....A + 
                Population...A + Suicide.Mortality.per.100k...A + Birth.Rate..Crude..Per.1k...A + 
                X..Crop.Production.to..14.16...A + Gov.Education.Budget....of....A + 
                Economic.Growth.difference + Population...Difference + GDP.LCU..T..Difference + 
                Maternal.Mortality.per.100k...Difference + X..Male..Pop....Difference + 
                Birth.Rate..Crude..Per.1k...Difference, data = Migdt)

lomodel <- lm(formula = Decisions.Per.100k ~ Country.of.asylum + Life.Expectancy..Year....O + 
                GDP.LCU..T..Difference + Life.Expectancy..Year....A + X..Male..Pop....Difference + 
                Gov.Education.Budget....of....A + Population...A, data = Migdt) #looks good enough will use this
summary(lomodel)

summary(upmodel)

plot(residuals(upmodel))
plot(residuals(lomodel))

which(residuals(upmodel) > 1000)

Migdt[296,]$Country.of.asylum

