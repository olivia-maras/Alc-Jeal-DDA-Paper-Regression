install.packages("tidyverse")
install.packages("rio")
install.packages("olsrr")
install.packages("broom")
install.packages("reghelper")
install.packages("sjPlot")
install.packages("ggplot2")
install.packages("devtools")
install.packages("rockchalk")
install.packages("interactions")
install.packages("sandwich")
install.packages("GGally")
install.packages("stargazer")
install.packages("ggfortify")
install.packages("effects")
install.packages("nloptr")
install.packages("psych")
library(nloptr) #for plotting simple slopts (Way 1)
library(effects)
library(ggfortify)
library(stargazer)
library(GGally) # for plotting data
library(tidyverse) # for plotting and data wrangling
library(rio) # for importing data
library(psych) # for descriptives
library(olsrr) # for diagnostics
library(broom) # for tidying model output
library(reghelper) # for calculating simple slopes (Way 1)
library(sjPlot) # for plotting simple slopes
library(ggplot2) # for graphing
library(sjmisc) # for simple slopes (Way 1)
library(interactions) #for plotting simple slopes (Way 2)

#read in data (SPSS Files)
library(haven)
finaldata <- read_sav('/Users/oliviamaras/Documents/ASU/@HEART Lab/1st Year Paper Project/Dating experience .sav')

#Selecting a subset of variables to make dataset smaller to only the variables in your model
#(adding c in front simplifies them to numeric vector, which is important for simple slopes)
mynewvars <- c("EIPmean", "SUBUSEm", "JEALINDm", "age_year", "datepref", "race7_f", "Ycurdate", "GendDum")
finaldata2 <- finaldata[mynewvars]

#Check how many participants have missing data, specify the rows you want to look at
sum(rowSums(is.na(finaldata2[,1:8])) > 0)

# create new dataset without missing data (listwise deletion)
cleandata <- na.omit(finaldata2)

#doublecheck the missing data is gone
sum(is.na(cleandata))

#Mean Center IVs one way, again the c is important to change to numeric from matrix
cleandata$Jealmc <- c(scale(cleandata$JEALINDm, center=T, scale=F))
cleandata$AlcUsemc <- c(scale(cleandata$SUBUSEm, center=T, scale=F))

#Another way to mean center IVs
cleandata <- cleandata %>% 
  mutate(Jealmc2 = as.numeric(scale(JEALINDm, center = TRUE, scale = FALSE)),
         AlcUsemc2 = as.numeric(scale(SUBUSEm, center = TRUE, scale = FALSE)))

#Look at descriptives 
describe(cleandata)

#Transform DV if necessary (if skew and kurtosis are high)
cleandata$EIPlog <- log(cleandata$EIPmean)

#Look at DV descriptives again to check skew/kurtosis
describe(cleandata$EIPlog)

#Run Regression
Reg1 <- lm(EIPlog ~ age_year + race7_f + datepref + Ycurdate + GendDum +Jealmc + AlcUsemc, data=cleandata)

#Include interaction - important: moderator should go second when multiplying
Reg2 <- lm(EIPlog ~ age_year + race7_f + datepref + Ycurdate + GendDum + AlcUsemc*Jealmc, data=cleandata)

#regression using only predictor and moderator, for simple slopes way #1
Reg3 <- lm(EIPlog ~ AlcUsemc*Jealmc, data=cleandata)

#look at results
summary(Reg1)
summary(Reg2)
summary(Reg3)

# R-Squared Change Between Reg 1 and Reg 2 ###############################
anova(Reg1, Reg2)

#Simple slopes Way 1
library(sjPlot)
library(sjmisc)
library(ggplot2)
plot_model(model = Reg3, # model fit object
           type = "int", # interaction 
           mdrt.values = "meansd") # which values of the moderator variable to use
simple_slopes(model = Reg3)
simple_slopes(Reg3) %>% 
  filter(AlcUsemc == "sstest")

#Simple slopes Way 2
library(interactions)
library(sandwich)
alcreg <- lm(EIPlog ~ AlcUsemc * Jealmc, data = cleandata)
sim_slopes(alcreg, pred = AlcUsemc, modx = Jealmc, jnplot = TRUE)
interact_plot(alcreg, pred = AlcUsemc, modx = Jealmc, interval = TRUE)

