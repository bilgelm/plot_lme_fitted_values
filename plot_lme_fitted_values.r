
library(nlme)
library(ggplot2)
library(repr)

# See: https://stats.idre.ucla.edu/r/examples/alda/r-applied-longitudinal-data-analysis-ch-4/

alcohol1 <- read.table("https://stats.idre.ucla.edu/stat/r/examples/alda/data/alcohol1_pp.txt", 
                       header=T, sep=",")

head(alcohol1)

alcohol1$cpeer[3] <- NA
alcohol1$alcuse[5] <- NA

model.g <- lme(alcuse ~ ccoa+cpeer*age_14 , data=alcohol1, random= ~ age_14 | id, 
               na.action=na.exclude)
summary(model.g)

table(alcohol1$age_14)

# expand.grid will create a data set with all possible combinations of the inputs
# this is the fake data set I will use for visualization
newdata <- expand.grid(age_14=c(0,2),
                       cpeer=0.7708544,
                       ccoa=unique(alcohol1$ccoa))

newdata

# I need to manually assign fake IDs
# Since I know the structure of my fake data set (that each ID has two rows),
# I can do this easily by repeating each fake ID twice
# To make sure that ID's are fake, I'm going to start numbering from the maximum ID in the original data set
newdata$id <- rep(1:(nrow(newdata)/2), each=2) + max(alcohol1$id)

newdata

newdata$alcuse_pred0 <- predict(model.g, newdata = newdata, level = 0)
# including `level=0` as an argument for predict means that we are not going to bring
# in random effects for prediction (we cannot, anyway, because the IDs we assigned to 
# the fake data points do not exist in the original data set)

options(repr.plot.width=5, repr.plot.height=3)
p <- ggplot(newdata, aes(x=age_14, y=alcuse_pred0, group=id, color=factor(ccoa))) +
     geom_line() + geom_point() +
     theme_bw() + ggtitle("Population-level (fixed) effects")
p

# this prediction incorporates fixed + random effects
alcohol1$alcuse_pred <- predict(model.g)


# this prediction is based on fixed effects only
alcohol1$alcuse_pred0 <- predict(model.g, level=0)

p <- ggplot(subset(alcohol1, !is.na(alcuse_pred)), 
            aes(x=age_14, y=alcuse_pred, group=id, color=factor(ccoa))) +
     geom_line() + geom_point() +
     theme_bw() + ggtitle("Individual fitted values based on fixed + random effects")
p

p <- ggplot(subset(alcohol1, !is.na(alcuse_pred0)), 
            aes(x=age_14, y=alcuse_pred0, group=id, color=factor(ccoa))) +
     geom_line() + geom_point() +
     theme_bw() + ggtitle("Individual fitted values based on fixed effects only")
p
