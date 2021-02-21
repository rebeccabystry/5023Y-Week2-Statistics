library(tidyverse)
library(skimr)

darwin <- read_csv("Data/darwin.csv")

str(darwin)
view(darwin)
skim(darwin)
unique(darwin)

darwin <- darwin %>% 
  pivot_longer(cols=c("Cross":"Self"),
             names_to="type",
             values_to="height")

view(darwin)

darwin %>% 
  ggplot(aes(x=type,y=height)) +
  geom_jitter(width=0.1, pch=21, aes(fill=type)) +
  theme_classic() +
  geom_abline(intercept=18.88, slope=0, linetype="dashed")

darwin_lm <- lm(height ~ 1, data=darwin)
# Here, the ~1 means we don't have an independent variable. We are just comparing the means of the "height" variable for each of the pollination types.

darwin_lm

darwin %>% summarise(mean=mean(height))

# Making model2

model2 <- lm(height~type, data = darwin)
# The ~type means we are using type as an independent variable which is affecting the dependent variable height. 

model2

summary(model2)

# Running an ANOVA on the same data. ANOVA tests are also linear models (?)

library(rstatix)

anova_test(height~type, data = darwin)

help(pf)
# Will tell us the probability of observing our "signal to noise" ratio:

pf(q=5.9395, df1=1, df2=28, lower.tail=FALSE)

# WRITE-UP RESULTS

# Plants which were self-pollinated were significantly shorted than plants which were pollinated by other plants (cross-pollinated).  