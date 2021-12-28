rm(list = ls()) # Erase memory
if(class(dev.list()) == "integer") dev.off(dev.list()["RStudioGD"]); # close plots

#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--
#
#   Code pour William
#
#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--



# Package -----------------------------------------------------------------

library(dplyr)
library(magrittr)
library(ggplot2)

# Importation des donn??es  ------------------------------------------------

data <- read.csv('~:/Users/williamwegener⁩/Desktop/commitment⁩/pdr/new/R/data_ww_2019.csv') %>%
            mutate( ann_prod = as.numeric( ann_prod),
                    ta_price = as.numeric(ta_price),
                    country  = as.categorial(country),
                    xmp_gdp = as.factor(xmp_gdp)
)


# Plot ----------------------------------------------------------------

plot(data)



df1<-data.frame(x=1:10,y=rnorm(10))
df2<-data.frame(x=1:10,y=rnorm(10))

data %>%
ggplot(aes(year, ann_prod))+
  geom_line(aes(color=country ))+
  labs(color="Legend text")



# Correlation  ------------------------------------------------------------


# entre corruption et production annuelle

cor(data$coc,data$ann_prod)

data %>%
  ggplot(aes(x = coc, y = ann_prod)) +
  geom_point() +
  geom_abline(intercept = -5, slope = 10, col = "red")


# R??gression  -------------------------------------------------------------

# regression lin??aire
# variable d??pendente production annuelle
# variable ind??pendente corruption + ta price


lm1 <- lm(ann_prod  ~  coc + ta_price + country , data = data)
summary(lm1)

par(mfrow = c(2,2))
plot(lm1)
