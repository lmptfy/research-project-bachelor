# Packages			------------------------------------------

install.packages('dplyr')
install.packages('magrittr')
install.packages('ggplot2')
install.packages('stargazer')
install.packages('psych')
install.packages('ggpubr')
install.packages('plm')
library(dplyr)
library(magrittr)
library(ggplot2)
library(stargazer)
library(psych)
library("ggpubr")
library("plm")

# Importation des données	------------------------------------------

data <- read.csv("~/Desktop/commitment/pdr/new/R/data_ww_2019.csv") %>%
          filter( !is.na(year)) %>%  group_by %>%
            mutate( ann_prod = as.numeric( ann_prod),
                    ta_price = as.numeric(ta_price),
                    country  = as.factor(country),
                    xmp_gdp = as.factor(xmp_gdp)
)


# Plot  scatter 1		------------------------------------------

# plot(data)

# Scatterplot
  pairs.panels(data[,c('country','ann_prod','coc','ta_price')],
               method = "pearson", # correlation method
               hist.col = "#00AFBB",
               density = TRUE,  # show density plots
               ellipses = TRUE # show correlation ellipses
  )

# Plot  scatter 2		------------------------------------------

# Correlation panel
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch = 19, col = data$country)
}
# Create the plots
pairs(data[,c('country','ann_prod','coc','xmp_gdp','ta_price')],
      lower.panel = panel.cor,
      upper.panel = upper.panel)


# Plot  scatter 3		------------------------------------------

# Correlation between IV
      panel.cor <- function(x, y){
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        r <- round(cor(x, y), digits=2)
        txt <- paste0("R = ", r)
        cex.cor <- 0.8/strwidth(txt)
        text(0.5, 0.5, txt, cex = cex.cor * r)
      }
      # Customize upper panel
      upper.panel<-function(x, y){
        points(x,y, pch = 19, col = data$country)
      }
      # Create the plots
      pairs(data[,c('coc','ps','rol')],
            lower.panel = panel.cor,
            upper.panel = upper.panel)



# Series temporelles	------------------------------------------
# Production annuelle
  data %>%
    ggplot(aes(year ,ann_prod))+
    geom_line(aes(color=country))+
    labs(color="Legend")+
    ggtitle( 'Évolution de la production annuelle du Ta: 2002-14') +
    labs(y = "Production annuelle", x="Temps") +
    theme(plot.title = element_text(color = "black", size = 12, hjust = 0.5),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(linetype = "dashed", colour = "#F0F0F0"),
          axis.text = element_text(colour = "black"),
          strip.text.x = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          axis.text.x  = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          axis.text.y  = element_text(size = 10))


# Corruption
  data %>%
    ggplot(aes(year ,coc))+
    geom_line(aes(color=country ))+
    labs(color="Legend") +
    ggtitle( 'Évolution de la corruption par pays: 2002-14') +
    labs(y = "Corruption", x="Année") +
    theme(plot.title = element_text(color = "black", size = 12, hjust = 0.5),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(linetype = "dashed", colour = "#F0F0F0"),
          axis.text = element_text(colour = "black"),
          strip.text.x = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          axis.text.x  = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          axis.text.y  = element_text(size = 10))


# Correlation			------------------------------------------

# chart - scatter plot coc & ann_prod

  data %>%
    ggplot(aes(x = coc, y = ann_prod , fill = country, color = country )) +
    geom_point() +
    # geom_abline(intercept = -5, slope = 10, col = "red") +
    labs(x = "Corruption", y="Production annuelle") +
    theme(plot.title = element_text(color = "black", size = 12, hjust = 0.5),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(linetype = "dashed", colour = "#F0F0F0"),
          axis.text = element_text(colour = "black"),
          strip.text.x = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          axis.text.x  = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          axis.text.y  = element_text(size = 10))



# correlation entre corruption et production annuelle	  --------------
  x <- data$coc
  y <- data$ann_prod

    cor(x,  y, method="pearson" )
    cor(x,  y, method= "kendall")
    cor(x,  y, method="spearman")

    cor.test(x, y, method="pearson" )
    cor.test(x, y, method= "kendall")
    cor.test(x, y, method="spearman")

# graph pearson
    ggscatter(data, x = "coc", y = "ann_prod",
              add = "reg.line", conf.int = TRUE,
              cor.coef = TRUE, cor.method = "pearson",
              xlab = "Corruption", ylab = "Production annuelle")

# graph kendall
    ggscatter(data, x = "coc", y = "ann_prod",
              add = "reg.line", conf.int = TRUE,
              cor.coef = TRUE, cor.method = "kendall",
              xlab = "Corruption", ylab = "Production annuelle")

# graph spearman
    ggscatter(data, x = "coc", y = "ann_prod",
              add = "reg.line", conf.int = TRUE,
              cor.coef = TRUE, cor.method = "spearman",
              xlab = "Corruption", ylab = "Production annuelle")

# Regression			------------------------------------------
# Regression fixed effect
grun.fe <- plm(ann_prod~coc, data=data, model= "within")
summary(grun.fe)

# Regression random effect
grun.re <- plm(ann_prod~coc, data=data, model= "random")
summary(grun.re)

# Regression between effect
grun.between <- plm(ann_prod~coc, data=data, model= "between")
summary(grun.between)

# Regression pooling effect
grun.pooling <- plm(ann_prod~coc, data=data, model= "pooling")
summary(grun.pooling)

# Regression first-difference effect
grun.fd <- plm(ann_prod~coc, data=data, model= "fd")
summary(grun.fd)

# Regression lineaire 1
  lm1 <- lm(ann_prod  ~  coc, data = data)
  summary(lm1 , type = "latex", title = "", style = "default", )

# diagnostic plots régression linéaire 1
  par(mfrow = c(2,2))
  plot(lm1)

# Regression lineaire 2
  lm2 <- lm(ann_prod  ~  coc + ta_price, data = data)
  summary(lm2 , type = "latex", title = "", style = "default", )

# diagnostic plots régression linéaire 2
  par(mfrow = c(2,2))
  plot(lm2)

# presentation of the results for report
  type = 'html' # type = 'text'
  style = 'io'
  r <- 3

  stargazer(lm1, lm2, grun.fe, grun.re, grun.between, grun.pooling, grun.fd,
             	  type = type , style = style,
    		  title = paste('Regressions'),
    		  dep.var.labels = c('LM1','LM2','FE','RE','BTW','POO','FD'),
   		  multicolumn = FALSE, object.names = F, initial.zero = FALSE,
    		  dep.var.caption  = "<b> Nome de la variabel dpendente </b>")