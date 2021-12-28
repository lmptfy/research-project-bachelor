rm(list = ls()) # Erase memory
if(class(dev.list()) == "integer") dev.off(dev.list()["RStudioGD"]); # close plots

#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--
#
#   Code pour William
#
#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--

# Package                  -----------------------------------------------------------------

install.packages('dplyr')
install.packages('magrittr')
install.packages('ggplot2')
install.packages('stargazer')
install.packages('psych')
install.packages('ggpubr')
library(dplyr)
library(magrittr)
library(ggplot2)
library(stargazer)
library(psych)
library("ggpubr")

# Importation des donnees  ------------------------------------------------

data <- read.csv("~/Desktop/commitment/pdr/new/R/data_ww_2019.csv") %>%
          filter( !is.na(year)) %>%  group_by %>%
            mutate( ann_prod = as.numeric( ann_prod),
                    ta_price = as.numeric(ta_price),
                    country  = as.factor(country),
                    xmp_gdp = as.factor(xmp_gdp)
)


# Plot  scatter 1          ----------------------------------------------------------------

# plot(data)

# Scatterplot
  pairs.panels(data[,c('country','ann_prod','coc','ta_price')],
               method = "pearson", # correlation method
               hist.col = "#00AFBB",
               density = TRUE,  # show density plots
               ellipses = TRUE # show correlation ellipses
  )

# Plot  scatter 2          ----------------------------------------------------------------

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


      # Plot  scatter 3          ----------------------------------------------------------------

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



# Series temporelles       --------------------------------------------------------

# Production annuelle
  data %>%
    ggplot(aes(year ,ann_prod))+
    geom_line(aes(color=country))+
    labs(color="Legend")+
    ggtitle( 'Titre de ton graphique: Prod annuelle') +
    labs(y = "Production Annuelle", x="Temps") +
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


# Correlation              ------------------------------------------------------------

# scatter plot entre deux variables

  # chart
  data %>%
    ggplot(aes(x = coc, y = ann_prod , fill = country, color = country )) +
    geom_point() +
    # geom_abline(intercept = -5, slope = 10, col = "red") +
    labs(y = "Corruption", x="Production annuelle") +
    theme(plot.title = element_text(color = "black", size = 12, hjust = 0.5),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(linetype = "dashed", colour = "#F0F0F0"),
          axis.text = element_text(colour = "black"),
          strip.text.x = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          axis.text.x  = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          axis.text.y  = element_text(size = 10))



# correlation entre corruption et production annuelle
  x <- data$coc
  y <- data$ann_prod

    cor(x,  y, method="pearson" )
    # cor(x,  y, method= "kendall")
    # cor(x,  y, method="spearman")

    cor.test(x, y, method="pearson" )
    # cor.test(x, y, method= "kendall")
    # cor.test(x, y, method="spearman")

# graph pearson
    ggscatter(data, x = "coc", y = "ann_prod",
              add = "reg.line", conf.int = TRUE,
              cor.coef = TRUE, cor.method = "pearson",
              xlab = "Corruption", ylab = "Production annuelle")

# graph kendall
    ggscatter(data, x = "coc", y = "ann_prod",
              add = "reg.line", conf.int = TRUE,
              cor.coef = TRUE, cor.method = "kendall",
              xlab = "Corruption", ylab = "production annuelle")

# graph spearman
    ggscatter(data, x = "coc", y = "ann_prod",
              add = "reg.line", conf.int = TRUE,
              cor.coef = TRUE, cor.method = "spearman",
              xlab = "Corruption", ylab = "production annuelle")

# Regression               -------------------------------------------------------------

# Regression lineaire
  lm1 <- lm(ann_prod  ~  coc + ta_price + country + year , data = data)
  summary(lm1 , type = "latex", title = "", style = "default", )
  par(mfrow = c(2,2))
  plot(lm1)

# presentation of the results for report
  type = 'html' # type = 'text'
  style = 'io'
  r <- 3

  stargazer(lm1,
            type = type , style = style,
            title = paste('Regressions'),
            dep.var.labels   = c('premier','deuxieme','troisieme'),
            multicolumn = FALSE, object.names = F, initial.zero = FALSE,
            dep.var.caption  = "<b> Nome de la variabel d�pendente </b>")





 # Regression               -------------------------------------------------------------

 # Regression lineaire
      lm1 <- lm(ann_prod  ~  coc + ta_price + xmp_gdp, data = data)
      summary(lm1 , type = "latex", title = "heyy titre", style = "default", )
      par(mfrow = c(2,2))
      plot(lm1)

  # presentation of the results for report
      type = 'html' # type = 'text'
      style = 'io'
      r <- 3

  stargazer(lm1,
      type = type , style = style,
      title = paste('Regressions'),
      dep.var.labels   = c('premier','deuxieme','troisieme'),
      multicolumn = FALSE, object.names = F, initial.zero = FALSE,
      dep.var.caption  = "<b> Nome de la variabel d�pendente </b>")











# copie le r�sultat dans la console (code html)
# ouvre un nouveau fichier R markdown
# efface tout le code sauf l'entete
# colle le r�sultat html
# lance le code (Ctrl + Shift + K )
# copie colle le r�sultat dans ton raport


 stargazer( fit.WHOREP, fit.TPST,   fit.FOOD, fit.INFCOM,  fit.FIN, fit.REA,
            type = type , style = style,
            title = paste('Inward - regression by sector (1)'),
            multicolumn = FALSE, object.names = F, initial.zero = FALSE,
            dep.var.caption  = "<b> Foreign affiliate sales </b>",

            p =   list(
            grun.re[,"Pr(>|z|)"],
            grun.fe[,"Pr(>|z|)"],
            grun.between[,"Pr(>|z|)"],
            grun.pooling[,"Pr(>|z|)"],
            grun.fd[,"Pr(>|z|)"],
            lm1[,"Pr(>|z|)"],
            lm2[,"Pr(>|z|)"],
            ),
            se =  list(
              fitr.WHOREP[,"Pr(>|z|)"],
              fitr.TPST[,"Pr(>|z|)"],
              fitr.FOOD[,"Pr(>|z|)"],
              fitr.INFCOM[,"Pr(>|z|)"],
              fitr.FIN[,"Pr(>|z|)"],
              fitr.REA[,"Pr(>|z|)"]
            ),
            p =   list(
              fitr.WHOREP[,"Pr(>|z|)"],
              fitr.TPST[,"Pr(>|z|)"],
              fitr.FOOD[,"Pr(>|z|)"],
              fitr.INFCOM[,"Pr(>|z|)"],
              fitr.FIN[,"Pr(>|z|)"],
              fitr.REA[,"Pr(>|z|)"]
            ),

            omit = c('iso3_o','iso3_d'),

            intercept.bottom = FALSE,
            dep.var.labels   = c('WHOREP','TPST','FOOD','INFCOM','FIN','REA'),
            single.row = FALSE,
            notes.label = "<em> Notes : <em>",
            notes.align = "r",
            notes.append = FALSE,
            notes = " <em>  Standard errors are robust for ppml  <br>  p-values in parenthesis <br> * p<.1, ** p<.05, ***<.01 <em>",
            digits = 3, digits.extra  = 3,
            omit.stat = c("f","ser"), nobs = F,
            column.sep.width = '7pt',

            add.lines = list(
              c('R2',           round(stat.WHOREP$R2,r), round(stat.TPST$R2,r), round(stat.FOOD$R2,r), round(stat.INFCOM$R2,r)    , round(stat.FIN$R2,r), round(stat.REA$R2,r)),
              c(' Zeros',      round(stat.WHOREP$Z,r) , round(stat.TPST$Z,r) , round(stat.FOOD$Z,r), round(stat.INFCOM$Z,r)      , round(stat.FIN$Z,r) , round(stat.REA$Z,r)),
              c(' of mirror',  round(stat.WHOREP$M,r) , round(stat.TPST$M,r) , round(stat.FOOD$M,r), round(stat.INFCOM$M,r)      , round(stat.FIN$M,r) , round(stat.REA$M,r))
            )
 )
