
### PACKAGES ###
# Affichage
library(knitr)

# Manipulation de bases de données
library(dplyr) 
library(data.table)

# Graphiques
library(ggplot2)
library(plotly)
library(RColorBrewer)

# Cartographie
library(sf)
library(mapsf)

### DONNEES
dt<-readRDS("chelem/data_chelem.RDS")

### CARTES
map<-readRDS("chelem/map_Chelem.RDS")
ctr<-readRDS("chelem/map_Chelem_ctr.RDS")


## (1) DONNEES




tabexp <- function(pays, periode){
  
  dt %>% filter(i==pays, t==periode) %>% 
    group_by(i,j, namei,namej) %>%
    summarise( Fij = sum(Fijkt)/1000,
               Mj = sum(Fjkt)/1000,
               DISTij = min(DISTij),
               CONTij = min(CONTij),
               COLOij = mean(COL1ij),
               LANGij = mean(LANGij)) %>%
    ungroup() %>%
    mutate(rnk = rank(-Fij)) %>%
    select(rnk,i,j,namei,namej,Fij,Mj,DISTij,CONTij,COLOij,LANGij) %>%
    arrange(rnk)
}

tab1<-tabexp("FRA", "2016-20")

tabimp <- function(pays, periode){
  
  dt %>% filter(j==pays, t==periode) %>% 
    group_by(i,j, namei,namej) %>%
    summarise( Fij = sum(Fijkt)/1000,
               Mi = sum(Fikt)/1000,
               DISTij = min(DISTij),
               CONTij = min(CONTij),
               COLOij = mean(COL1ij),
               LANGij = mean(LANGij)) %>%
    ungroup() %>%
    mutate(rnk = rank(-Fij)) %>%
    select(rnk,i,j,namei,namej,Fij,Mi,DISTij,CONTij,COLOij,LANGij) %>%
    arrange(rnk)
}

  
tab2<-tabimp("FRA", "2016-20")

tabvol <- function(pays, periode){
  
  dt %>% filter(j==pays, t==periode) %>% 
    group_by(i,j, namei,namej) %>%
    summarise( Vij = (sum(Fjikt) + sum(Fijkt))/1000,
               V = (sum(Fikt)+sum(Fjkt))/1000,
               DISTij = min(DISTij),
               CONTij = min(CONTij),
               COLOij = mean(COL1ij),
               LANGij = mean(LANGij)) %>%
    ungroup() %>%
    mutate(rnk = rank(-Vij)) %>%
    select(rnk,i,j,namei,namej,Vij,V,DISTij,CONTij,COLOij,LANGij) %>%
    arrange(rnk)
}


tab3<-tabexp("FRA", "2011-15")


mod <- glm(data=tab3,formula = Fij~log(Mj)+log(DISTij)+CONTij+COLOij+LANGij, family = "poisson")
summary(mod)

