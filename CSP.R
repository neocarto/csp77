library("sf")
library("readxl")
library("cartography")
library("mapview")
library("ggplot2")
library('reshape2')

setwd("/home/nlambert/Documents/R/Carto_77")

# Geométries #############################################

com <- st_read(dsn = "sources/IGN/COMMUNE.shp", stringsAsFactors = F)
com77 <- com[com$CODE_DEPT == 77,]
com77 <- com77[,c("INSEE_COM","NOM_COM","geometry")]
com77 <- st_transform(com77, 2154 )
# st_write(com77, dsn = "data", layer = "com77.shp", driver = "ESRI Shapefile")

# Données #############################################

getdata <- function(sheet){
year <- substr(sheet, 5, 8)
x <- read_excel("sources/INSEE/pop-act2554-csp-cd-6814.xls", sheet = sheet, skip = 15)
x <- x[data.frame(x)$DR == 77,]
x$INSEE_COM <- paste0(x$DR, x$CR)
x <- x[,-c(1:6)]
head(data.frame(x))
# x <- merge(x, csp77, by="INSEE_COM", all.x=TRUE)

cs <- c("agr1", "agr0", "art1", "art0", "cad1", "cad0", "int1","int0", 
        "emp1", "emp0","ouv1", "ouv0")
names(x)[1:12] <- cs

# agregate field (employed + unemployed)
x[,paste0("agr",year)] <- x$agr0 + x$agr1
x[,paste0("art",year)]  <- x$art0 + x$art1
x[,paste0("cad",year)]  <- x$cad0 + x$cad1
x[,paste0("int",year)]  <- x$int0 + x$int1
x[,paste0("emp",year)]  <- x$emp0 + x$emp1
x[,paste0("ouv",year)]  <- x$ouv0 + x$ouv1
x[,paste0("act",year)]  <- x[,paste0("agr",year)]  + x[,paste0("art",year)] + x[,paste0("cad",year)] + x[,paste0("int",year)] + x[,paste0("emp",year)]  + x[,paste0("ouv",year)]
x  <- x[,13:20]
return(x)
}

csp77_1968 <- getdata(sheet = "COM_1968")
csp77_1975 <- getdata(sheet = "COM_1975")
csp77_1982 <- getdata(sheet = "COM_1982")
csp77_1990 <- getdata(sheet = "COM_1990")
csp77_1999 <- getdata(sheet = "COM_1999")
csp77_2009 <- getdata(sheet = "COM_2009")
csp77_2014 <- getdata(sheet = "COM_2014")


# Merge #############################################

com77 <- merge(com77, csp77_1968, by="INSEE_COM", all.x=TRUE)
com77 <- merge(com77, csp77_1975, by="INSEE_COM", all.x=TRUE)
com77 <- merge(com77, csp77_1982, by="INSEE_COM", all.x=TRUE)
com77 <- merge(com77, csp77_1990, by="INSEE_COM", all.x=TRUE)
com77 <- merge(com77, csp77_1999, by="INSEE_COM", all.x=TRUE)
com77 <- merge(com77, csp77_2009, by="INSEE_COM", all.x=TRUE)
com77 <- merge(com77, csp77_2014, by="INSEE_COM", all.x=TRUE)



# COULEURS #############################################

ouv.pal <- "red.pal"
emp.pal <- "orange.pal"
agr.pal <- "brown.pal"
cad.pal <- "blue.pal"
int.pal <- "purple.pal"
art.pal <- "green.pal"

# FONCTIONS #############################################

col_ouv <- carto.pal(pal1 = "red.pal",n1= 1)
col_emp <- carto.pal(pal1 = "orange.pal",n1= 1)
col_agr <- carto.pal(pal1 = "brown.pal",n1= 1)
col_cad <- carto.pal(pal1 = "blue.pal",n1= 1)
col_int <- carto.pal(pal1 = "purple.pal",n1= 1)
col_art <- carto.pal(pal1 = "green.pal",n1= 1)

plotCircles <- function(var, col, title, titleleg, fixmax,legend.pos, inches){
  plot(st_geometry(com77), col="#d8cac5", border="ivory")
  propSymbolsLayer(com77, var = var, col=col, inches = inches, 
                   border = "white", lwd=0.7,
                   fixmax = fixmax,legend.pos = legend.pos, 
                   legend.title.txt = titleleg)
  barscale(size = 10)
  north(pos = "topleft", col = "#706f6f")
  layoutLayer(title = title, 
              sources = "Insee, 2018", author = "Nicolas LAMBERT, 2018", 
              col = "#706f6f", coltitle = "white", 
              frame = TRUE, scale = NULL, north = FALSE)
  top <- sort(data.frame(com77)[,var], decreasing = TRUE)
  labelLayer(x = com77[data.frame(com77)[,var] %in% top[1:15],], txt = "NOM_COM", halo=TRUE, cex = 0.6, col= "#000000", bg = "#FFFFFF50", overlap = FALSE) 
}

plotChoro <- function(var, col, title, titleleg, bks, legend.pos){
  choroLayer(x = com77, 
             var = var, breaks = bks,
             col = cols,
             border = "white",
             add = FALSE,
             legend.pos = "topright",
             legend.title.txt = titleleg,
             legend.values.rnd = 2,lwd = 0.3)  
  barscale(size = 10)
  north(pos = "topleft", col = "#706f6f")
  layoutLayer(title = title, 
              sources = "Insee, 2018", author = "Nicolas LAMBERT, 2018", 
              col = "#706f6f", coltitle = "white", 
              frame = TRUE, scale = NULL, north = FALSE)
  top <- sort(data.frame(com77)[,var], decreasing = TRUE)
  labelLayer(x = com77[data.frame(com77)[,var] %in% top[1:15],], txt = "NOM_COM", halo=TRUE, cex = 0.6, col= "#000000", bg = "#FFFFFF50", overlap = FALSE) 
}

names (com77)
sum(com77ù)


# Premieres cartes #############################################

# Les stocks d'ouvriers

exp_dim <- getFigDim(x = com77, width = 1000, mar = c(0.2,0.2,1.4,0.2), res = 100)
png(filename = "maps/ouvriers_by_years.png", width = exp_dim[1], height = exp_dim[2]/2, res = 100)
par(mar=c(0.2,0.2,1.4,0.2), bg="grey95", mfrow=c(1,2))

plotCircles(var = "ouv1968", 
            col = col_ouv, 
            title = "Les ouvriers en Seine-et-Marne (1968)", 
            titleleg = "Nombre de d'ouvriers\nagés de 25 à 54 ans",
            fixmax=max(com77$ouv1968),
            inches = 0.3,
            legend.pos="topright")
plotCircles(var = "ouv2014", 
            col = col_ouv, 
            title = "Les ouvriers en Seine-et-Marne (2014)", 
            titleleg = "Nombre de d'ouvriers\nagés de 25 à 54 ans",
            fixmax=max(com77$ouv1968),
            inches = 0.3,
            legend.pos="topright")
dev.off()

# Les stocks d'employés

exp_dim <- getFigDim(x = com77, width = 1000, mar = c(0.2,0.2,1.4,0.2), res = 100)
png(filename = "maps/employés_by_years.png", width = exp_dim[1], height = exp_dim[2]/2, res = 100)
par(mar=c(0.2,0.2,1.4,0.2), bg="grey95", mfrow=c(1,2))

plotCircles(var = "emp1968", 
            col = col_emp, 
            title = "Les employés en Seine-et-Marne (1968)", 
            titleleg = "Nombre de d'employés\nagés de 25 à 54 ans",
            fixmax=max(com77$emp1968),
            inches = 0.3,
            legend.pos="topright")
plotCircles(var = "emp2014", 
            col = col_emp, 
            title = "Les employés en Seine-et-Marne (2014)", 
            titleleg = "Nombre de d'employés\nagés de 25 à 54 ans",
            fixmax=max(com77$emp1968),
            inches = 0.3,
            legend.pos="topright")
dev.off()

# Les CSP en 2014


exp_dim <- getFigDim(x = com77, width = 2000, mar = c(0.2,0.2,1.4,0.2), res = 100)
png(filename = "maps/CSP2014.png", width = exp_dim[1]/2, height = exp_dim[2]/3, res = 100)
par(mar=c(0.2,0.2,1.4,0.2), bg="grey95", mfrow=c(2,3))
k <- 100
inches <- 0.04
plotCircles(var = "agr2014", 
            col = col_agr, 
            title = "1 - Agriculteurs exploitants (2014)", 
            titleleg = "Nombre de personnes\nagés de 25 à 54 ans",
            fixmax=k,
            inches=inches,
            legend.pos="topright")
plotCircles(var = "art2014", 
            col = col_art, 
            title = "2 - Artisans, commerçants et chefs d'entreprise (2014)", 
            titleleg = "Nombre de de personnes\nagés de 25 à 54 ans",
            fixmax=k,
            inches=inches,
            legend.pos="topright")
plotCircles(var = "cad2014", 
            col = col_cad, 
            title = "3 - Cadres et professions intellectuelles supérieures (2014)", 
            titleleg = "Nombre de personnes\nagés de 25 à 54 ans",
            fixmax=k,
            inches=inches,
            legend.pos="topright")
plotCircles(var = "int2014", 
            col = col_int, 
            title = "4 - Professions Intermédiaires (2014)", 
            titleleg = "Nombre de personnes\nagés de 25 à 54 ans",
            fixmax=k,
            inches=inches,
            legend.pos="topright")
plotCircles(var = "emp2014", 
            col = col_emp, 
            title = "5 - Employés (2014)", 
            titleleg = "Nombre de personnes\nagés de 25 à 54 ans",
            fixmax=k,
            inches=inches,
            legend.pos="topright")
plotCircles(var = "ouv2014", 
            col = col_ouv, 
            title = "6 - Ouvriers (2014)", 
            titleleg = "Nombre de personnes\nagés de 25 à 54 ans",
            fixmax=k,
            inches=inches,
            legend.pos="topright")
dev.off()


# taux ########################################

exp_dim <- getFigDim(x = com77, width = 2000, mar = c(0.2,0.2,1.4,0.2), res = 100)
png(filename = "maps/CSP2014_tx.png", width = exp_dim[1]/2, height = exp_dim[2]/3, res = 100)
par(mar=c(0.2,0.2,1.4,0.2), bg="grey95", mfrow=c(2,3))

n = 4

com77$tmp <- com77$agr2014/com77$act2014*100
bks <- getBreaks(v = com77$tmp,method = "quantile",nclass = n)
cols <- carto.pal(pal1 = agr.pal,n1= n)
plotChoro(var = "tmp",
            col = cols, 
            bks = bks,
            title = "1 - Agriculteurs exploitants (2014)", 
            titleleg = "En part de la\npopulation active (%)",
            legend.pos="topright")

com77$tmp <- com77$art2014/com77$act2014*100
bks <- getBreaks(v = com77$tmp,method = "quantile",nclass = n)
cols <- carto.pal(pal1 = art.pal,n1= n)
plotChoro(var = "tmp",
          col = cols, 
          bks = bks,
          title = "2 - Artisans, commerçants et chefs d'entreprise (2014)", 
          titleleg = "En part de la\npopulation active (%)",
          legend.pos="topright")

com77$tmp <- com77$cad2014/com77$act2014*100
bks <- getBreaks(v = com77$tmp,method = "quantile",nclass = n)
cols <- carto.pal(pal1 = cad.pal,n1= n)
plotChoro(var = "tmp",
          col = cols, 
          bks = bks,
          title = "3 - Cadres et professions intellectuelles supérieures (2014)", 
          titleleg = "En part de la\npopulation active (%)",
          legend.pos="topright")

com77$tmp <- com77$int2014/com77$act2014*100
bks <- getBreaks(v = com77$tmp,method = "quantile",nclass = n)
cols <- carto.pal(pal1 = int.pal,n1= n)
plotChoro(var = "tmp",
          col = cols, 
          bks = bks,
          title = "4 - Professions Intermédiaires (2014)", 
          titleleg = "En part de la\npopulation active (%)",
          legend.pos="topright")

com77$tmp <- com77$emp2014/com77$act2014*100
bks <- getBreaks(v = com77$tmp,method = "quantile",nclass = n)
cols <- carto.pal(pal1 = emp.pal,n1= n)
plotChoro(var = "tmp",
          col = cols, 
          bks = bks,
          title = "5 - Employés (2014)", 
          titleleg = "En part de la\npopulation active (%)",
          legend.pos="topright")

com77$tmp <- com77$ouv2014/com77$act2014*100
bks <- getBreaks(v = com77$tmp,method = "quantile",nclass = n)
cols <- carto.pal(pal1 = ouv.pal,n1= n)
plotChoro(var = "tmp",
          col = cols, 
          bks = bks,
          title = "6 - Ouvriers (2014)", 
          titleleg = "En part de la\npopulation active (%)",
          legend.pos="topright")


dev.off()

# taux2 ########################################

exp_dim <- getFigDim(x = com77, width = 1000, mar = c(0.2,0.2,1.4,0.2), res = 100)
png(filename = "maps/CSP2014_tx2.png", width = exp_dim[1], height = exp_dim[2]/2, res = 100)
par(mar=c(0.2,0.2,1.4,0.2), bg="grey95", mfrow=c(1,2))
n = 4

com77$tmp <- (com77$emp2014 + com77$ouv2014) /com77$act2014*100
bks <- getBreaks(v = com77$tmp,method = "quantile",nclass = n)
cols <- carto.pal(pal1 = ouv.pal,n1= n)
plotChoro(var = "tmp",
          col = cols, 
          bks = bks,
          title = "Ouvriers en employés (2014)", 
          titleleg = "En part de la\npopulation active (%)",
          legend.pos="topright")


com77$tmp <- (com77$agr2014 + com77$int2014 + com77$cad2014 + com77$art2014 ) /com77$act2014*100
bks <- getBreaks(v = com77$tmp,method = "quantile",nclass = n)
cols <- carto.pal(pal1 = "grey.pal",n1= n)
plotChoro(var = "tmp",
          col = cols, 
          bks = bks,
          title = "Autres catégories (2014)", 
          titleleg = "En part de la\npopulation active (%)",
          legend.pos="topright")


dev.off()



# Type 2014 ########################################

nb <-   dim(com77)[1]
for (i in 1:nb)
{
  m <- max(com77$ouv2014[i], com77$emp2014[i], com77$agr2014[i], com77$cad2014[i], com77$int2014[i], com77$art2014[i])
  if (com77$ouv2014[i]==m){com77$typo[i] <- "Ouvriers"}
  if (com77$ouv2014[i]!=m){com77$typo[i] <- "Autre"}
}

nb <-   dim(com77)[1]
for (i in 1:nb)
{
  m <- max(com77$ouv2014[i], com77$emp2014[i], com77$agr2014[i], com77$cad2014[i], com77$int2014[i], com77$art2014[i])
  if (com77$emp2014[i]==m){com77$typo2[i] <- "Employés"}
  if (com77$emp2014[i]!=m){com77$typo2[i] <- "Autre"}
}

exp_dim <- getFigDim(x = com77, width = 1000, mar = c(0.2,0.2,1.4,0.2), res = 100)
png(filename = "maps/catégories2014.png", width = exp_dim[1], height = exp_dim[2]/2, res = 100)
par(mar=c(0.2,0.2,1.4,0.2), bg="grey95", mfrow=c(1,2))

plot(st_geometry(com77), col="#d8cac5", border="#e2d8cc", lwd=0.3)
propSymbolsTypoLayer(x = com77, var = "act2014", var2 = "typo",
                     symbols = "circle",          
                     legend.var2.title.txt = "CSP la plus représentée\n(parmi 6)",
                     col = c(col_ouv, "#a5a4a4"),
                     legend.var2.values.order = c("Ouvriers","Autre"),
                     legend.var.pos = "topright", border = "grey",
                     legend.var2.pos = "bottomright",
                     legend.var.title.txt = "Nombre d'actifs")
barscale(size = 10)
north(pos = "topleft", col = "#706f6f")
layoutLayer(title = "Les ouvriers (2014)", 
            sources = "Insee, 2018", author = "Nicolas LAMBERT, 2018", 
            col = "#706f6f", coltitle = "white", 
            frame = TRUE, scale = NULL, north = FALSE)


plot(st_geometry(com77), col="#d8cac5", border="#e2d8cc", lwd=0.3)
propSymbolsTypoLayer(x = com77, var = "act2014", var2 = "typo2",
                     symbols = "circle",          
                     legend.var2.title.txt = "CSP la plus représentée\n(parmi 6)",
                     col = c(col_emp, "#a5a4a4"),
                     legend.var2.values.order = c("Employés","Autre"),
                     legend.var.pos = "topright", border = "grey",
                     legend.var2.pos = "bottomright",
                     legend.var.title.txt = "Nombre d'actifs")
barscale(size = 10)
north(pos = "topleft", col = "#706f6f")
layoutLayer(title = "Les employés (2014)", 
            sources = "Insee, 2018", author = "Nicolas LAMBERT, 2018", 
            col = "#706f6f", coltitle = "white", 
            frame = TRUE, scale = NULL, north = FALSE)
dev.off()


# Type 1968 ########################################

nb <-   dim(com77)[1]
for (i in 1:nb)
{
  m <- max(com77$ouv1968[i], com77$emp1968[i], com77$agr1968[i], com77$cad1968[i], com77$int1968[i], com77$art1968[i])
  if (com77$ouv1968[i]==m){com77$typo[i] <- "Ouvriers"}
  if (com77$ouv1968[i]!=m){com77$typo[i] <- "Autre"}
}

nb <-   dim(com77)[1]
for (i in 1:nb)
{
  m <- max(com77$ouv1968[i], com77$emp1968[i], com77$agr1968[i], com77$cad1968[i], com77$int1968[i], com77$art1968[i])
  if (com77$emp1968[i]==m){com77$typo2[i] <- "Employés"}
  if (com77$emp1968[i]!=m){com77$typo2[i] <- "Autre"}
}

exp_dim <- getFigDim(x = com77, width = 1000, mar = c(0.2,0.2,1.4,0.2), res = 100)
png(filename = "maps/catégories1968.png", width = exp_dim[1], height = exp_dim[2]/2, res = 100)
par(mar=c(0.2,0.2,1.4,0.2), bg="grey95", mfrow=c(1,2))

plot(st_geometry(com77), col="#d8cac5", border="#e2d8cc", lwd=0.3)
propSymbolsTypoLayer(x = com77, var = "act1968", var2 = "typo",
                     symbols = "circle",          
                     legend.var2.title.txt = "CSP la plus représentée\n(parmi 6)",
                     col = c(col_ouv, "#a5a4a4"),
                     legend.var2.values.order = c("Ouvriers","Autre"),
                     legend.var.pos = "topright", border = "grey",
                     legend.var2.pos = "bottomright",
                     legend.var.title.txt = "Nombre d'actifs")
barscale(size = 10)
north(pos = "topleft", col = "#706f6f")
layoutLayer(title = "Les ouvriers (1968)", 
            sources = "Insee, 2018", author = "Nicolas LAMBERT, 2018", 
            col = "#706f6f", coltitle = "white", 
            frame = TRUE, scale = NULL, north = FALSE)


plot(st_geometry(com77), col="#d8cac5", border="#e2d8cc", lwd=0.3)
propSymbolsTypoLayer(x = com77, var = "act1968", var2 = "typo2",
                     symbols = "circle",          
                     legend.var2.title.txt = "CSP la plus représentée\n(parmi 6)",
                     col = c(col_emp, "#a5a4a4"),
                     legend.var2.values.order = c("Employés","Autre"),
                     legend.var.pos = "topright", border = "grey",
                     legend.var2.pos = "bottomright",
                     legend.var.title.txt = "Nombre d'actifs")
barscale(size = 10)
north(pos = "topleft", col = "#706f6f")
layoutLayer(title = "Les employés (1968)", 
            sources = "Insee, 2018", author = "Nicolas LAMBERT, 2018", 
            col = "#706f6f", coltitle = "white", 
            frame = TRUE, scale = NULL, north = FALSE)

dev.off()

# Typo ouv et emp ########################################
nb <-   dim(com77)[1]

for (i in 1:nb)
{
  a <- com77$ouv1968[i] + com77$emp1968[i]
  b <- com77$agr1968[i] + com77$cad1968[i] + com77$int1968[i] + com77$art1968[i]
  if (a >= b ){com77$typo68[i] <- "ouvemp"}
  else {com77$typo68[i] <- "autre"}
}


for (i in 1:nb)
{
  a <- com77$ouv2014[i] + com77$emp2014[i]
  b <- com77$agr2014[i] + com77$cad2014[i] + com77$int2014[i] + com77$art2014[i]
  if (a >= b ){com77$typo14[i] <- "ouvemp"}
  else {com77$typo14[i] <- "autre"}
}


exp_dim <- getFigDim(x = com77, width = 1000, mar = c(0.2,0.2,1.4,0.2), res = 100)
png(filename = "maps/empetouv.png", width = exp_dim[1], height = exp_dim[2]/2, res = 100)
par(mar=c(0.2,0.2,1.4,0.2), bg="grey95", mfrow=c(1,2))

plot(st_geometry(com77), col="#d8cac5", border="#e2d8cc", lwd=0.3)
propSymbolsLayer(x = com77[com77$typo68 == "autre",], var = "act1968",
                 symbols = "circle",          
                 col = "#a5a0a050",
                 inches = 0.3,
                 fixmax = max(com77$act1968),
                 legend.pos = "n", border = "#d3cfcf50")
propSymbolsLayer(x = com77[com77$typo68 == "ouvemp",], var = "act1968",
                     symbols = "circle",
                     col = "#b51515",
                     inches = 0.3,
                     fixmax = max(com77$act1968),
                     legend.pos = "topright", border = "white",
                     legend.title.txt = "Nombre d'actifs")
barscale(size = 10,pos = c(711312, 6799737))
north(pos = "topleft", col = "#706f6f")
layoutLayer(title = "Les grandes villes populaires de Seine-et-Marne (1968)", 
            sources = "Insee, 2018", author = "Nicolas LAMBERT, 2018", 
            col = "#706f6f", coltitle = "white", 
            frame = TRUE, scale = NULL, north = FALSE)
legendTypo(pos = "bottomright", title.txt = "", title.cex = 0.8,
           values.cex = 0.6, col = "#b51515", categ = "Majorité d'ouvriers et d'employés dans la commune", 
           cex = 0.75,
           nodata = FALSE, frame = FALSE)

com77$ouvemp1968 <- com77$ouv1968 + com77$emp1968
x <- com77[com77$typo68 == "ouvemp",]
top <- sort(x$ouvemp1968, decreasing = TRUE)
labelLayer(x = com77[data.frame(com77)[,"ouvemp1968"] %in% top[1:20],], txt = "NOM_COM", halo=TRUE, cex = 0.5, col= "#000000", bg = "#FFFFFF50", overlap = FALSE, show.lines = TRUE)


plot(st_geometry(com77), col="#d8cac5", border="#e2d8cc", lwd=0.3)

propSymbolsLayer(x = com77[com77$typo14 == "autre",], var = "act2014",
                 symbols = "circle",          
                 col = "#a5a0a050",
                 inches = 0.3,
                 fixmax = max(com77$act1968),
                 legend.pos = "n", border = "#d3cfcf50")
propSymbolsLayer(x = com77[com77$typo14 == "ouvemp",], var = "act2014",
            symbols = "circle",          
            col = "#b51515",
            inches = 0.3,
            fixmax = max(com77$act1968),
            legend.pos = "topright", border = "white",
            legend.title.txt = "Nombre d'actifs")
barscale(size = 10,pos = c(711312, 6799737))
north(pos = "topleft", col = "#706f6f")
layoutLayer(title = "Les grandes villes populaires de Seine-et-Marne (2014)", 
            sources = "Insee, 2018", author = "Nicolas LAMBERT, 2018", 
            col = "#706f6f", coltitle = "white", 
            frame = TRUE, scale = NULL, north = FALSE)
legendTypo(pos = "bottomright", title.txt = "", title.cex = 0.8,
           values.cex = 0.6, col = "#b51515", categ = "Majorité d'ouvriers et d'employés dans la commune", 
           cex = 0.75,
           nodata = F)

com77$ouvemp2014 <- com77$ouv2014 + com77$emp2014
x <- com77[com77$typo14 == "ouvemp",]
top <- sort(x$ouvemp2014, decreasing = TRUE)
labelLayer(x = com77[data.frame(com77)[,"ouvemp2014"] %in% top[1:20],], txt = "NOM_COM", halo=TRUE, cex = 0.5, col= "#000000", bg = "#FFFFFF50", overlap = FALSE, show.lines = TRUE)
dev.off()





# Graphique évolution par années ########################################

byyear <- function(var){
x <- c(sum(data.frame(com77)[,paste0(var,"1968")],na.rm=T),
sum(data.frame(com77)[,paste0(var,"1975")],na.rm=T),
sum(data.frame(com77)[,paste0(var,"1982")],na.rm=T),
sum(data.frame(com77)[,paste0(var,"1990")],na.rm=T),
sum(data.frame(com77)[,paste0(var,"1999")],na.rm=T),
sum(data.frame(com77)[,paste0(var,"2009")],na.rm=T),
sum(data.frame(com77)[,paste0(var,"2014")],na.rm=T))
x <- round(x,0)
return(x)
}

years <- c("1968","1975","1982","1990","1999","2009","2014")
df <- data.frame(year=years, agr=byyear("agr"), art=byyear("art"),cad = byyear("cad"),int = byyear("int"), emp=byyear("emp"),ouv=byyear("ouv"))

hisplot <- function(var,title,col){
x <- df[,c("year",var)]
  names(x) <- c("year","nb")
  ggplot(data=x, aes(x=year, y=nb)) +
    geom_bar(stat="identity", fill=col)+
    geom_text(aes(label=nb), vjust=1.6, color="white", size=3.5)+
    theme_minimal() + ggtitle(title) +
    xlab("Années") + ylab("Nombre de personnes")
}

# agriculteurs
png(filename = "maps/hist_agriculteurs.png", width = exp_dim[1], height = 500, res = 100)
hisplot(var = "agr",title = "Les agriculteurs en Seine-et-Marne, 1968-2014",col=col_agr)
dev.off()

names(com77)

# artisans
png(filename = "maps/hist_artisans.png", width = exp_dim[1], height = 500, res = 100)
hisplot(var = "art",title = "Artisans, commerçants et chefs d'entreprise en Seine-et-Marne, 1968-2014",col=col_art)
dev.off()

# cadres
png(filename = "maps/hist_cadres.png", width = exp_dim[1], height = 500, res = 100)
hisplot(var = "cad",title = "Cadres et professions intellectuelles supérieures en Seine-et-Marne, 1968-2014",col=col_cad)
dev.off()

# Professions Intermédiaires
png(filename = "maps/hist_intermédiares.png", width = exp_dim[1], height = 500, res = 100)
hisplot(var = "int",title = "Professions Intermédiaires en Seine-et-Marne, 1968-2014",col=col_int)
dev.off()

# employés
png(filename = "maps/hist_employés.png", width = exp_dim[1], height = 500, res = 100)
hisplot(var = "emp",title = "Les employés en Seine-et-Marne, 1968-2014",col=col_emp)
dev.off()

# ouvriers
png(filename = "maps/hist_ouvriers.png", width = exp_dim[1], height = 500, res = 100)
hisplot(var = "ouv",title = "Les ouvriers en Seine-et-Marne, 1968-2014",col=col_ouv)
dev.off()

# ALL
cols <- c(col_agr,col_art,col_cad,col_int,col_emp,col_ouv)
df2 <-  melt(df, id.vars="year")
png(filename = "maps/courbes_all.png", width = exp_dim[1], height = 500, res = 100)
ggplot(data=df2, aes(x=year, y=value, group=variable, color=variable)) + geom_line(size=1) + scale_color_manual(values=cols) + ggtitle('Les CSP en Seine-et-Marne, 1968-2014')+ xlab("Années") + ylab("Nombre de personnes") + geom_point(size=2) + guides(color= guide_legend(title = ""))
dev.off()

# Si on regroupe
df_aggreg <- df
df$ouvemp <- df$ouv + df$emp
df$rest <- df$agr + df$art + df$cad + df$int

df_aggreg <- df[,c("year","ouvemp","rest")]
names(df_aggreg) <- c("year", "Ouvriers et employés","Autres catégories")
# ALL
cols <- c("#d62222", "#63666b")
df2 <-  melt(df_aggreg, id.vars="year")
png(filename = "maps/courbes_all2.png", width = exp_dim[1], height = 500, res = 100)
ggplot(data=df2, aes(x=year, y=value, group=variable, color=variable)) + geom_line(size=1) + scale_color_manual(values=cols) + ggtitle('Les CSP en Seine-et-Marne, 1968-2014')+ xlab("Années") + ylab("Nombre de personnes") + geom_point(size=2) + guides(color= guide_legend(title = ""))
dev.off()
