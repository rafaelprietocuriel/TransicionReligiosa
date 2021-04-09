
##### Transición religiosa en México
##### Archivo para analizar % de distintas religiones en México
##### utiliza datos del censo de México 2020
##### La base de datos creada como proporciones
add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}

DB <- read.csv("BD_Religion_20210328_Prop.csv")
names(DB)[1] <- "ENT"
entidades <- unique(DB$ENT)
entidades[2:33] <- entidades[order(DB[(2:33)*18, 3], decreasing = T)+1]
colent <- colorRampPalette(c("tomato4", "tomato", "orange", "gold","cyan","blue", "navyblue", "black"))(33)
colent <-sample(colent )
colentA <- add.alpha(colent, alpha =.8)



entsToPlot <- 2:33 #32 entidades
#entsToPlot <- 1 # Todo México
#entsToPlot <- 10 #CDMX
#pobsToPlot <- 2:13
pobsToPlot <- c(2) #30 to 34
#pobsToPlot <- 8 #60 to 64
#pobsToPlot <- c(2, 8)

#### transición católicos
png("All.PNG", width = 600, height = 500)
{
par(mar = c(0,0,0,0))
plot(0, col = NA,
     xlim = c(1,4),
     ylim = c(0.5, 1))

for(e in entsToPlot){#entidad sin Mexico
 for(p in pobsToPlot){#todos los gpo pob
   h = 3 ### total, h = 4 hombres, h = 5 mujeres
   u <- DB$ENT == entidades[e]
   TDB <- DB[u, ]
   points(c(1:3),
          c(TDB[p, h], TDB[p+2, h+3], TDB[p+4, h+6]),
          type = "l", col=colent[e], lwd = 5*p/4)
 }
}

polygon(c(0,1,1,0), c(.5,.5,1,1), border = NA, col = "white")
polygon(c(0,1,1,0)+3, c(.5,.5,1,1), border = NA, col = "white")


for(k in 5:10){
  points(c(0.8, 3.2), c(k,k)*0.1, type = "l", lwd = 2, col = "gray30")
  text(3.31, k*.1, paste(10*k, "%", sep = ""),
       col = "gray30", cex = 2, adj = 0)
}

dev.off()
}

#### transición otras religiones
{
par(mar = c(0,0,0,0))
plot(0, col = NA,
     xlim = c(0,4),
     ylim = c(0., .4))
#for(e in 1){#solo todo  Mexico
#for(e in 10){#solo CDMX
for(e in 2:33){#entidad sin Mexico
  #for(p in 2:13){#todos los gpo pob
   #for(p in 2){# 30 a 34
      for(p in 8){# 60 a 64
    h = 12 ### total, h = 4 hombres, h = 5 mujeres
    u <- DB$ENT == entidades[e]
    TDB <- DB[u, ]
    
    points(c(1:3),
           c(TDB[p, h], TDB[p+2, h+3], TDB[p+4, h+6]),
           type = "l", col=colent[e], lwd = p/2)
  }
}
}

#### transición sin religion
{
par(mar = c(0,0,0,0))
plot(0, col = NA,
     xlim = c(0,4),
     ylim = c(0., .4))
for(e in 1){#solo todo  Mexico
#for(e in 10){#solo CDMX
#for(e in 2:33){#entidad sin Mexico
  for(p in 2:13){#todos los gpo pob
  #for(p in 2){# 30 a 34
  #for(p in 8){# 60 a 64
    h = 21 ### total, h = 4 hombres, h = 5 mujeres
    u <- DB$ENT == entidades[e]
    TDB <- DB[u, ]
    
    points(c(1:3),
           c(TDB[p, h], TDB[p+2, h+3], TDB[p+4, h+6]),
           type = "l", col=colent[e], lwd = p/2)
  }
}
}

#### scatter Nacional
{
png("NacionalshpsC.PNG", width = 600, height = 500)
h = 3 #all catolics
#h = 4 #all males
#h = 5 #all females
#h = 10 #others
#h = 21 # non rel
par(mar = c(0,0,0,0))
entsToPlot <- 33:2 #32 entidades
entsToPlot <- 1 # Todo México
#entsToPlot <- 16 #CDMX
#pobsToPlot <- 13:2
#pobsToPlot <- c(2) #30 to 34
#pobsToPlot <- 8 #60 to 64
#pobsToPlot <- c()
plot(1, col = NA,
#     xlim = c(0.,.2), ylim = c(0.,.2))
     xlim = c(0.7,1.), ylim = c(0.7,1.))

polygon(c(-1,2,2,-1),
        c(-1,-1,2,2),
        col = "white")
for(k in 0:10){
  points(c(k/10, 1), c(0,1-k/10), type = "l", lwd = 4, col = "gray30")
  points(c(k/10, k/10), c(0.1,k/10), 
         lty = 2, type = "l", lwd = 2, col = "gray60")
  points(c(k/10, 1), c(k/10,k/10), 
         lty = 2, type = "l", lwd = 2, col = "gray60")
  
}
# polygon(c(-1,2,2,-1),
#         c(-1,-1,.5,.5),
#         border = NA,
#         col = "white")
# polygon(c(1,2,2,1),
#         c(-1,-1,2,2),
#         border = NA,
#         col = "white")
for(e in entsToPlot){#entidad sin Mexico
  for(p in pobsToPlot){#todos los gpo pob
    u <- DB$ENT == entidades[e]
    TDB <- DB[u, ]
    points(TDB[p, h], TDB[p+4, h+6],
           pch = 21,
           col = "white",
           bg=colentA[e], cex = 2+p/1.5)
  }
}
points(pobsToPlot*0+0.8,
       .85+.1*pobsToPlot/max(pobsToPlot),
       pch = 25,
       col = "white",
       bg="gray", cex = 2+pobsToPlot/1.5)

# polygon(c(.5,1,1,.5),
#         c(.45,.45,.5,.5),
#         col = "gold")
# text(0.75, 0.475, "% católico en 2000", 
# adj = 0.5, col = "white", cex = 3)
# polygon(c(1,1.05,1.05,1),
#         c(.5,.5,1,1),
#         col = "navyblue")
# text(1.025, 0.75, "% católico en 2020", srt = 90,
#      adj = 0.5, col = "white", cex = 3)
dev.off()
}

#### scatter Nacional
{
png("Nacionalshp.PNG", width = 600, height = 500)
h = 3 #all catolics
#h = 4 #all males
#h = 5 #all females
#h = 10 #others
#h = 21 # non rel
par(mar = c(0,0,0,0))
entsToPlot <- 1 # Todo México
plot(1, col = NA,
     #     xlim = c(0.,.2), ylim = c(0.,.2))
     xlim = c(0.7,1.), ylim = c(0.7,1.))

polygon(c(-1,2,2,-1),
        c(-1,-1,2,2),
        col = "white")
for(k in 0:10){
  points(c(k/10, 1), c(0,1-k/10), type = "l", lwd = 4, col = "gray30")
  points(c(k/10, k/10), c(0.1,k/10), 
         lty = 2, type = "l", lwd = 2, col = "gray60")
  points(c(k/10, 1), c(k/10,k/10), 
         lty = 2, type = "l", lwd = 2, col = "gray60")
  
}
for(e in entsToPlot){#entidad sin Mexico
  for(p in pobsToPlot){#todos los gpo pob
    u <- DB$ENT == entidades[e]
    TDB <- DB[u, ]
    points(TDB[p, h], TDB[p+4, h+6],
           pch = 21,
           col = "white",
           bg=colentA[e], cex = 2+p/1.5)
  }
}
dev.off()
}

#### scatter CDMX
png("CDMX.PNG", width = 600, height = 500)
{
h = 3 #all catolics
#h = 4 #all males
#h = 5 #all females
#h = 10 #others
#h = 21 # non rel
par(mar = c(0,0,0,0))
entsToPlot <- 16 # Todo México
plot(1, col = NA,
     #     xlim = c(0.,.2), ylim = c(0.,.2))
     xlim = c(0.7,1.), ylim = c(0.7,1.))

polygon(c(-1,2,2,-1),
        c(-1,-1,2,2),
        col = "white")
for(k in 0:10){
  points(c(k/10, 1), c(0,1-k/10), type = "l", lwd = 4, col = "gray30")
  points(c(k/10, k/10), c(0.1,k/10), 
         lty = 2, type = "l", lwd = 2, col = "gray60")
  points(c(k/10, 1), c(k/10,k/10), 
         lty = 2, type = "l", lwd = 2, col = "gray60")
  
}
for(e in entsToPlot){#entidad sin Mexico
  for(p in pobsToPlot){#todos los gpo pob
    u <- DB$ENT == entidades[e]
    TDB <- DB[u, ]
    points(TDB[p, h], TDB[p+4, h+6],
           pch = 21,
           col = "white",
           bg=colentA[e], cex = 2+p/1.5)
  }
}
dev.off()
}

#### scatter All 
{
png("All.PNG", width = 600, height = 500)
h = 3 #all catolics
#h = 4 #all males
#h = 5 #all females
#h = 10 #others
#h = 21 # non rel
pobsToPlot <- 13:2
par(mar = c(0,0,0,0))
entsToPlot <- sample.int(32, 1)+1 # Todo México
plot(1, col = NA,
     #     xlim = c(0.,.2), ylim = c(0.,.2))
     xlim = c(0.5,1.), ylim = c(0.5,1.))

polygon(c(-1,2,2,-1),
        c(-1,-1,2,2),
        col = "white")
for(k in 0:10){
  points(c(k/10, 1), c(0,1-k/10), type = "l", lwd = 4, col = "gray30")
  points(c(k/10, k/10), c(0.1,k/10), 
         lty = 2, type = "l", lwd = 2, col = "gray60")
  points(c(k/10, 1), c(k/10,k/10), 
         lty = 2, type = "l", lwd = 2, col = "gray60")
  
}
for(e in entsToPlot){#entidad sin Mexico
  for(p in pobsToPlot){#todos los gpo pob
    u <- DB$ENT == entidades[e]
    TDB <- DB[u, ]
    points(TDB[p, h], TDB[p+4, h+6],
           pch = 21,
           col = "white",
           bg=colentA[e], cex = 2+p/1.5)
  }
  text(TDB[2, h], TDB[2+4, h+6], adj = 0, cex= 2,
       entidades[e], col = 1) 
#       col = colentA[e])
}

dev.off()
}


#### scatter All por rango de edades
{
png("All60-64.PNG", width = 600, height = 500)
pobsToPlot = 8
h = 3 #all catolics
#h = 4 #all males
#h = 5 #all females
#h = 10 #others
#h = 21 # non rel
par(mar = c(0,0,0,0))
entsToPlot <- 33:2 # Todo México
plot(1, col = NA,
     #     xlim = c(0.,.2), ylim = c(0.,.2))
     xlim = c(0.5,1.), ylim = c(0.5,1.))

polygon(c(-1,2,2,-1),
        c(-1,-1,2,2),
        col = "white")
for(k in 0:10){
  points(c(k/10, 1), c(0,1-k/10), type = "l", lwd = 4, col = "gray30")
  points(c(k/10, k/10), c(0.1,k/10), 
         lty = 2, type = "l", lwd = 2, col = "gray60")
  points(c(k/10, 1), c(k/10,k/10), 
         lty = 2, type = "l", lwd = 2, col = "gray60")
  
}
for(e in entsToPlot){#entidad sin Mexico
  for(p in pobsToPlot){#todos los gpo pob
    u <- DB$ENT == entidades[e]
    TDB <- DB[u, ]
    points(TDB[p, h], TDB[p+4, h+6],
           pch = 21,
           col = "white",
           bg=colentA[e], cex = 2+p/1.5)
  }
}
dev.off()
}

#### scatter Nacional sin religion y otras
{
  png("NacionalSinRelYOtras.PNG", width = 600, height = 500)
  #h = 3 #all catolics
  #h = 4 #all males
  #h = 5 #all females
  pobsToPlot <- 13:2
  #h = 21 # non rel
  par(mar = c(0,0,0,0))
  entsToPlot <- 1 # Todo México
  plot(1, col = NA,
       #     xlim = c(0.,.2), ylim = c(0.,.2))
       xlim = c(0.,0.19), ylim = c(0.,0.19))
  
  polygon(c(-1,2,2,-1),
          c(-1,-1,2,2),
          col = "white")
  for(k in 0:10){
    points(c(0,k/10), c(1-k/10,1), type = "l", lwd = 4, col = "gray30")
    points(c(k/10, 1), c(0,1-k/10), type = "l", lwd = 4, col = "gray30")
    points(c(k/10, k/10), c(0,1), 
           lty = 2, type = "l", lwd = 2, col = "gray60")
    points(c(0, 1), c(k/10,k/10), 
           lty = 2, type = "l", lwd = 2, col = "gray60")
    
  }
  h = 12 #others
  for(e in entsToPlot){#entidad sin Mexico
    for(p in pobsToPlot){#todos los gpo pob
      u <- DB$ENT == entidades[e]
      TDB <- DB[u, ]
      points(TDB[p, h], TDB[p+4, h+6],
             pch = 25,
             col = "white",
             bg=colentA[e], cex = 2+p/1.5)
    }
  }
  h = 21
  for(e in entsToPlot){#entidad sin Mexico
    for(p in pobsToPlot){#todos los gpo pob
      u <- DB$ENT == entidades[e]
      TDB <- DB[u, ]
      points(TDB[p, h], TDB[p+4, h+6],
             pch = 22,
             col = "white",
             bg=colentA[e+1], cex = 2+p/1.5)
    }
  }
  dev.off()
}

#### scatter CDMX sin religion y otras
{
  png("CDMXSinRelYOtras.PNG", width = 600, height = 500)
  #h = 3 #all catolics
  #h = 4 #all males
  #h = 5 #all females
  pobsToPlot <- 13:2
  #h = 21 # non rel
  par(mar = c(0,0,0,0))
  entsToPlot <- 16 # Todo México
  plot(1, col = NA,
       #     xlim = c(0.,.2), ylim = c(0.,.2))
       xlim = c(0.,0.19), ylim = c(0.,0.19))
  
  polygon(c(-1,2,2,-1),
          c(-1,-1,2,2),
          col = "white")
  for(k in 0:10){
    points(c(0,k/10), c(1-k/10,1), type = "l", lwd = 4, col = "gray30")
    points(c(k/10, 1), c(0,1-k/10), type = "l", lwd = 4, col = "gray30")
    points(c(k/10, k/10), c(0,1), 
           lty = 2, type = "l", lwd = 2, col = "gray60")
    points(c(0, 1), c(k/10,k/10), 
           lty = 2, type = "l", lwd = 2, col = "gray60")
    
  }
  h = 12 #others
  for(e in entsToPlot){#entidad sin Mexico
    for(p in pobsToPlot){#todos los gpo pob
      u <- DB$ENT == entidades[e]
      TDB <- DB[u, ]
      points(TDB[p, h], TDB[p+4, h+6],
             pch = 25,
             col = "white",
             bg=colentA[e], cex = 2+p/1.5)
    }
  }
  h = 21
  for(e in entsToPlot){#entidad sin Mexico
    for(p in pobsToPlot){#todos los gpo pob
      u <- DB$ENT == entidades[e]
      TDB <- DB[u, ]
      points(TDB[p, h], TDB[p+4, h+6],
             pch = 22,
             col = "white",
             bg=colentA[e+1], cex = 2+p/1.5)
    }
  }
  dev.off()
}

#### scatter All Otras
{
  png("AllOtras.PNG", width = 600, height = 500)
  #h = 3 #all catolics
  #h = 4 #all males
  #h = 5 #all females
  pobsToPlot <- 13:2
  #h = 21 # non rel
  par(mar = c(0,0,0,0))
  entsToPlot <- sample.int(32,1)+1 # Todo México
  plot(1, col = NA,
       #     xlim = c(0.,.2), ylim = c(0.,.2))
       xlim = c(0.,0.35), ylim = c(0.,0.35))
  
  polygon(c(-1,2,2,-1),
          c(-1,-1,2,2),
          col = "white")
  for(k in 0:10){
    points(c(0,k/10), c(1-k/10,1), type = "l", lwd = 4, col = "gray30")
    points(c(k/10, 1), c(0,1-k/10), type = "l", lwd = 4, col = "gray30")
    points(c(k/10, k/10), c(0,1), 
           lty = 2, type = "l", lwd = 2, col = "gray60")
    points(c(0, 1), c(k/10,k/10), 
           lty = 2, type = "l", lwd = 2, col = "gray60")
    
  }
  h = 12 #others
  for(e in entsToPlot){#entidad sin Mexico
    for(p in pobsToPlot){#todos los gpo pob
      u <- DB$ENT == entidades[e]
      TDB <- DB[u, ]
      points(TDB[p, h], TDB[p+4, h+6],
             pch = 25,
             col = "white",
             bg=colentA[e], cex = 2+p/1.5)
    }
    text(TDB[2, h], TDB[2+4, h+6], adj = 0, cex= 2,
         entidades[e], col = 1) 
  }
  # h = 21
  # for(e in entsToPlot){#entidad sin Mexico
  #   for(p in pobsToPlot){#todos los gpo pob
  #     u <- DB$ENT == entidades[e]
  #     TDB <- DB[u, ]
  #     points(TDB[p, h], TDB[p+4, h+6],
  #            pch = 22,
  #            col = "white",
  #            bg=colentA[e], cex = 2+p/1.5)
  #   }
  # }
  dev.off()
}

### scatter All Sin Rel
{
  png("AllSinRel.PNG", width = 600, height = 500)
  #h = 3 #all catolics
  #h = 4 #all males
  #h = 5 #all females
  pobsToPlot <- 13:2
  #h = 21 # non rel
  par(mar = c(0,0,0,0))
  entsToPlot <- sample.int(32,1)+1 # Todo México
  plot(1, col = NA,
       #     xlim = c(0.,.2), ylim = c(0.,.2))
       xlim = c(0.,0.19), ylim = c(0.,0.19))
  
  polygon(c(-1,2,2,-1),
          c(-1,-1,2,2),
          col = "white")
  for(k in 0:10){
    points(c(0,k/10), c(1-k/10,1), type = "l", lwd = 4, col = "gray30")
    points(c(k/10, 1), c(0,1-k/10), type = "l", lwd = 4, col = "gray30")
    points(c(k/10, k/10), c(0,1), 
           lty = 2, type = "l", lwd = 2, col = "gray60")
    points(c(0, 1), c(k/10,k/10), 
           lty = 2, type = "l", lwd = 2, col = "gray60")
    
  }
  # h = 12 #others
  # for(e in entsToPlot){#entidad sin Mexico
  #   for(p in pobsToPlot){#todos los gpo pob
  #     u <- DB$ENT == entidades[e]
  #     TDB <- DB[u, ]
  #     points(TDB[p, h], TDB[p+4, h+6],
  #            pch = 25,
  #            col = "white",
  #            bg=colentA[e], cex = 2+p/1.5)
  #   }
  # }
  h = 21
  for(e in entsToPlot){#entidad sin Mexico
    for(p in pobsToPlot){#todos los gpo pob
      u <- DB$ENT == entidades[e]
      TDB <- DB[u, ]
      points(TDB[p, h], TDB[p+4, h+6],
             pch = 22,
             col = "white",
             bg=colentA[e], cex = 2+p/1.5)
    }
    text(TDB[2, h], TDB[2+4, h+6], adj = 0, cex= 2,
         entidades[e], col = 1) 
  }
  dev.off()
}
