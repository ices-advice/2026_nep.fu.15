# Run analysis, write model results

# Before:
# After:

library(icesTAF)

mkdir("model")

library(fields)
library(gstlearn)
library(maps)
library(sessioninfo)
library(mapproj)
.Last.projection=list(active=F,projection="mean",parameters=NULL,orientation=NULL)
library(RColorBrewer)
library(sf)
library(tidyverse)

# set up
curr.year <- 2026
last.year <- curr.year-1

save.plots <- T # set it up as T or F whether you want to save the plots when running all the chunks or kniting the document
save.tables <- T # set it up as T or F whether you want to save the tables

nep15 <- read_csv("data/FU15_tv_final_2026.csv")

dim(nep15)

# quality control of entries

v <- ggplot(nep15, aes(x=as.factor(Year),y=Density_Adjusted))+ 
  geom_violin(aes(group=Year,colour=Year,fill=Year),alpha=0.5, 
              kernel="rectangular")+           # passes to stat_density, makes violin rectangular 
  geom_boxplot(aes(group=Year), width=.2)+    
  stat_summary(fun=mean, geom="line", colour="blue", aes(group=1)) +
  xlab("Year")+                                # label one axis
  ylab("Density burrow/m-2")+                       # label the other
  theme_bw()+                                  # make white background on plot
  theme(legend.position = "none")              # suppress legend

v + theme(axis.title.x=element_text(size=10),
          axis.title.y=element_text(size=10))

if (save.plots == T) {
  ggsave("model/FU15_violin_plot_density.png")
}

v <- ggplot(nep15, aes(x=as.factor(Year),y=Density_Adjusted))+ 
  geom_violin(aes(group=Year,colour=Year,fill=Year),alpha=0.5, 
              kernel="rectangular")+           # passes to stat_density, makes violin rectangular 
  geom_boxplot(aes(group=Year), width=.2)+    
  stat_summary(fun=mean, geom="line", colour="blue", aes(group=1)) +
  xlab("Year")+                                # label one axis
  ylab("Density burrow/m-2")+                       # label the other
  theme_bw()+                                  # make white background on plot
  theme(legend.position = "none")              # suppress legend

v + theme(axis.title.x=element_text(size=10),
          axis.title.y=element_text(size=10))

if (save.plots == T) {
  ggsave("model/FU15_violin_plot_density.png")
}

v <- ggplot(nep15, aes(x=as.factor(Year),y=Count))+ 
  geom_violin(aes(group=Year,colour=Year,fill=Year),alpha=0.5, 
              kernel="rectangular")+           # passes to stat_density, makes violin rectangular 
  geom_boxplot(aes(group=Year), width=.2)+    
  stat_summary(fun=mean, geom="line", colour="blue", aes(group=1)) +
  xlab("Year")+                                # label one axis
  ylab("Verified Burrow Counts")+                       # label the other
  theme_bw()+                                  # make white background on plot
  theme(legend.position = "none")              # suppress legend

v + theme(axis.title.x=element_text(size=10),
          axis.title.y=element_text(size=10))


mean_table <- nep15 %>%
  group_by(Year) %>%
  summarise(
    mean_burrow = mean(Count, na.rm = TRUE),
    sd_burrow   = sd(Count, na.rm = TRUE),
    n           = n()
  )

mean_table

if (save.plots == T) {
  ggsave("model/FU15_violin_plot_counts.png")
}

v <- ggplot(nep15, aes(x=as.factor(Year),y=DoGm))+ 
  geom_violin(aes(group=Year,colour=Year,fill=Year),alpha=0.5, 
              kernel="rectangular")+           # passes to stat_density, makes violin rectangular 
  geom_boxplot(aes(group=Year), width=.2)+    
  stat_summary(fun=mean, geom="line", colour="blue", aes(group=1)) +
  xlab("Year")+                                # label one axis
  ylab("Distance Over Ground Metres")+                       # label the other
  theme_bw()+                                  # make white background on plot
  theme(legend.position = "none")              # suppress legend

v + theme(axis.title.x=element_text(size=10),
          axis.title.y=element_text(size=10))

# Calculate mean Distance Over Ground per year
mean_table_dog <- nep15 %>%
  group_by(Year) %>%
  summarise(
    mean_dog = mean(DoGm, na.rm = TRUE),
    sd_dog   = sd(DoGm, na.rm = TRUE),
    n        = n()
  )

mean_table_dog

if (save.plots == T) {
  ggsave("model/FU15_violin_plot_dog.png")
}

#data summary

nep15 %>% group_by(Year, Ground) %>% 
  summarise(total.number.stns = length(unique(Station)),
            total.dog = sum(DoGm),
            total.burrow.count = sum(Count),
            total.area = sum(m2),
            mean.density.adj = mean(Density_Adjusted),
  )

#Kriging
## Create a gstlearn database and plot the data to check.

nep <- subset(nep15, Year == curr.year) #for 2026 UWTV data change to curr year
nep <- nep[ , c("Year", "MidDeglong", "MidDeglat", "Density_Adjusted")]

surv.yr<- mean(nep$Year)
mt <- paste(surv.yr, "FU15 UWTV")

# create .csv to be read by gstlearn. No need anymore
# write.csv(nep, paste0("nep", curr.year, ".csv"), row.names = F)

# Create db with gstlearn
data.db = fromTL(nep)
# data.db$display()

# Set Coordinates (x1 and x2) and Variable (z)
data.db$setLocators(c("MidDeglong", "MidDeglat"), ELoc_X())
data.db$setLocator("Density_Adjusted", ELoc_Z(), cleanSameLocator=TRUE)
# data.db[] # display as data.frame
# If we want to see detailed info of certain columns:
dbfmt = DbStringFormat_createFromFlags(flag_stats=TRUE, names=c("Density_Adjusted"))
data.db$display(dbfmt)

# Plotting with gstlearn
plot.init(asp=1.5) +
  plot.symbol(data.db,
              nameSize = "Density_Adjusted",
              nameColor="Density_Adjusted",
              flagLegend = TRUE,
              legendNameColor="Density_Adjusted",
              legendNameSize="Density_Adjusted") +
  plot.decoration(title=mt, xlab="Longitude", ylab="Latitude")


## Define the survey domain
plot_lim_x <- c(-7, -4)
plot_lim_y <- c(53, 55)
# FU
# Create db from csv with gstlearn
pol.FU15 = Polygons_createFromCSV(filename="data/Pol.FU15.csv")
pol.FU15$display()
plot.init() +
  plot.polygon(pol.FU15) +
  plot.decoration(title=mt, xlab="Longitude", ylab="Latitude")

# Europe
europa <- read.table("data/europa.txt", header=T)
europa <- subset(europa, x > plot_lim_x[1] & x < plot_lim_x[2] & y > plot_lim_y[1] & y < plot_lim_y[2]) # Cropping Europe polygon. I need to do this, if not Europe's polygon breaks when plotting it
europa_pol = Polygons()
polyelem1 = PolyElem(x=europa$x, y = europa$y)
europa_pol$addPolyElem(polyelem1)
# plot.init() +
#   plot.polygon(europa_pol)

# select points inside polygon
db_polygon(data.db, pol.FU15) # Now we have "Number of active samples" for all the points inside the polygon

plot.init(xlim = plot_lim_x, ylim = plot_lim_y, asp=1.5) +
  plot.symbol(data.db,
              nameSize = "Density_Adjusted",
              nameColor="Density_Adjusted",
              flagLegend = T,
              legendNameColor="Density_Adjusted") +
  plot.decoration(title=paste0(mt, " (all stns)"), xlab="Longitude", ylab="Latitude") +
  plot.polygon(pol.FU15, fill=NA)

## Visualizing the data set in projected space based on the mean of the points.
source("functions_for_gstlearn.R")

# Project samples points
pts_sf <- st_as_sf(data.frame(
  lon = nep$MidDeglong,
  lat = nep$MidDeglat,
  value = nep$Density_Adjusted
), coords = c("lon", "lat"), crs = 4326)
# We'll choose best projection for the polygon
pol.coords <- read.table("data/Pol.FU15.csv",
                         header = TRUE, sep = ",")
coords <- as.matrix(pol.coords)
# Run helper
prep <- prep_for_gstlearn(coords_polygon = coords, pts_sf = pts_sf, value_col = "value")
# Now we have:
prep$polygon_xy  
prep$points_df    # data.frame(x, y, z) for gstlearn
# prep$grid_df      # grid data.frame(x, y) (optional)
prep$crs_used     # which CRS was chosen
# New projected polygon
pol.FU15_m <- Polygons() # in metres
pol.FU15_m$addPolyElem(PolyElem(x = prep$polygon_xy[,1],
                                y = prep$polygon_xy[,2]))

pol.FU15_m$getSurface()/1000000   # area in km2
# New projected database
db.c1 = fromTL(prep$points_df)
db.c1$display()
# db.c1[] # display as data.frame
# Set Coordinates (x1 and x2) and Variable (z)
db.c1$setLocators(c("x", "y"), ELoc_X())
db.c1$setLocator("Density_Adjusted", ELoc_Z(), cleanSameLocator=TRUE)
# db.c1$display()

# select points inside polygon
db_polygon(db.c1, pol.FU15_m) # Now we have "Number of active samples" for all the points inside the polygon
db.c1$display()

# Now only "active samples" inside the polygon will be plotted
plot.init() + # Cropping breaks Europe polygon. Need to fix it
  plot.symbol(db.c1,
              nameSize = "AdjustedBurrowDensity",
              nameColor="AdjustedBurrowDensity",
              flagLegend = T,
              legendNameColor="Adjusted Burrow Density",
              legendNameSize="Adjusted Burrow Density") +
  plot.decoration(title=paste0(mt, " (stns inside polygon)"), xlab="Longitude", ylab="Latitude") +
  plot.polygon(pol.FU15_m, fill=NA) 

## Calculate summary statistics inside the polygon.
# Mean, variance, histogramme of data inside polygon.
# Two methods:

# From gstlearn (but numbers are rounded):
dbfmt = DbStringFormat_createFromFlags(flag_stats=TRUE, names=c("AdjustedBurrowDensity"))
db.c1$display(dbfmt)
plot.init() +
  plot.hist(db.c1, name="AdjustedBurrowDensity", bins=13, fill="blue")

# Manually:
zm<-mean(db.c1[,c("AdjustedBurrowDensity")][db.c1[,c("Polygon")]==1],na.rm=T) 
zv<-var(db.c1[,c("AdjustedBurrowDensity")][db.c1[,c("Polygon")]==1],na.rm=T)*(sum(db.c1[,c("Polygon")],na.rm=T)-1)/sum(db.c1[,c("Polygon")],na.rm=T)
cat("mean: ",zm,"    var: ",zv,"   cv: ",sqrt(zv)/zm,"\n")
hist(db.c1[,c("AdjustedBurrowDensity")][db.c1[,c("Polygon")]==1],nclass=20,xlab="burrow density n/m^2",main=mt)



## Set up the experimental variogram and plotting the points.

# Get suggestions for nlag and dlag parameters
sug <- suggest_lag_params(db.c1[,c("x", "y")], min_pairs = 50)
sug$nlag # nlag2024 = 9;        nlag2025 = 9
sug$dlag # nlag2024 = 10786.13; dlag2025 = 10860.86

# Compute the experimental variogram:
varioParamOmni <- VarioParam_createOmniDirection(nlag = sug$nlag, dlag = sug$dlag)
db.c1$setLocator("AdjustedBurrowDensity",ELoc_Z())
varioexp = Vario(varioParamOmni)
err = varioexp$compute(db.c1) # 0 means "no errors"

varioexp
# plot.init() + plot.varmod(varioexp)
plot.init() + plot.varmod(varioexp,drawPsize=TRUE) +
  plot.decoration(xlab="Distance (m)")

# We then the fit a model fitmod (automatic model fitting)
fitmod = Model()
# types = ECov_fromKeys(c("NUGGET","EXPONENTIAL","GAUSSIAN")) # If we want Model Fitting with pre-defined basic structures
types = ECov_fromKeys(c("SPHERICAL")) # If we want Model Fitting with pre-defined basic structures
err = fitmod$fit(varioexp,types=types)
# err = fitmod$fit(varioexp)
# Plot both experimental variogram and fitted model
plot.init() + plot.varmod(varioexp, fitmod) +
  plot.decoration(xlab="Distance (m)")
fitmod

# Variogram Maps ----
var.vmap = db_vmap(db.c1)
p1 = plot.init() + plot.raster(var.vmap, flagLegend=TRUE, legendName="Adjusted\nBurrow\nDensity\nVar")
p2 = plot.init() + plot.raster(var.vmap, name="VMAP.AdjustedBurrowDensity.Nb", flagLegend=TRUE, legendName="# pairs")
ggarrange(p1,p2,nrow=1,ncol=2)


## Gridding data set
gnx=99;gny=99 # to match the 100x100 from RGeostats and be able to plot historical data together
gx0=min(coords[,1]); gx1=max(coords[,1])
gy0=min(coords[,2]); gy1=max(coords[,2])
gdx=(gx1-gx0)/gnx; gdy=(gy1-gy0)/gny

grid_ggin = DbGrid_create(x0=c(gx0, gy0),
                          dx=c(gdx, gdy),
                          nx=c(100, 100))

# Grid for kriging in metres
gnx=99;gny=99
gx0=min(prep$polygon_xy[,1]); gx1=max(prep$polygon_xy[,1])
gy0=min(prep$polygon_xy[,2]); gy1=max(prep$polygon_xy[,2])
gdx=(gx1-gx0)/gnx; gdy=(gy1-gy0)/gny

grid_m = DbGrid_create(x0=c(gx0, gy0),
                       dx=c(gdx, gdy),
                       nx=c(100, 100))

# select points inside polygon
db_polygon(grid_ggin, pol.FU15) # degrees
db_polygon(grid_m, pol.FU15_m) # metres

# Print summary with info about the extent of the grid
dbfmt = DbStringFormat_createFromFlags(flag_extend=TRUE) 
grid_m$display(dbfmt)

# Now only "active samples" inside the polygon will be plotted
plot.init() + # Cropping breaks Europe polygon. Need to fix it
  plot.raster(grid_ggin) +
  plot.symbol(data.db,
              nameSize = "Density_Adjusted",
              nameColor="Density_Adjusted",
              flagLegend = T,
              legendNameColor="Density_Adjusted",
              legendNameSize="Density_Adjusted") +
  plot.decoration(title=paste0(mt, " GRID and stns inside polygon"), xlab="Longitude", ylab="Latitude") +
  plot.polygon(pol.FU15, fill=NA)

# Now only "active samples" inside the polygon will be plotted
plot.init() + # Cropping breaks Europe polygon. Need to fix it
  plot.raster(grid_m) +
  plot.symbol(db.c1,
              nameSize = "AdjustedBurrowDensity",
              nameColor="AdjustedBurrowDensity",
              flagLegend = T,
              legendNameColor="Adjusted Burrow Density",
              legendNameSize="Adjusted Burrow Density") +
  plot.decoration(title=paste0(mt, " GRID and stns inside polygon"), xlab="Longitude", ylab="Latitude") +
  plot.polygon(pol.FU15_m, fill=NA)


##  Simple Kriging

# We then create a “neighborhood” object corresponding to the specification of a unique neighborhood.
uniqueNeigh = NeighUnique()

# SIMPLE KRIGING ----
# Just for testing. This kriging is wrong as it assumes a mean = 0

# We now call the kriging function to perform the kriging prediction.
# We use the model fitmod that we previously fitted on our data,
# require to compute the kriging predictor and its standard-deviation (but not its variance),
# and change the prefix of the newly created variables to “SK”.
err = kriging(dbin=db.c1, dbout=grid_m, model=fitmod, 
              neigh=uniqueNeigh,
              flag_est=TRUE, flag_std=TRUE, flag_varz=FALSE,
              namconv=NamingConvention("SK"))

# We see that the kriging predictor and its standard deviation have been added to the grid data base.
grid_m

# Plot the kriging prediction over the grid using the plot.raster function and the data points.
p = plot.init(asp=1)
p = p + plot.raster(grid_m,
                    flagLegend = TRUE, palette="Spectral",legendName="Kriged\nAdjusted\nBurrow\nDensity") 
p = p + plot.symbol(db.c1,
                    nameSize = "AdjustedBurrowDensity",
                    flagLegend = T,
                    legendNameSize="Adjusted\nBurrow\nDensity")
p = p + plot.decoration(title="SIMPLE KRIGING over whole Grid")
plot.end(p)

# By default, the plotting function plots the variable with locator z1,
# which in our case corresponds to the kriging predictor (as the kriging function automatically assigns the locator z1 to it).
# To plot another variable, we can simply specify their name.
# For instance, we can plot the kriging standard deviation using the following code.

p = plot.init(asp=1)
p = p + plot.raster(grid_m,name="SK.AdjustedBurrowDensity.stdev",
                    flagLegend = TRUE, palette="Spectral",legendName="sd")
p = p + plot.symbol(db.c1,flagCst = T,pch=18,cex=1.5)
p = p + plot.decoration(title="SIMPLE KRIGING std-dev over whole Grid")
plot.end(p)

## ORDINARY KRIGING ----

fitmodOK = fitmod$clone()
err = fitmodOK$addDrift(DriftM())

# Then, ordinary kriging is performed using the same command as before, but with the newly created model.
err = kriging(dbin=db.c1, dbout=grid_m, model=fitmodOK, 
              neigh=uniqueNeigh,
              flag_est=TRUE, flag_std=TRUE, flag_varz=FALSE,
              namconv=NamingConvention("OK"))
# Finally, we plot the new kriging prediction over the grid and the data points.
p = plot.init(asp=1)
p = p + plot.raster(grid_m,
                    flagLegend = TRUE, palette="Spectral",legendName="Kriged\nAdjusted\nBurrow\nDensity") 
p = p + plot.symbol(db.c1,
                    nameSize = "AdjustedBurrowDensity",
                    flagLegend = T,
                    legendNameSize="Adjusted\nBurrow\nDensity")
p = p + plot.decoration(title="ORDINARY KRIGING over whole Grid")
plot.end(p)

# And the sd
p = plot.init(asp=1)
p = p + plot.raster(grid_m,name="OK.AdjustedBurrowDensity.stdev",
                    flagLegend = TRUE, palette="Spectral",legendName="sd")
p = p + plot.symbol(db.c1,flagCst = T,pch=18,cex=1.5,
                    legendNameSize="Adjusted Burrow Density")
p = p + plot.decoration(title="ORDINARY KRIGING std-dev over whole Grid")
plot.end(p)


## Compare Kriging plots ----

p = plot.init()
p = p + plot.correlation(grid_m,namex="OK.AdjustedBurrowDensity.estim",namey="SK.AdjustedBurrowDensity.estim", 
                         flagBiss=TRUE, flagSameAxes=TRUE, bins=100)
p = p + plot.decoration(title="Estimation SIMPLE vs. ORDINARY", 
                        xlab="Ordinary Kriging", ylab="Simple Kriging")
plot.end(p)


# We also compare the kriging standard-deviations obtained in both cases.
p = plot.init()
p = p + plot.correlation(grid_m,namex="OK.AdjustedBurrowDensity.stdev",namey="SK.AdjustedBurrowDensity.stdev", 
                         flagBiss=TRUE, flagSameAxes=TRUE, bins=100)
p = p + plot.decoration(title="St. dev. SIMPLE vs. ORDINARY", 
                        xlab="Ordinary Kriging", ylab="Simple Kriging")
plot.end(p)


# And we compare the numbers
## Global estimation by arithmetic average:
global.ar <- global_arithmetic(db.c1, grid_m, fitmod, ivar0 = 0, verbose = 1) # Global estimation by arithmetic average 
## Global estimation kriging:
global.ma_SK <- global_kriging(db.c1, grid_m, fitmod, ivar0 = 0, verbose=1) # SIMPLE kriging
global.ma_OK <- global_kriging(db.c1, grid_m, fitmodOK, ivar0 = 0, verbose=1) # ORDINARY kriging

# Arithmetic estimates
cat("arith.mean: ",round(global.ar$zest,7)," CV.geo: ",round(global.ar$cvgeo,8)," SSE: ",sse<-round(global.ar$cvgeo*global.ar$zest,8),"\n")
# Simple Kriging estimates (just for testing)
cat("SK.mean: ",round(global.ma_SK$zest,7)," CV.geo: ",round(global.ma_SK$cvgeo,8),"\n")
# Ordinary kriging estimates (very close to the arithmetic estimates because we have a regular grid and we have an almost-flat variogram)
cat("OK.mean: ",round(global.ma_OK$zest,7)," CV.geo: ",round(global.ma_OK$cvgeo,8),"\n")


# We want to save ggin, for this we run the kriging again in an epmty grid. So we only have the ordinary kriging outputed.
# And we want it with decimal degrees to match historical format.
err = kriging(dbin=data.db, dbout=grid_ggin, model=fitmodOK, 
              neigh=uniqueNeigh,
              flag_est=TRUE, flag_std=TRUE, flag_varz=FALSE)

ggin <- as.data.frame(grid_ggin[])
ggin$Polygon <- ggin$Polygon == 1 # Convert to logical to match historical format
ggin$Kriging.Density_Adjusted.estim[is.na(ggin$Kriging.Density_Adjusted.estim)] <- 0
ggin$Kriging.Density_Adjusted.stdev[is.na(ggin$Kriging.Density_Adjusted.stdev)] <- 0
write.csv(ggin, "model/ggin.csv")


## UWTV Survey Summary Statistics
# Survey abundance estimate in numbers (millions) 
abun <- global.ma_OK$zest*pol.FU15_m$getSurface()/1000000

# read in summary file from surfer calculation
k.sum <- read.csv("data/FU15_Summary_ADG_method_2025.csv")

k.sum <- k.sum[,!names(k.sum) == "X"]

k.sum <- rbind(k.sum, data.frame(Year=mean(nep$Year), Ground ="FU15", mean= zm,  N= sum(sapply(0:(db.c1$getNSample()-1), function(i) db.c1$isActive(i))),  
                                 sd = zv/zv^.05, se= sse, ciMult=NA, ci= abun*global.ma_OK$cvgeo*1.96, 
                                 area= pol.FU15_m$getSurface()/1000000, abund = abun, 
                                 upper= abun+abun*global.ma_OK$cvgeo*1.96, 
                                 lower= abun-abun*global.ma_OK$cvgeo*1.96, 
                                 CViid= zv/zm, meanGeo=  global.ma_OK$zest, CVgeo= global.ma_OK$cvgeo,method = "R-gstlearn"))



knitr::kable(k.sum[,1:8] )
knitr::kable(k.sum[,c(1, 9:15)])
if (save.tables == T) {
  write.csv(k.sum, "model/fu15.uwtv.summary.statistics.adg.csv", row.names = F)
}

err = xvalid(db=db.c1, model=fitmodOK, neigh=uniqueNeigh, 
             flag_xvalid_est=1, flag_xvalid_std=1,  
             namconv=NamingConvention_create("Xvalid", flag_locator = FALSE) # We specify, through the nameconv that we do not wish to modify the current locators in the data base (otherwise, the locator z1 is “moved” to the variable containing the cross-validation error).
)
# Histogram of cross-validation errors
fu15.assess<-read.csv("data/fu15.assess.csv")
fu15.assess<-rbind(fu15.assess,data.frame(year=curr.year,
                                         lower = abun-abun*global.ma_OK$cvgeo*1.96,
                                         abund = abun, 
                                         upper= abun+abun*global.ma_OK$cvgeo*1.96,
                                         ci= abun*global.ma_OK$cvgeo*1.96,
                                         int.lan.num ="",
                                         int.dis.num="",
                                         removals.n="",
                                         hr="",
                                         int.lan.wgt="",
                                          int.dis.wgt="",
                                         dis.rn="",
                                         dead.disc.r="",
                                         mw.lan="",
                                         mw.dis=""))
write.csv(fu15.assess,"model/fu15.assess.csv")
### Number of Stations completed
ggplot(k.sum,  aes(x = Year, y = N)) + 
  geom_line(size = 1) +  
  geom_point(size = 3) +        
  geom_text(aes(label = N), vjust = -0.8, linewidth = 3) +  # Text annotations above points
  #  geom_vline(xintercept = 2015, linetype = "dashed", color = "darkgrey", size = 1) +  # Dashed line for missing year
  theme_bw() +
  scale_x_continuous(name = "\nYear",
                     breaks = seq(min(k.sum$Year, na.rm = TRUE), max(k.sum$Year, na.rm = TRUE), 1)) +
  scale_y_continuous(name = "Number of Stations \n",
                     breaks = seq(0, max(k.sum$N, na.rm = TRUE) + 10, 10),
                     limits = c(0, max(k.sum$N, na.rm = TRUE) + 10)) +
  theme(panel.grid = element_blank()) 


if (save.plots == T) {
  ggsave("model/FU15_Number UWTV Stations.png")
}


### Abundance estimate (millions individuals)

ggplot(k.sum , aes(x=Year, y= abund))+
  geom_errorbar(aes(ymax=upper, ymin=lower, width=0.25)) +
  geom_line(size = 1) +
  geom_point() +
  geom_hline(yintercept = 3000, linetype = "dashed", color = "red", size = 1) + 
  theme_bw() +
  scale_x_continuous(name="\nYear",
                     breaks = seq(min(k.sum$Year, na.rm = TRUE), max(k.sum$Year, na.rm = TRUE), 1)) +
  scale_y_continuous(name = "Abundance (millions)\n",
                     breaks = seq(0, max(k.sum$upper, na.rm = TRUE)+100, 250),
                     limits = c(0, max(k.sum$upper, na.rm = TRUE)+100))  +
  theme(panel.grid = element_blank())




if (save.plots == T) {
  ggsave("model/FU15_Abundance_estimates.png")
}

### Uncertainty estimate

png("FU15_uncertainty_estimate.png", height=1000, width=1500, res=200)  

with(k.sum, plot(Year, CViid, type="l", ylim=c(0,0.21), ylab="CV", main="CV estimates"))
with(k.sum, lines(Year, CVgeo, type="l", col="red"))
abline(h=0.2, lty=2, col="green")
legend("topleft",c("CVgeo", "CViid", "WGNEPS Limit"), col =c(2,1, "green"), lty = c(1,1,2) )

dev.off()



### Mean Density estimate (burrow/m2)

png("model/FU15_UWTV Mean Density.png", height=1000, width=1500, res=200)  

with(k.sum, plot(Year, mean, type="l", ylim=c(0,max(mean, na.rm=T)), ylab="Numbers/m2", col="red", main="Mean estimates"))
with(k.sum, lines(Year, meanGeo, type="l"))
legend("bottomright",c("MeanGeo", "MeanArit"), col =c(1,2), lty = c(1,1) )

dev.off()


### Krigged contour plots over time.

ggin<- read.csv("data/ggin_combine.csv")

##data checks for station spacing
ggin$Density[is.na(ggin$Density)==T]
ggin$Density[ggin$Density<0]<-0.0
range(ggin$Density)
range(ggin$longitude)
range(ggin$latitude)

latlimits <- c(53.4, 54.6) 
longlimits <- c(-6.2, -4.7)

shapefile_path <- "data/FU15_polygon.shp"
FG <- st_read(shapefile_path)
print(FG)


ggplot(data = FG) +
  geom_sf(fill = "lightgrey", color = "darkgrey", size = 0.5) +  # Customize fill and border color
  geom_tile(data = subset(ggin, Polygon == TRUE), 
            aes(x = round(longitude, 5), 
                y = round(latitude, 5), 
                fill = Density)) +
  scale_fill_gradientn(colours = brewer.pal(9, "YlOrRd"), guide = "legend") +
  theme_bw() +
  coord_sf(xlim = longlimits, ylim = latlimits) +  # Use coord_sf for spatial data
  scale_x_continuous(labels = function(x) paste0(abs(x), "\u00B0")) + # Remove "W", keep degree symbol
  scale_y_continuous(labels = function(y) paste0(abs(y), "\u00B0")) + # Removed "°" symbol and added "\u00B0" - Unicode representation for the degree symbol (°)  - errors for the symbol 
  labs(y = "Latitude", x = "Longitude") +
  facet_wrap(~year, nrow = 4) + theme(axis.title.x=element_text(size=12, margin = margin(t = 15)),
                                      axis.text=element_text(size=8),
                                      axis.title.y=element_text(size=12, margin = margin(r = 20)),
                                      strip.text.x=element_text(size=8),
                                      legend.title = element_text(size=12),
                                      legend.text=element_text(size=9),
                                      legend.key.size = unit(1, "cm")) # Facet by year


#Kriged contour and density bubble plot.

p <- 
  ggplot(data = FG) +
  geom_sf(fill = "lightgrey", color = "darkgrey", size = 0.5) +  # Customize fill and border color
  geom_tile(data = subset(ggin, Polygon == TRUE), 
            aes(x = round(longitude, 5), 
                y = round(latitude, 5), 
                fill = Density)) +
  scale_fill_gradientn(colours = brewer.pal(9, "YlOrRd"), guide = "legend") +
  theme_bw() +
  coord_sf(xlim = longlimits, ylim = latlimits) +  # Use coord_sf for spatial data
  scale_x_continuous(labels = function(x) paste0(abs(x), "\u00B0")) + # Remove "W", keep degree symbol
  scale_y_continuous(labels = function(y) paste0(abs(y), "\u00B0")) + # Removed "°" symbol and added "\u00B0" - Unicode representation for the degree symbol (°)  - errors for the symbol
  labs(y = "Latitude", x = "Longitude") +
  facet_wrap(~year, nrow = 4) + theme(axis.title.x=element_text(size=12, margin = margin(t = 15)),
                                      axis.text=element_text(size=8),
                                      axis.title.y=element_text(size=12, margin = margin(r = 20)),
                                      strip.text.x=element_text(size=8),
                                      legend.title = element_text(size=12),
                                      legend.text=element_text(size=9),
                                      legend.key.size = unit(1, "cm")) # Facet by year


surv <- nep15
names(surv)[2] <- "year"
names(surv)[12] <- "MidDeglon"
names(surv)[13] <- "MidDeglat"
names(surv)[21] <- "Density_Adjusted"


session_info()
