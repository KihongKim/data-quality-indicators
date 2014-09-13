# data quality indicators
# for single and multiple family housing
# comparing ACS and RLIS
# created by Prof. Liming Wang and Kihong Kim
# created in 6/24/2013

setwd("/home/kihong/DQI")

library(ggplot2)
library(RPostgreSQL)
library(plyr)
library(data.table)
library(reshape2)

# read "B25024(housing unit type)" tables of 1-year ACS SF
# the csv files were stored in /home/kihong/DQI, that were downloaded via American Fact Finder
# 2007 through 2011 years are available
ACSSF1yr2011_B25024_PUMA <- read.csv("ACS_11_1YR_B25024_with_ann.csv", sep=",", header=T)
ACSSF1yr2010_B25024_PUMA <- read.csv("ACS_10_1YR_B25024_with_ann.csv", sep=",", header=T)
ACSSF1yr2009_B25024_PUMA <- read.csv("ACS_09_1YR_B25024_with_ann.csv", sep=",", header=T)
ACSSF1yr2008_B25024_PUMA <- read.csv("ACS_08_1YR_B25024_with_ann.csv", sep=",", header=T)
ACSSF1yr2007_B25024_PUMA <- read.csv("ACS_07_1YR_B25024_with_ann.csv", sep=",", header=T)
# ACSSF3yr2011_B25024_PUMA <- read.csv("ACS_11_3YR_B25024_with_ann.csv", sep=",", header=T)
# ACSSF5yr2011_B25024_PUMA <- read.csv("ACS_11_5YR_B25024_with_ann.csv", sep=",", header=T)

# read "multifamily_housing_inventory20XX_puma2000" tables of RLIS
# the tables are stored in the PostgreSQL database
# 2010 and 2011 years are only available
conn <- dbConnect(PostgreSQL(), host="sapporo.usp.pdx.edu", user="smartdata", password="Smartaa00", dbname="portland") # connect to the PostgreSQL database
RLIS2011_MFI_PUMA <- dbReadTable(conn, c("rlis","multifamily_housing_inventory2011_puma2000"))
RLIS2010_MFI_PUMA <- dbReadTable(conn, c("rlis","multifamily_housing_inventory2010_puma2000"))

# read "taxlots20XX_sfh_puma2000" tables of RLIS
# the tables are stored in the PostgreSQL database
# 2002 through 2011 years are available (note that year 2008 needs to be corrected)
RLIS2011_TL_SFH_PUMA <- dbReadTable(conn, c("rlis","taxlots2011_sfh_puma2000"))
RLIS2010_TL_SFH_PUMA <- dbReadTable(conn, c("rlis","taxlots2010_sfh_puma2000"))
RLIS2009_TL_SFH_PUMA <- dbReadTable(conn, c("rlis","taxlots2009_sfh_puma2000"))
# RLIS2008_TL_SFH_PUMA <- dbReadTable(conn, c("rlis","taxlots2008_sfh_puma2000"))
RLIS2007_TL_SFH_PUMA <- dbReadTable(conn, c("rlis","taxlots2007_sfh_puma2000"))
RLIS2006_TL_SFH_PUMA <- dbReadTable(conn, c("rlis","taxlots2006_sfh_puma2000"))
RLIS2005_TL_SFH_PUMA <- dbReadTable(conn, c("rlis","taxlots2005_sfh_puma2000"))
RLIS2004_TL_SFH_PUMA <- dbReadTable(conn, c("rlis","taxlots2004_sfh_puma2000"))
RLIS2003_TL_SFH_PUMA <- dbReadTable(conn, c("rlis","taxlots2003_sfh_puma2000"))
RLIS2002_TL_SFH_PUMA <- dbReadTable(conn, c("rlis","taxlots2002_sfh_puma2000"))


#######
# ACS #
#######

# function
acs <- function(x){
	x$puma <- x$GEO.id2 - 4100000
	x <- subset(x,puma>1300)
	x$year <- substring(deparse(sys.call()),13,16)
	x$geography <- paste("puma", x$puma, sep="")
	x$mfh <- x$HD01_VD04 + x$HD01_VD05 + x$HD01_VD06 + x$HD01_VD07 + x$HD01_VD08 + x$HD01_VD09
	x$sfh <- x$HD01_VD02 + x$HD01_VD03
	x.melt <- melt(x,id.vars=c("year","geography"),measure.vars=c("mfh","sfh"),variable.name="unit_type",value.name="num_units")
	x.melt <- x.melt[order(rev(x.melt$year),x.melt$geography,x.melt$unit_type),]
	return(x.melt)
}

##### B25024 metadata of ACS SF 
#                   V1                                            V2
# 1             GEO.id                                            Id
# 2            GEO.id2                                           Id2
# 3  GEO.display-label                                     Geography
# 4          HD01_VD01                              Estimate; Total:
# 5          HD02_VD01                       Margin of Error; Total:
# 6          HD01_VD02                Estimate; Total: - 1, detached
# 7          HD02_VD02         Margin of Error; Total: - 1, detached
# 8          HD01_VD03                Estimate; Total: - 1, attached
# 9          HD02_VD03         Margin of Error; Total: - 1, attached
# 10         HD01_VD04                          Estimate; Total: - 2
# 11         HD02_VD04                   Margin of Error; Total: - 2
# 12         HD01_VD05                     Estimate; Total: - 3 or 4
# 13         HD02_VD05              Margin of Error; Total: - 3 or 4
# 14         HD01_VD06                     Estimate; Total: - 5 to 9
# 15         HD02_VD06              Margin of Error; Total: - 5 to 9
# 16         HD01_VD07                   Estimate; Total: - 10 to 19
# 17         HD02_VD07            Margin of Error; Total: - 10 to 19
# 18         HD01_VD08                   Estimate; Total: - 20 to 49
# 19         HD02_VD08            Margin of Error; Total: - 20 to 49
# 20         HD01_VD09                 Estimate; Total: - 50 or more
# 21         HD02_VD09          Margin of Error; Total: - 50 or more
# 22         HD01_VD10                Estimate; Total: - Mobile home
# 23         HD02_VD10         Margin of Error; Total: - Mobile home
# 24         HD01_VD11        Estimate; Total: - Boat, RV, van, etc.
# 25         HD02_VD11 Margin of Error; Total: - Boat, RV, van, etc.

##### PUMS Defition of BLD (Units in structure)
# BLD 2
# Units in structure
# bb .N/A (GQ)
# 01 .Mobile home or trailer
# 02 .One-family house detached
# 03 .One-family house attached
# 04 .2 Apartments
# 05 .3-4 Apartments
# 06 .5-9 Apartments
# 07 .10-19 Apartments
# 08 .20-49 Apartments
# 09 .50 or more apartments
# 10 .Boat, RV, van, etc.

# combine all B25024 tables
acs.all <- rbind(
acs(ACSSF1yr2011_B25024_PUMA),
acs(ACSSF1yr2010_B25024_PUMA),
acs(ACSSF1yr2009_B25024_PUMA),
acs(ACSSF1yr2008_B25024_PUMA),
acs(ACSSF1yr2007_B25024_PUMA)
)


########
# RLIS #
########

# function
rlis <- function(y,mfh=TRUE){
	if(mfh==FALSE){
		y$puma <- as.numeric(y$puma2000)
		y <- subset(y, puma!=99999)
		y$year <- substring(deparse(sys.call()),10,13)
		y$geography <- paste("puma",y$puma,sep="")
		y$sfh <- y$num_sfh
		return(y[c("year","geography","sfh")])
	} else {
	y$puma <- as.numeric(y$puma2000)
	y <- subset(y, puma!=99999)
	y$year <- substring(deparse(sys.call()),10,13)
	y$geography <- paste("puma",y$puma,sep="")
	y$mfh <- y$num_apartments + y$num_condominiums + y$num_duplexes + y$num_triplexes + y$num_townhomes
	return(y[c("year","geography","mfh")])
	}
}

# combine all MFI tables
rlis.mfh <- rbind(
rlis(RLIS2011_MFI_PUMA,mfh=TRUE),
rlis(RLIS2010_MFI_PUMA,mfh=TRUE))

# combine all SFH tables
rlis.sfh <- rbind(
rlis(RLIS2011_TL_SFH_PUMA,mfh=FALSE),
rlis(RLIS2010_TL_SFH_PUMA,mfh=FALSE),
rlis(RLIS2009_TL_SFH_PUMA,mfh=FALSE),
# rlis(RLIS2008_TL_SFH_PUMA,mfh=FALSE),
rlis(RLIS2007_TL_SFH_PUMA,mfh=FALSE),
rlis(RLIS2006_TL_SFH_PUMA,mfh=FALSE),
rlis(RLIS2005_TL_SFH_PUMA,mfh=FALSE),
rlis(RLIS2004_TL_SFH_PUMA,mfh=FALSE),
rlis(RLIS2003_TL_SFH_PUMA,mfh=FALSE),
rlis(RLIS2002_TL_SFH_PUMA,mfh=FALSE))

# combine all MFI and SFH tables
rlis.all <- merge(rlis.mfh,rlis.sfh,by=c("year","geography"),all.y=TRUE)
rlis.all <- melt(rlis.all,id.vars=c("year","geography"),measure.vars=c("mfh","sfh"),variable.name="unit_type",value.name="num_units")
rlis.all <- rlis.all[order(rev(rlis.all$year),rlis.all$geography,rlis.all$unit_type),]


##############
# ACS + RLIS #
##############
all <- merge(acs.all,rlis.all,by=c("year","geography","unit_type"),all.x=TRUE,all.y=TRUE)
all <- rename(all,c(num_units.x="num_units.acs",num_units.y="num_units.rlis"))
all <- all[order(rev(all$year),all$geography,all$unit_type),]


###########################
# Data Quality Indicators #
###########################

# functions to calculate several data quality indicators
count.na <- function(x) sum(is.na(x))
all.na <- function(x, y) max(count.na(x), count.na(y))==length(y)
rmse <- function(x=x, y=y) sqrt(mean((x-y)^2))
r2 <- function(x=x, y=y, ...) {if(!all.na(x, y)) summary(lm(y~x,...))$r.squared else 0.0}
intercept <- function(x=x, y=y, ...) {if(!all.na(x,y)) coef(lm(y~x,...))["(Intercept)"] else 0.0}
slope <- function(x=x, y=y, ...) {if(!all.na(x,y)) coef(lm(y~x,...))["x"] else 0.0}

# create a table for data quality indicators
all.dt <- data.table(all, key = "year")
dqi <- ddply(all.dt, .(year,unit_type), summarise, 
		rmse=round(rmse(x=num_units.rlis, y=num_units.acs),2),
		r2=round(r2(x=num_units.rlis, y=num_units.acs),2),
		intercept=round(intercept(x=num_units.rlis, y=num_units.acs),2),
		slope=round(slope(x=num_units.rlis, y=num_units.acs),2))
dqi <- dqi[order(rev(dqi$year),dqi$unit_type),]
dqi[dqi==0] <- NA
dqi$year <- as.numeric(dqi$year)

# create graphs
rmse <- ggplot(data=dqi,aes(x=year,y=rmse))+
		geom_point(aes(shape=unit_type),size=5)+
		theme(legend.position="none")+
		xlim(2007,2011)
r2 <- ggplot(data=dqi,aes(x=year,y=r2))+
		geom_point(aes(shape=unit_type),size=5)+
		theme(legend.position="none")+
		xlim(2007,2011)
intercept <- ggplot(data=dqi,aes(x=year,y=intercept))+
		geom_point(aes(shape=unit_type),size=5)+
		theme(legend.position="bottom")+
		xlim(2007,2011)
slope <- ggplot(data=dqi,aes(x=year,y=slope))+
		geom_point(aes(shape=unit_type),size=5)+
		theme(legend.position="none")+
		xlim(2007,2011)

png(file="dqi_housing.png")
multiplot(rmse,slope,r2,intercept,cols=2) # the "multiplot" function must be generated first
dev.off()

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

