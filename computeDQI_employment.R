# DQI on number of jobs vs. building square footage
# across census block and census tract
# created by Kihong Kim
# created in May 20, 2013

setwd("/home/kihong/DQI")

library(MASS)
library(RPostgreSQL)
library(plyr)
library(ggplot2)

# connect to the PostgreSQL database
conn <- dbConnect(PostgreSQL(), host="sapporo.usp.pdx.edu", user="smartdata", password="Smartaa00", dbname="portland")

# read wac2010_bg
wac2010_bg <- dbReadTable(conn, c("lehd","wac2010_bg"))

# read buildings2010
buildings2010 <- dbReadTable(conn, c("rlis","buildings2010"))
buildings2010 <- with(buildings2010, buildings2010[order(gid),])
buildings2010_bg <- ddply(buildings2010, .(state,county,tract,blockgroup), summarise, sqftBldg=sum(bldg_sqft))
# sqftBldg.tract <- ddply(buildings2010, .(state,county,tract), summarise, sqftBldg=sum(bldg_sqft))

# merge buildings2010 and wac2010_bg
merged_bg <- merge(buildings2010_bg, wac2010_bg, by=c("state","county","tract","blockgroup"))

# OLS regression analysis
ols.jobs.bg.nojittering <- lm(sqftBldg~numjobs, data=merged_bg)
summary(ols.jobs.bg.nojittering)
ols.jobs.bg.jittering <- lm(sqftBldg~numjobsjit, data=merged_bg)
summary(ols.jobs.bg.jittering)

# identify the observations with either large residuals (outliers) or high leverages from OLS regression plots
png("ols.jobs.bg.nojittering.png")
op <- par(mfrow = c(2, 2), oma=c(0,0,1.1,0))
plot(ols.jobs.bg.nojittering, las=1)
par(op)
dev.off()

png("ols.jobs.bg.jittering.png")
op <- par(mfrow = c(2, 2), oma=c(0,0,1.1,0))
plot(ols.jobs.bg.jittering, las=1)
par(op)
dev.off()

merged_bg[c(473,476,711),-8]

# create a data set to detect outliers and leverages
CD.ols.nojit <- cooks.distance(ols.jobs.bg.nojittering)
RES.ols.nojit <- stdres(ols.jobs.bg.nojittering)
RES.abs.ols.nojit <- abs(RES.ols.nojit)
CD.ols.jit <- cooks.distance(ols.jobs.bg.jittering)
RES.ols.jit <- stdres(ols.jobs.bg.jittering)
RES.abs.ols.jit <- abs(RES.ols.jit)
diagnose <- cbind(merged_bg,CD.ols.nojit,RES.ols.nojit,RES.abs.ols.nojit,CD.ols.jit,RES.ols.jit,RES.abs.ols.jit)

# find out the observations that have relatively large values of Cook's Distance (a conventional cut-off point is 4/n, where n is the number of observations)
diagnose[CD.ols.nojit>4/nrow(diagnose),c("state","county","tract","blockgroup","sqftBldg","numjobs","CD.ols.nojit")]
diagnose[CD.ols.jit>4/nrow(diagnose),c("state","county","tract","blockgroup","sqftBldg","numjobsjit","CD.ols.jit")]

# find out ten observations with the highest absolute residual values
diagnose.sorted.nojit <- with(diagnose, diagnose[order(-RES.abs.ols.nojit),])
diagnose.sorted.nojit[1:10,c("state","county","tract","blockgroup","sqftBldg","numjobs","RES.ols.nojit","RES.abs.ols.nojit")]
diagnose.sorted.jit <- with(diagnose, diagnose[order(-RES.abs.ols.jit),])
diagnose.sorted.jit[1:10,c("state","county","tract","blockgroup","sqftBldg","numjobsjit","RES.ols.jit","RES.abs.ols.jit")]



# robust regression analysis
# a compromise between excluding outliers or leverages entirely from the analysis and
# including all the data points and treating all them equally in OLS regression
rls.jobs.bg.nojittering <- rlm(sqftBldg~numjobs, data=merged_bg)
summary(rls.jobs.bg.nojittering)
rls.jobs.bg.jittering <- rlm(sqftBldg~numjobsjit, data=merged_bg)
summary(rls.jobs.bg.jittering)














# ignore for now
summary(dqi.jobs.blockgroup)
summary(dqi.jobs.blockgroup)$r.squared
ggplot(merge.blockgroup, aes(x=numJobs,y=sqftBldg))+
	geom_point(shape=1)+
	labs(title="Building SQFT vs. Number of Jobs in 2010 \n across census block groups")+
	ggsave(file="dqi.jobs.blockgroup.png")

merge.tract <- merge(sqftBldg.tract, numJobs.tract, by=c("state","county","tract"))
head(merge.tract)
dqi.jobs.tract <- lm(sqftBldg~numJobs, merge.tract)
summary(dqi.jobs.tract)
summary(dqi.jobs.tract)$r.squared
ggplot(merge.tract, aes(x=numJobs,y=sqftBldg))+
	geom_point(shape=1)+
	labs(title="Building SQFT vs. Number of Jobs in 2010 \n across census tracts")+
	ggsave(file="dqi.jobs.tract.png")

