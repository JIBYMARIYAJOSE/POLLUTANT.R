NEIdataBaltimore<-subset(NEIdata, fips == "24510")
totalEmissionBaltimore <- aggregate(Emissions ~ year, NEIdataBaltimore, sum)
totalEmissionBaltimore

##   year Emissions
## 1 1999      3274
## 2 2002      2454
## 3 2005      3091
## 4 2008      1862
barplot(
  (totalEmissionBaltimore$Emissions)/10^6,
  names.arg=totalEmissionBaltimore$year,
  xlab="Year",
  ylab="PM2.5 Emissions (10^6 Tons)",
  main="Total PM2.5 Emissions From All Baltimore City Sources"
)

plot of chunk plottotalEmissionBaltimore
