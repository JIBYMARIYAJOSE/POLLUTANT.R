g<-ggplot(aes(x = year, y = Emissions, fill=type), data=NEIdataBaltimore)
g+geom_bar(stat="identity")+
  facet_grid(.~type)+
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (Tons)")) + 
  labs(title=expression("PM"[2.5]*" Emissions, Baltimore City 1999-2008 by Source Type"))+
  guides(fill=FALSE)
  
