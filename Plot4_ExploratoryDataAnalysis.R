library(ggplot2)
plot4_ExploratoryDataAnalysis <- function(){
    # Across the United States, 
    #how have emissions from coal combustion-related sources changed from 1999-2008?
    
    ## read data
    if(!exists("NEI")){
        NEI <- readRDS("summarySCC_PM25.rds")
    }
    if(!exists("SCC")){
        SCC <- readRDS("Source_Classification_Code.rds")
    }
    # merge the two data sets 
    if(!exists("NEISCC")){
        NEISCC <- merge(NEI, SCC, by="SCC")
    }
    
    # fetch all NEIxSCC records with Short.Name (SCC) Coal
    coalMatches  <- grepl("coal", NEISCC$Short.Name, ignore.case=TRUE)
    subsetNEISCC <- NEISCC[coalMatches, ]
    
    #Sum of Emissions from PM2.5 grouped by years
    coal_emissions <- aggregate(Emissions ~ year, NEISCC, sum)
    
    #facet_grid(.~aggregatedTotalByYear$type)
    png('plot4.png' , width=640, height=480)
    g <- ggplot(coal_emissions,aes(factor(year),Emissions))    
    g<-g + geom_bar(stat="identity") 
    g <- g + xlab("Year") + ylab("Total PM2.5 Emissions") + ggtitle("Total Emissions from coal sources from 1999 to 2008")
    print(g)
    dev.off()
  
}    
