library(ggplot2)
plot3_ExploratoryDataAnalysis <- function(){
    # Of the four types of sources indicated by the \color{red}{\verb|type|}
    #type (point, nonpoint, onroad, nonroad) variable, 
    #which of these four sources have seen decreases in emissions from 1999-2008 
    #for Baltimore City? Which have seen increases in emissions from 1999-2008? 
    #Use the ggplot2 plotting system to make a plot answer this question..
    
    ## read data
    if(!exists("NEI")){
        NEI <- readRDS("summarySCC_PM25.rds")
    }
    if(!exists("SCC")){
        SCC <- readRDS("Source_Classification_Code.rds")
    }
    
    #Sum of Emissions from PM2.5 grouped by years
    NEI_Baltimore <- subset(NEI,fips=="24510")
    NEI_Emissions <- NEI_Baltimore$Emissions
    aggregatedTotalByYear <- aggregate(NEI_Emissions, by=list(NEI_Baltimore$year,NEI_Baltimore$type),FUN=sum,na.rm=TRUE)
    names(aggregatedTotalByYear) <- c("year","type","Emissions")
    
    # another way for taking yearwise sUm of emissions
    yearly_emmissions <- aggregate(Emissions ~ year+type, NEI_Baltimore, sum)
    
    #facet_grid(.~aggregatedTotalByYear$type)
    png('plot3.png', width=640, height=480)
    g <- ggplot(aggregatedTotalByYear,aes(factor(aggregatedTotalByYear$year),aggregatedTotalByYear$Emissions))    
    g<-g + geom_col(status="identity",fill=aggregatedTotalByYear$year)+ facet_grid(.~aggregatedTotalByYear$type) 
    g <- g + xlab("Year") + ylab("Total PM'[2.5]* Emissions") + ggtitle("Total Emissions in Baltimore City,from 1999 to 2008")
    print(g)
    dev.off()
    
    png('plot3_1.png')
    g <- ggplot(aggregatedTotalByYear,aes(aggregatedTotalByYear$year,aggregatedTotalByYear$Emissions, color=aggregatedTotalByYear$type))    
    g <- g + xlab("Year") + ylab("Total PM'[2.5]* Emissions") + ggtitle("Total Emissions in Baltimore City,from 1999 to 2008")
    g <- g + geom_line()
    print(g)
    dev.off()
    
    
}