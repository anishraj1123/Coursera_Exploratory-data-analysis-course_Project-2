plot2_ExploratoryDataAnalysis <- function(){
    # Have total emissions from PM2.5 decreased in the Baltimore City, 
    #Maryland (\color{red}{\verb|fips == "24510"|}fips=="24510") from 1999 to 2008? 
    #Use the base plotting system to make a plot answering this question.
    
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
    aggregatedTotalByYear <- aggregate(NEI_Emissions, by=list(NEI_Baltimore$year),FUN=sum,na.rm=TRUE)
    names(aggregatedTotalByYear) <- c("year","Emissions")
    
    # another way for taking yearwise sUm of emissions
    #yearly_emmissions <- aggregate(Emissions ~ year, NEI, sum)
    
    png('plot2.png')
    barplot(height=aggregatedTotalByYear$Emissions/1000, names.arg=aggregatedTotalByYear$year, col="blue", xlab='Years', ylab="Total PM'[2.5]* Emissions in millions", main =  "Total PM'[2.5]* Emissions in Baltimore in various Years")
    dev.off()
    
}