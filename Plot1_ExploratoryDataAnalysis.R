plot1_ExploratoryDataAnalysis <- function(){
    # Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
    # Using the base plotting system, make a plot showing the total PM2.5 emission from all sources 
    # for each of the years 1999, 2002, 2005, and 2008.
    
    ## read data
    if(!exists("NEI")){
        NEI <- readRDS("summarySCC_PM25.rds")
    }
    if(!exists("SCC")){
        SCC <- readRDS("Source_Classification_Code.rds")
    }

    #Sum of Emissions from PM2.5 grouped by years
    NEI_Emissions <- NEI$Emissions
    aggregatedTotalByYear <- aggregate(NEI_Emissions, by=list(NEI$year),FUN=sum,na.rm=TRUE)
    names(aggregatedTotalByYear) <- c("year","Emissions")
    
    # another way for taking yearwise sUm of emissions
    #yearly_emmissions <- aggregate(Emissions ~ year, NEI, sum)
    
    png('plot1.png')
    barplot(height=aggregatedTotalByYear$Emissions/1000, names.arg=aggregatedTotalByYear$year, col="blue", xlab='Years', ylab="Total PM'[2.5]* Emissions", main = "Total PM'[2.5]* Emissions at various Years")
    dev.off()

}
    