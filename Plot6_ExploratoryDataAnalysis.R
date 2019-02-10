library(ggplot2)
plot6_ExploratoryDataAnalysis <- function(){
    #Compare emissions from motor vehicle sources in Baltimore City with emissions 
    #from motor vehicle sources in Los Angeles County, 
    #California (\color{red}{\verb|fips == "06037"|}fips=="06037"). 
    #Which city has seen greater changes over time in motor vehicle emissions?
    
    ## read data
    if(!exists("NEI")){
        NEI <- readRDS("summarySCC_PM25.rds")
    }
    if(!exists("SCC")){
        SCC <- readRDS("Source_Classification_Code.rds")
    }


    # 24510 is Baltimore and 06037 is LA,
    # Searching for ON-ROAD type in NEI
    NEI_City_Comparison <- NEI[(NEI$fips=="24510" | NEI$fips=="06037") & NEI$type=="ON-ROAD",  ]
    
    
    #Sum of Emissions from PM2.5 grouped by years
    NEI_City_Comparison <- aggregate(Emissions ~ year+fips, NEI_City_Comparison, sum)
    
    # replacing the county code with text
    NEI_City_Comparison$fips[NEI_City_Comparison$fips=="24510"] <-"Baltimore City"
    NEI_City_Comparison$fips[NEI_City_Comparison$fips=="06037"] <-"Los Angeles City"
    
    
    png('plot6.png' , width=640, height=480)
    g <- ggplot(NEI_City_Comparison,aes(factor(year),Emissions))    
    g <- g + facet_grid(.~fips) 
    
    g<-g + geom_bar(stat="identity") 
    g <- g + xlab("Year") + ylab("Total PM2.5 Emissions") 
    g <- g+ ggtitle("Total Emissions from motor vehicle (type=ON-ROAD) in Baltimore City vs Los Angeles,1999-2008")
    print(g)
    dev.off()
    
}  