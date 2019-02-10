library(ggplot2)
plot5_ExploratoryDataAnalysis <- function(){
    # How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
    
    ## read data
    if(!exists("NEI")){
        NEI <- readRDS("summarySCC_PM25.rds")
    }
    if(!exists("SCC")){
        SCC <- readRDS("Source_Classification_Code.rds")
    }
    
    
    # 24510 is Baltimore,
    # Searching for ON-ROAD type in NEI
    NEI_Baltimore_Motor <- NEI[NEI$fips=="24510" & NEI$type=="ON-ROAD",  ]
    
    #Sum of Emissions from PM2.5 grouped by years
    NEI_Baltimore_Motor <- aggregate(Emissions ~ year, NEI_Baltimore_Motor, sum)
    
    
    png('plot5.png' , width=640, height=480)
    g <- ggplot(NEI_Baltimore_Motor,aes(factor(year),Emissions))    
    g<-g + geom_bar(stat="identity") 
    g <- g + xlab("Year") + ylab("Total PM2.5 Emissions") + ggtitle("Total Emissions from motor vehicle (type = ON-ROAD) in Baltimore City, from 1999 to 2008")
    print(g)
    dev.off()
    
}  