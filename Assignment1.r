library(readxl)
library(tidyr);
library(qcc)
library(data.table)
library(ggplot2)
library(plyr)
library(lattice)
library(rio)

# function to data from location
loadData <- function(location){
  data <- read_excel(location)
  return(data)
}

# function to create Jpg
createJpeg <- function(name, type = NULL){
    if(length(type) == 0){
        jpeg(file = paste(name, ".jpg"))
    } else {
        jpeg(file = paste(name, type, ".jpg"))
    }
}

getData <- function(data, type) {
    if(type == "Domestic"){
        # Load domestic data
        return(data$MN)
    } else {
        # Load international data
        return(data$QR)
    }
}

# function to create Pareto Chart
createParetoChart <- function(data){
    # Aggregate popuplation by state
    aggregate(data$IJ, by=list(state = data$GH), FUN = sum)
    # Order by population
    orderByPopulation <- data[order(-data$IJ),]
    # Select top 10 states
    top10States <- head(orderByPopulation, 10)
    # Store population
    States <- top10States$IJ
    # Name population
    names(States) <- top10States$GH
    # Create jpeg
    createJpeg("paretoChart")
    # Create pareto chart
    pareto.chart(States, main ="Pareto Chart for Top 10 States", col = 5)
}

# function to create Relative Frequency Distribution
createRelativeFrequencyDistrubution <- function(data, type) {
    matrix <- getData(data,type)
    # Build Matrix
    matrix <- cbind(prop.table(table(matrix)))
    # Store colnames
    colnames(matrix)<- c("Relative Frequency")
    # Create jpeg
    createJpeg("relativeFrequencyDistribution", type)
    # Create hist distribution
    hist(matrix,xlab = paste(type," Migration Rate"), main = paste("Relative Frequency For ",type," Migration Rate"),col = 5)
}

# create cumulativ frequenct plot by type
createCumulativeFrequencyLinePlot <- function(data, type) {
    # Set Rate for type [Domestic, International]
    rate <- getData(data, type)
     # Set intervals
    intervals = seq(-100, 100, by = 1)
    # Set cut to Domestic Rate
    rate.cut = cut(rate, intervals, right = FALSE)
    # Set frwquency to Domestic Rate
    rate.freq = table(rate.cut)
    # Create cummulative frequency
    cumFreq = c(0, cumsum(rate.freq))
    # Create jpeg
    createJpeg("cumulativeFrequencyLinePlot", type)
    # Create Plot intervals with cummulative Frequency
    plot(intervals, cumFreq, col ="red", main = paste("Cumulative Frequency for ", type, " Migration"), xlab = paste(type," Migration Rate"), ylab= "Cumulative Frequency" )
    # Create lines with cummulative Frequency
    lines(intervals, cumFreq, col = "blue") 
}

# function to create descriptive statistics by type
createDescriptiveStatistics <- function(data, type) {
    export(print(summary(getData(data, type))), paste("descriptiveStatistics", type, ".xlsx"))
    sapply(getData(data, type), median)
}

# function to create outliers by type
createOutliers <- function(data, type) {
    outliersData <- getData(data, type)
    # Create jpeg
    createJpeg("outliers", type)  
    # Create plot for data
    plot(outliersData)
    # Create box plot
    boxplot(outliersData)
    # Create box plot horizontal
    boxplot(outliersData, horizontal = T)
    # Store outlier data
    myboxplot <- boxplot(outliersData, horizontal = T, col = 5, main = paste("Outliers for ", type, " Migration Rate"))
    # Show outliers
    myboxplot$out
}

# function to create scatter plot
createScatterPlot <- function(data) {
    # create jpeg
    createJpeg("scatterPlot")  
    # create scatter plot for Domestic mirgation Rate & International Migration Rate
    plot(data$MN, data$QR, main = "ScatterPlot", xlab = "DomesticMigrationRate", ylab = "InternationalMigrationRate", las = 1, xlim = c(0,25), col = 5)
    # Create abline for correlation
    abline(lm(data$MN ~ data$QR), col = 5)
    # Smoothify lines
    lines(smooth.spline(data$MN, data$QR), col=4)
}

# Load segregated data
segregatedData <- loadData("uspopulation_segregated.xlsx")
# 1. Create a Pareto chart of the top 10 10 states with the highest population in 2017.
createParetoChart(segregatedData)
# 2. Create a relative frequency distribution histogram for the “Domestic Migration Rate” for all counties. Comment on and interpret the shape of the distribution.
createRelativeFrequencyDistrubution(segregatedData, "Domestic")
# 3. Create a cumulative frequency line plot for the “Domestic Migration Rate” for all counties.
createCumulativeFrequencyLinePlot(segregatedData, "Domestic")
# 4. Perform numerical descriptive statistics for the “Domestic Migration Rate” for all counties.
createDescriptiveStatistics(segregatedData, "Domestic")
# 5. For the “Domestic Migration Rate” for all counties, determine whether there are any outliers. Comment on the states to which the outliers belong.
createOutliers(segregatedData, "Domestic")
# 6. Repeat tasks 2-5 above for the “International Migration Rate” for all counties.
createRelativeFrequencyDistrubution(segregatedData, "International")
createCumulativeFrequencyLinePlot(segregatedData, "International")
createDescriptiveStatistics(segregatedData, "International")
createOutliers(segregatedData, "International")
# 7. Create a scatter plot of the (“International Migration Rate” versus “Domestic Migration Rate”.
createScatterPlot(segregatedData)