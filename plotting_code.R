##################################### Reading data #####################################################

## Creat a directory for plotting data and later for the plot files
if(!file.exists("./optional_plotting")){
        dir.create("./optional_plotting")
}

## Download and read data
fileUrl <- "https://d18ky98rnyall9.cloudfront.net/_e143dff6e844c7af8da2a4e71d7c054d_payments.csv?Expires=1495929600&Signature=Nb3lO4G-C0IX4w0nG7l9roNit25pMM8vPOKcu~nhl0uH~zT11oaeXsiW8-PvufnUqTYacEgZ4g2HgxfS5hCtXQ0~AymiwvYAD2LDuyzj-EWu4bJPRdtQVXCt3Tj8rhSMwnWQZzYGOgOxnGk-o7mPrbRiJcuVRPjdDpg8UeAzFac_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A"
download.file(fileUrl, destfile = "./Optional_Plotting/plotting_data.csv", method = "curl")
medical_data <- read.csv("./Optional_Plotting/plotting_data.csv")

######################################## Plot 1 #####################################################

with(medical_data[medical_data$Provider.State == "NY",], 
     plot(Average.Covered.Charges, Average.Total.Payments,
          pch = 19, col = rgb(0,0.4,0.1,0.3), log = "xy",
          xlab = "Average Covered Charges", ylab = "Average Total Payments", 
          col.lab = colorRampPalette(c("red", "green", "blue"))(10)[8]))

## Plot linear regression line (untf = TRUE is adjusting the line to our log/log scale)
abline(lm(Average.Total.Payments ~ Average.Covered.Charges, data = medical_data),
       col = rgb(0,0.4,0.1), untf = TRUE, lwd = 2)

title(main = "Average Covered Charges and Average Total Payments \nRelationship in New York", 
      col.main = colorRampPalette(c("red", "green", "blue"))(10)[8], cex = 1.5)

## Add text to the figure indicating that plotting is done on log/log scale.
text(7000,29000, "log/log scale", col = colorRampPalette(c("red", "green", "blue"))(10)[9])

## Copy the plot to a pdf file and close the device
dev.copy2pdf(file = "./optional_plotting/plot1.pdf")
dev.off()


######################################## Plot 2 #####################################################

## load RColorBrewer to nicely color each category of DRG.Definition(we will use a qualitative pallete)
library(RColorBrewer)

## choose colors for variable DRG.Definition in three steps
## First: Extract number of levels from variable DRG.Definition
nlevels <- nlevels(medical_data$DRG.Definition)

## Second: Choose colors from RColorBrewer package
drg_colors <- brewer.pal(nlevels, "Set3")

## Third(optional: if used, increase number of colors in previous step): Choose a pallete of colors
## pallete <- colorRampPalette(drg_colors)

## Fourth(optional): get colors for each level
## drg_colors <- pallete(nlevels)

## Third: Make colors a little transparent
drg_colors_trans <- paste(drg_colors, sprintf("%x", ceiling(255*0.8)), sep = "")

## Save the names of the states in new variable called states
states <- levels(medical_data$Provider.State)

## Make the plots
par(oma = c(4,1,5,1))   ## make the outer margin at the bottom and at the top of the plot large
par(mai = c(0.6,0.6,0.1,0.1))  ## set the margins of each individual plot
par(mfrow = c(2,3))   ## make panels for the plots

for(state in states){
        state_data <- medical_data[medical_data$Provider.State == state,]
        with(state_data, 
             plot(Average.Covered.Charges, Average.Total.Payments,
                  pch = 19, col = drg_colors_trans[DRG.Definition],  ## map colors to DRG.Definition
                  log = "xy",
                  xlab = "Average Covered Charges", ylab = "Average Total Payments", 
                  col.lab = colorRampPalette(c("red", "green", "blue"))(10)[8]))
        legend("topleft", bty = "n", state, cex = 1.5)
        ## Fit linear model for each category of DRG.Definition and plot the curves for log/log scale(untf = TRUE)
        i = 1
        for(drg in levels(state_data$DRG.Definition)){
                abline(lm(state_data[state_data$DRG.Definition == drg,]$Average.Total.Payments ~
                                  state_data[state_data$DRG.Definition == drg,]$Average.Covered.Charges),
                       col = drg_colors_trans[i], untf = TRUE, lwd = 2)
                i = i + 1
        }
        
}

## Make a legend: http://dr-k-lo.blogspot.com/2014/03/the-simplest-way-to-plot-legend-outside.html
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 4, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom", legend = levels(medical_data$DRG.Definition), xpd = TRUE, 
       inset = c(0, 0), bty = "n", pch = 19, col = drg_colors_trans, ncol = 2,
       y.intersp = 1.2, cex = 0.9)

## Put a title
mtext("Mean Covered Charges and Mean Total Payments \nBy Medical Condition and State (log/log scale)", 
      side = 3, line = -4.5, cex = 1.6, col = rgb(0,0.6,0.8,1))

## Copy the plot to a pdf file and close the devicee
dev.copy2pdf(file = "./optional_plotting/plot2.pdf")
dev.off()
