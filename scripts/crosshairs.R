# Visualize paper results 
# by Pablo Villalobos

data <- read.csv("datasets/results.csv")

# Preliminary data manipulation
data$min.span = data$Start.of.outcome-data$End.of.exposure
data$max.span = data$End.of.outcome - data$Start.of.exposure
data$center.span = (data$max.span + data$min.span)/2
data$exposure.length = data$End.of.exposure - data$Start.of.exposure

colors <- c("red", "green", "blue", "cyan", "magenta", "yellow", "black")


# Plot persistence span vs effect size
png(file="figures/persistence_span_vs_effect_size.png", width=1000, height=600)
plot(0,0,type="n",xlim=c(0,1.1*max(data$max.span)),ylim=c(0,1), ylab = '', xlab = '')
segments(data$min.span, abs(data$Identified.persistent.correlation), 
         data$max.span, abs(data$Identified.persistent.correlation), 
         col=colors)
segments(data$center.span, abs(data$Identified.persistent.correlation)-data$Correlation.standard.error, 
         data$center.span, abs(data$Identified.persistent.correlation)+data$Correlation.standard.error, 
         col=colors)
title(xlab="Persistence span", ylab="Effect size", cex.lab=2)
legend(x="topright", legend=data$Paper, fill=colors)
dev.off()

# Plot exposure length vs effect size
png(file="figures/exposure_length_vs_effect_size.png", width=1000, height=600)
plot(0,0,type="n",xlim=c(0,1.1*max(data$exposure.length)),ylim=c(0,1), ylab = '', xlab = '')
segments(data$exposure.length, abs(data$Identified.persistent.correlation)-data$Correlation.standard.error, 
         data$exposure.length, abs(data$Identified.persistent.correlation)+data$Correlation.standard.error, 
         col=colors)
title(xlab="Exposure length", ylab="Effect size", cex.lab=2)
legend(x="topright", legend=data$Paper, fill=colors)
dev.off()
