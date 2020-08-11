data <- read.csv("datasets/results.csv")

data$min.span = data$Start.of.outcome-data$End.of.exposure
data$max.span = data$End.of.outcome - data$Start.of.exposure
data$center.span = (data$max.span + data$min.span)/2

colors <- c("red", "green", "blue", "cyan", "magenta", "yellow", "black")


dev.new(width=15, height=8)

plot(0,0,type="n",xlim=c(0,1.1*max(data$max.span)),ylim=c(0,1), ylab = '', xlab = '')

segments(data$min.span, abs(data$Identified.persistent.effect), data$max.span, abs(data$Identified.persistent.effect), col=colors)

segments(data$center.span, abs(data$Identified.persistent.effect)-data$Standard.error, data$center.span, abs(data$Identified.persistent.effect)+data$Standard.error, col=colors)

title(xlab="Persistence span", ylab="Effect size")
legend(x="topright", legend=data$Paper, fill=colors)
