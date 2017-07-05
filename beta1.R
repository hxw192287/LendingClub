library(ggplot2)
library(dplyr)
library(maps)
library(plotly)
library(ggthemes)
library(RColorBrewer)
library(lattice)
library(reshape)

#load approved loan data
load("approved.RData")
aprvd <- approved

#convert amount values to numeric (avoid integer overflow)
aprvd$amount <- as.numeric(aprvd$amount)

#boxplot of the distribution the DTI for each grade:
qplot(grade, dti, data = approved, geom = "boxplot", color = grade) + theme_bw() +
  xlab(toupper("Lending Club Grades")) +
  ylab(toupper("Debt to income ratio")) +
  ggtitle(toupper("DTI vs Grades"))

#loan data by states
amount_state <- group_by(aprvd, state) %>% summarise(loan=sum(amount))

# specify some map projection/options
l <- list(color = toRGB("steelblue"), width = 1)
g <- list(scope = 'usa', projection = list(type = 'albers usa'), showlakes = F, lakecolor = toRGB('white'))
amount_state$hover = with(amount_state, paste(state, '<br>', loan))
#total loan amount in each state
plot_ly(amount_state, z = amount_state$loan, text = hover, locations = amount_state$state, type = 'choropleth',
        locationmode = 'USA-states', color = amount_state$loan, colors = 'Oranges',
        marker = list(line = l), colorbar = list(title = "Default rates")) %>%
  layout(title = 'DEFAULT RATES PER STATES', geo = g)