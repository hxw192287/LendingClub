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

#add fico score group
aprvd <- transform(aprvd, score_group=
                       ifelse(score<690,"660-690",
                       ifelse(score<720,"690-720",
                       ifelse(score<750,"720-750",
                       ifelse(score<780,"750-780",
                       ifelse(score<810,"780-810",
                       ifelse(score<840,"810-840","840-870")))))))

#loans for different score
amount_score <- group_by(aprvd, score_group) %>% summarise(loan=sum(aprvd$amount))
plot_ly(amount_score, type="pie", values=sum(aprvd$amount), labels=aprvd$score_group, hole=0.4, sort=F, textinfo="label+percent",
        marker = list(colors = brewer.pal(7, "Pastel2"),line = list(width = 1, color = "rgb(52, 110, 165)")),textposition="outside"
        ) %>% layout(title="Loans for different fico score", autosize=T)

#loans for different grades
amount_grade <- group_by(aprvd, grade) %>% summarise(loan=sum(amount))
plot_ly(amount_grade, type="pie", values=sum(aprvd$amount), labels=aprvd$grade, hole=0.4, sort=F, textinfo="label+percent",
        marker = list(colors = brewer.pal(7, "Pastel2"),line = list(width = 1, color = "rgb(52, 110, 165)")),textposition="outside"
        )%>% layout(title="Loans for different grades", autosize=T)

#loans for different purpose
amount_purpose <- group_by(aprvd, purpose) %>% summarise(loan=sum(amount))
plot_ly(type = "bar", x = amount_purpose$purpose,y = amount_purpose$loan,
        marker = list(color = amount_purpose$purpose,
                      colorscale = list(c(0, "rgb(201, 218, 248)"), list(1, "rgb(61, 133, 198)")),
                      line = list(width = 1, color = "rgb(255, 255, 255)"))
) %>% layout(title = "Loans for different purpose", bargap = 0,
             yaxis = list(title = "Total Loan Issued"),
             xaxis = list(title = "Loan Purpose"))
#From this bar plot, we can see most of the loans are used on debt consolidation and credit card.

#fico score distribution for each LC Grades
qplot(grade, score, data = approved, geom = "boxplot", color = grade) + theme_bw() +
    xlab(toupper("Lending Club Grades")) + ylab("Fico Scores") +
    ggtitle(toupper("Fico score vs LC Grades"))
#From this graph, average debt-to-income ratios have no big difference in differentgroups. And people who have high debt-to-income ratio are in C, D and E groups, they cannot get good grade.

#the densities of fico scores against Lending Club's grades
qplot(grade, score, data = approved, geom = "violin", color = grade) + theme_bw() +
    xlab(toupper("Lending Club Grades")) + ylab("Fico Scores") +
    ggtitle(toupper("Fico score vs Grades"))
#From this graph, we can see that Lending Club¡¯s grade basically matches fico score, borrowers who have high fico score can usually get high grade. However, there are no big difference between C and D, E and F.

#remove records with dti > 100
d <- approved$dti<100
#boxplot of the distribution the DTI for each grade:
qplot(grade, dti, data = approved[d,], geom = "boxplot", color = grade) + theme_bw() +
    xlab(toupper("Lending Club Grades")) +
    ylab(toupper("Debt to income ratio")) +
    ggtitle(toupper("DTI vs Grades"))


