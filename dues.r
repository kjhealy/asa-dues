### --------------------------------------------------
### ASA dues comparison
### --------------------------------------------------

library(ggplot2)
library(RColorBrewer)

dues <- read.csv("data/dues-table.csv", header=TRUE)
##dues$Income <- dues$Income*1000

dues.flat <- read.csv("data/dues-flat.csv", header=TRUE)

dues.m <- melt(dues, measure.vars=c("ASA.Current", "ASA.Proposed",
                       "AEA.Current",
                       "AEA.2012", "APSA", "AAA", "AHA"),
               variable_name="Association")
colnames(dues.m) <- c("Income", "Association", "Dues")
levels(dues.m$Association) <- c("ASA Current", "ASA Proposed",
                                "AEA Current",
                                "AEA 2012", "APSA", "AAA", "AHA")

g <- ggplot(dues.m, aes(x=Income, y=Dues, color=Association, group=Association))

pdf(file="figures/dues-comparison.pdf", height=7, width=9)
g + geom_path(size=2) + labs(x="\nAnnual Income in Thousands of Dollars",
                y="Annual Membership Dues for Regular, Employed Members\n") +
  scale_colour_discrete("Association") + #scale_colour_brewer(palette="Accent") +
  scale_y_continuous(formatter="dollar") +
  scale_x_continuous(breaks=c(15, 30, 50, 75, 100, 125, 150, 200)) + theme_bw() + opts(axis.text.y=theme_text(colour="#000000",hjust=1),
                       axis.text.x=theme_text(colour="#000000",vjust=1))
dev.off()


png(file="figures/dues-comparison.png", height=768, width=1024,
    res=96)
g + geom_path(size=2) + labs(x="\nAnnual Income in Thousands of Dollars",
                y="Annual Membership Dues for Regular, Employed Members\n") +
  scale_colour_discrete("Association") + #scale_colour_brewer(palette="Accent") +
  scale_y_continuous(formatter="dollar") +
  scale_x_continuous(breaks=c(15, 30, 50, 75, 100, 125, 150, 200)) + theme_bw() + opts(axis.text.y=theme_text(colour="#000000",hjust=1),
                       axis.text.x=theme_text(colour="#000000",vjust=1))
dev.off()



dues.specials <- read.csv("data/fixed-fee-table.csv", header=TRUE)

ind <- order(dues.specials$Unemployed)
dues.unemp <- dues.specials[ind,c("Association","Unemployed")]

ind.s <- order(dues.specials$Student)
dues.student <- dues.specials[ind.s, c("Association","Student")]

library(gdata)
dues.unemp[,"Association"] <- reorder.factor(dues.unemp[,"Association"],
                                             new.order=dues.specials$Association[ind])


dues.student[,"Association"] <- reorder.factor(dues.student[,"Association"],
                                               new.order=dues.specials$Association[ind.s])
detach(package:gdata)


unemp.m <- melt(dues.unemp)
colnames(unemp.m) <- c("Association", "Status", "Dues")

g <- ggplot(unemp.m, aes(x=Association, y=Dues))

pdf(file="figures/dues-unemployed.pdf", height=8,width=6)
g + geom_point(size=3) +
  labs(y="\nEffective cost for Unemployed members") +
  scale_y_continuous(formatter="dollar") + coord_flip() + theme_bw()
dev.off()

png(file="figures/dues-unemployed.png", height=600,width=400, res=96)
g + geom_point(size=3) +
  labs(y="\nEffective cost for Unemployed members") +
  scale_y_continuous(formatter="dollar") + coord_flip() + theme_bw()
dev.off()



student.m <- melt(dues.student)
colnames(student.m) <- c("Association", "Status", "Dues")

g <- ggplot(student.m, aes(x=Association, y=Dues))

pdf(file="figures/dues-student.pdf", height=8,width=6)
g + geom_point(size=3) +
  labs(y="\nEffective cost for Student members") +
  scale_y_continuous() + coord_flip() + theme_bw()
dev.off()

png(file="figures/dues-student.png", height=600,width=400, res=96)
g + geom_point(size=3) +
  labs(y="\nEffective cost for Student members") +
  scale_y_continuous() + coord_flip() + theme_bw()
dev.off()

