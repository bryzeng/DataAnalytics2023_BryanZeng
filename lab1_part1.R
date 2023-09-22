EPI_data <- read.csv("/Users/Bryan/Desktop/Data_Analytics/EPI_data2010.csv")
#or
#EPI_data <- read.xlsx(”<path>/2010EPI_data.xlsx")
# Note: replace default data frame name – cannot start with numbers!
View(EPI_data)

# Replacing Header
names(EPI_data) <- as.matrix(EPI_data[1,])
EPI_data <- EPI_data[-1,]
EPI_data[] <- lapply(EPI_data, function(x) type.convert(as.character(x)))
EPI_data

# Summarize Data
shapiro.test(EPI_data$EPI) # fails to reject NULL
shapiro.test(EPI_data$DALY) # p-value < 0.05 significance

attach(EPI_data) 	# sets the ‘default’ object
fix(EPI_data) 	# launches a simple data editor
EPI 			# prints out values EPI_data$EPI
tf <- is.na(EPI) # records True values if the value is NA
E <- EPI[!tf] # filters out NA values, new array
summary(E)

#Exercise 1: Exploring Distribution
summary(EPI) 	# stats
fivenum(EPI,na.rm=TRUE)
help(stem)
stem(EPI)		 # stem and leaf plot
help(hist)
hist(EPI)
hist(EPI, seq(30., 95., 1.0), prob=TRUE)
help(lines)
lines(density(EPI,na.rm=TRUE,bw=1.)) # or try bw=“SJ”
help(rug)
rug(EPI) 

# Fitting Distribution Beyond Histograms
plot(ecdf(EPI), do.points=FALSE, verticals = TRUE) # Cumulative Density Function
par(pty="s") # Quantile-Quantile
qqnorm(EPI);qqline(EPI) #qqline() adds line to plot
x <- seq(30, 95, 1) # QQplot against generating distribution
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for tdsn")
qqline(x)

# Fitting Distribution w/ other data
plot(ecdf(EPI), do.points=TRUE, verticals = TRUE) # Visible points on plot
x2 <- seq(30, 95, 2)
qqplot(qt(ppoints(250), df = 20), x2, xlab = "Q-Q plot")
qqline(x2)

fivenum(DALY, na.rm = TRUE)
fivenum(WATER_H, na.rm = TRUE)
TD <- list(EPI_data$EPI, EPI_data$DALY, EPI_data$WATER_H)
names(TD) <- c("EPI", "DALY", "WA_H")
par(mgp = c(2, 0.5, 0))
boxplot(TD, xlab = "EPI Variables")
D <- EPI_data$DALY
W <- EPI_data$WATER_H
qqplot(E, D)
qqplot(E, W)

# Exercise 2: filtering (populations)
EPIland <- EPI[!Landlock] #conditional filtering
Eland <- EPIland[!is.na(EPIland)]
hist(Eland)
hist(Eland, seq(30., 95., 1.0), prob = TRUE)

summary(EPI_regions == "South Asia")
EPI_South_Asia <- EPI[EPI_regions == "South Asia"]
EPI_SA <- EPI_South_Asia[!is.na(EPI_South_Asia)]
hist(EPI_SA, seq(40., 70., 5.0), prob = TRUE)

#other data
GRUMP_data <- read.csv("/Users/Bryan/Desktop/Data_Analytics/GPW3_GRUMP_SummaryInformation_2010.csv")
attach(GRUMP_data)
summary(GRUMP_data$Mean.Point..pop.)
unique(GRUMP_data$Mean.Point..pop.)
GRUMP_data <- GRUMP_data[complete.cases(GRUMP_data$Mean.Point..pop., GRUMP_data$Mean.Extent..pop.), ]
str(Mean.Point..pop.)
str(Mean.Extent..pop.)
GRUMP_data$Mean.Point..pop. <- as.numeric(as.factor(GRUMP_data$Mean.Point..pop.))
GRUMP_data$Mean.Extent..pop. <- as.numeric(as.factor(GRUMP_data$Mean.Extent..pop.))
MPP <- GRUMP_data$Mean.Point..pop.
MEP <- GRUMP_data$Mean.Extent..pop.
MP <- data.frame(MPP, MEP)
qqplot(MPP, MEP)
hist(MPP)
hist(MEP)

Water_Treatment <- read.csv("/Users/Bryan/Desktop/Data_Analytics/water-treatment.csv")
unique(Water_Treatment$COND.P)
unique(Water_Treatment$PH.P)
CE <- Water_Treatment$COND.P
PP <- Water_Treatment$PH.P
qqplot(CE, PP)
hist(CE)
hist(PP)
