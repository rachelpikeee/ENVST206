# Activity 4

# Reading in the beaver data
datB <- read.csv("/Users/rachelpike/ENVST Data/beaver_dam.csv")

# Visualizing the data with a scatter plot
# "pch" designates what shapes we want the points on the plot
plot(datB$dams.n, datB$area.ha, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Surface water area (ha)",
     xlab = "Number of beaver dams")

# Creating linear regression model
dam.mod <- lm(datB$area.ha ~ datB$dams.n)

# Calculating standardized residuals
dam.res <- rstandard(dam.mod)

# Checking to see if residuals are normally distributed
qqnorm(dam.res)
qqline(dam.res)
shapiro.test(dam.res)

# Checking for equal variances by making a plot of residuals
plot(datB$dams.n, dam.res,
     pch = 19,
     xlab = "beaver dams",
     ylab = "standardized residual")
abline(h=0)

# Printing results of the model
summary(dam.mod)
