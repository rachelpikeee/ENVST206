# Activity 3

# Looking to see if lemming grazing has an effect on methane production

# Reading in lemming grazing data
ch4 <- read.csv("/Users/rachelpike/Downloads/lemming_herbivory.csv")

# Making data factors so it easier to use later
ch4$herbivory <- as.factor(ch4$herbivory)

# Making a plot of the data to get a better sense of what it looks like
plot(ch4$CH4_Flux ~ ch4$herbivory, xlab ="Treatment", 
     ylab="CH4 fluxes (mgC m –2 day–1) ")

# Checking to see if data is normally distributed
shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ex"])
shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ctl"])

# Checking to see if the variances are roughly equal
bartlett.test(ch4$CH4_Flux ~ ch4$herbivory)

# Now we have met assumptions to use a t-test
t.test(ch4$CH4_Flux ~ ch4$herbivory)
# p-value well above confidence threshold --> difference between means could be
#   due to random chance --> don't reject null hypothesis

# Example of a one-sided t test
t.test(ch4$CH4_Flux, mu=10, alternative = 'greater')

# Learning ANOVA testing

# Reading in the insect data
datI <- read.csv("/Users/rachelpike/Downloads/insect_richness.csv")

# Making the names into factors
datI$urbanName <- as.factor(datI$urbanName)

# Checking to see if data is normally distributed
shapiro.test(datI$Richness[datI$urbanType == 1])
shapiro.test(datI$Richness[datI$urbanType == 3])
shapiro.test(datI$Richness[datI$urbanType == 8])
shapiro.test(datI$Richness[datI$urbanType == 9])

# Checking to see if the variances are equal across groups
bartlett.test(datI$Richness ~ datI$urbanType)

# Specify model for species richness and urban type
in.mod <- lm(datI$Richness ~ datI$urbanName)

# Run the ANOVA
in.aov <- aov(in.mod)

# Print out ANOVA table
summary(in.aov)

#run Tukey HSD to test how different means are of the different groups
tukeyT <- TukeyHSD(in.aov)

#view results
tukeyT

# Make a plotof Tukey test
# Make axes labels smaller than usual to fit on plot using cex.axis 
plot(tukeyT, cex.axis=0.75)

# Looking at the means of each group
tapply(datI$Richness, datI$urbanName, "mean")

# Learning Chi Squared Testing

# Set up contigency table with data on protected species population behavior
species <- matrix(c(18,8,15,32), ncol=2, byrow = TRUE) 
colnames(species) <- c("Not protected", "Protected")
rownames(species) <- c("Declining", "Stable/Increase")

# Make a mosaic plot with an informative title and axes labels
mosaicplot(species, xlab="population status", ylab="legal protection",
           main="Legal protection impacts on populations")

# Conduct a chi-squared test
chisq.test(species)

