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
