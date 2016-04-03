library(plyr)
temp <- tempfile()
download.file("http://www.nyc.gov/html/ceo/downloads/misc/2013-NYC-Dataset.zip",temp)
`2013.NYC.ACS.CEO` <- read.csv(unz(temp, "2013 NYC Web Dataset.csv"))
unlink(temp)
#Doing some data munging now. Going to code certain variables that will be important to later demographic analysis.

`2013.NYC.ACS.CEO`$Boro = as.character(`2013.NYC.ACS.CEO`$Boro)
`2013.NYC.ACS.CEO`$HousingStatus = as.character(`2013.NYC.ACS.CEO`$HousingStatus)
`2013.NYC.ACS.CEO`$Ethnicity = as.character(`2013.NYC.ACS.CEO`$Ethnicity)
`2013.NYC.ACS.CEO`$EducAttain = as.character(`2013.NYC.ACS.CEO`$EducAttain)
`2013.NYC.ACS.CEO`$CitizenStatus = as.character(`2013.NYC.ACS.CEO`$CitizenStatus)
`2013.NYC.ACS.CEO`$WorkExpIndiv = as.character(`2013.NYC.ACS.CEO`$WorkExpIndiv)

`2013.NYC.ACS.CEO`$Boro[`2013.NYC.ACS.CEO`$Boro == 1] = "Bronx"
`2013.NYC.ACS.CEO`$Boro[`2013.NYC.ACS.CEO`$Boro == 2] = "Brooklyn"
`2013.NYC.ACS.CEO`$Boro[`2013.NYC.ACS.CEO`$Boro == 3] = "Manhattan"
`2013.NYC.ACS.CEO`$Boro[`2013.NYC.ACS.CEO`$Boro == 4] = "Queens"
`2013.NYC.ACS.CEO`$Boro[`2013.NYC.ACS.CEO`$Boro == 5] = "Staten Island"

`2013.NYC.ACS.CEO`$HousingStatus[`2013.NYC.ACS.CEO`$HousingStatus == 1] = "Public Housing"
`2013.NYC.ACS.CEO`$HousingStatus[`2013.NYC.ACS.CEO`$HousingStatus == 2] = "Mitchell Lama"
`2013.NYC.ACS.CEO`$HousingStatus[`2013.NYC.ACS.CEO`$HousingStatus == 3] = "Subsidized"
`2013.NYC.ACS.CEO`$HousingStatus[`2013.NYC.ACS.CEO`$HousingStatus == 4] = "Rent Regulated"
`2013.NYC.ACS.CEO`$HousingStatus[`2013.NYC.ACS.CEO`$HousingStatus == 5] = "Other Regulated"
`2013.NYC.ACS.CEO`$HousingStatus[`2013.NYC.ACS.CEO`$HousingStatus == 6] = "Market"
`2013.NYC.ACS.CEO`$HousingStatus[`2013.NYC.ACS.CEO`$HousingStatus == 7] = "No Cash"
`2013.NYC.ACS.CEO`$HousingStatus[`2013.NYC.ACS.CEO`$HousingStatus == 8] = "Fully Owned"
`2013.NYC.ACS.CEO`$HousingStatus[`2013.NYC.ACS.CEO`$HousingStatus == 9] = "Owned Mortgage"

`2013.NYC.ACS.CEO`$Ethnicity[`2013.NYC.ACS.CEO`$Ethnicity == 1] = "White"
`2013.NYC.ACS.CEO`$Ethnicity[`2013.NYC.ACS.CEO`$Ethnicity == 2] = "Black"
`2013.NYC.ACS.CEO`$Ethnicity[`2013.NYC.ACS.CEO`$Ethnicity == 3] = "Asian"
`2013.NYC.ACS.CEO`$Ethnicity[`2013.NYC.ACS.CEO`$Ethnicity == 4] = "Hispanic"
`2013.NYC.ACS.CEO`$Ethnicity[`2013.NYC.ACS.CEO`$Ethnicity == 5] = "Other"

`2013.NYC.ACS.CEO`$EducAttain[`2013.NYC.ACS.CEO`$EducAttain == 1] = "No HS"
`2013.NYC.ACS.CEO`$EducAttain[`2013.NYC.ACS.CEO`$EducAttain == 2] = "HS"
`2013.NYC.ACS.CEO`$EducAttain[`2013.NYC.ACS.CEO`$EducAttain == 3] = "Some College"
`2013.NYC.ACS.CEO`$EducAttain[`2013.NYC.ACS.CEO`$EducAttain == 4] = "College or More"

`2013.NYC.ACS.CEO`$CitizenStatus[`2013.NYC.ACS.CEO`$CitizenStatus == 1] = "Citizen By Birth"
`2013.NYC.ACS.CEO`$CitizenStatus[`2013.NYC.ACS.CEO`$CitizenStatus == 2] = "Naturalized"
`2013.NYC.ACS.CEO`$CitizenStatus[`2013.NYC.ACS.CEO`$CitizenStatus == 3] = "Not A Citizen"

`2013.NYC.ACS.CEO`$WorkExpIndiv[`2013.NYC.ACS.CEO`$WorkExpIndiv == 1] = "Full Time"
`2013.NYC.ACS.CEO`$WorkExpIndiv[`2013.NYC.ACS.CEO`$WorkExpIndiv == 2] = "Part Time"
`2013.NYC.ACS.CEO`$WorkExpIndiv[`2013.NYC.ACS.CEO`$WorkExpIndiv == 3] = "No Work"

`2013.NYC.ACS.CEO`$EducAttain[`2013.NYC.ACS.CEO`$Povrel == 1] = "Head"
`2013.NYC.ACS.CEO`$EducAttain[`2013.NYC.ACS.CEO`$Povrel == 2] = "Spouse/Partner"
`2013.NYC.ACS.CEO`$EducAttain[`2013.NYC.ACS.CEO`$Povrel == 3] = "Child"
`2013.NYC.ACS.CEO`$EducAttain[`2013.NYC.ACS.CEO`$Povrel == 4] = "Other"

Povtable = table(`2013.NYC.ACS.CEO`$CEO_Poverty)
Povrate = Povtable[1]/Povtable[2]
Povrate

#Let's break our data set up into respondents classified as "Poor" or "Not Poor".

CEOPoor = subset(`2013.NYC.ACS.CEO`, CEO_Poverty == 1)
CEOSafe = subset(`2013.NYC.ACS.CEO`, CEO_Poverty == 2)
SafeUnits = sum(CEOSafe$Povunit) #The number of not poor units in our survey.
PoorUnits = sum(CEOPoor$Povunit) #The number of poor units in our survey.
CEOPovRate = PoorUnits/(PoorUnits + SafeUnits)
CEOPovRate

min(CEOPoor$CEO_Income)

CEOIncPov = cbind.data.frame(`2013.NYC.ACS.CEO`$SERIALNO, `2013.NYC.ACS.CEO`$CEO_Income, `2013.NYC.ACS.CEO`$CEO_Threshold, `2013.NYC.ACS.CEO`$CEO_Poverty, `2013.NYC.ACS.CEO`$Official_Poverty, `2013.NYC.ACS.CEO`$PreTaxIncome, `2013.NYC.ACS.CEO`$Official_Threshold, `2013.NYC.ACS.CEO`$Housing, `2013.NYC.ACS.CEO`$Childcare, `2013.NYC.ACS.CEO`$Commuting, `2013.NYC.ACS.CEO`$MOOP, `2013.NYC.ACS.CEO`$HEAP, `2013.NYC.ACS.CEO`$WIC, `2013.NYC.ACS.CEO`$FoodStamps, `2013.NYC.ACS.CEO`$SchoolLunch, `2013.NYC.ACS.CEO`$PovunitType) 

#These variable names are very long. Let's make them more succinct.

colnames(CEOIncPov) = c("IncPovSerial", "CEO_Inc", "CEO_Thresh", "Poor", "Off_Poor", "Pretax", "Off_Thresh", "Housing", "Childcare", "Commuting", "MOOP", "HEAP", "WIC", "FoodStamps", "School_Lunch", "Pov_Unit")

#Now let's make variables of subsetted poor and safe households of these.

SimpCEOPoor = subset(CEOIncPov, Poor == 1)
SimpCEOSafe = subset(CEOIncPov, Poor == 2)

#The time has come to aggregate these subsets so we can look at household level data!

AggSimpCEOPoor = aggregate(cbind(CEO_Inc, CEO_Thresh, Poor, Off_Poor, Pretax, Off_Thresh, Housing, Childcare, Commuting, MOOP, HEAP, WIC, FoodStamps, School_Lunch, Pov_Unit)~IncPovSerial, data=SimpCEOPoor, FUN=mean)

AggSimpCEOSafe = aggregate(cbind(CEO_Inc, CEO_Thresh, Poor, Off_Poor, Pretax, Off_Thresh, Housing, Childcare, Commuting, MOOP, HEAP, WIC, FoodStamps, School_Lunch, Pov_Unit)~IncPovSerial, data=SimpCEOSafe, FUN=mean)

MinPoor = AggSimpCEOPoor[which(AggSimpCEOPoor$CEO_Inc == min(AggSimpCEOPoor$CEO_Inc)),]
MinPoor[c("CEO_Inc", "Pretax", "MOOP", "FoodStamps", "HEAP", "Off_Thresh")]
ResultPoor = MinPoor["Pretax"] - MinPoor["MOOP"] + MinPoor["FoodStamps"] + MinPoor["HEAP"]
ResultPoor

MaxPoor = AggSimpCEOPoor[which(AggSimpCEOPoor$CEO_Inc == max(AggSimpCEOPoor$CEO_Inc)),]
MaxPoor[c("CEO_Inc", "CEO_Thresh", "Pretax", "Off_Thresh", "MOOP", "Housing", "Commuting", "WIC", "School_Lunch", "FoodStamps", "HEAP", "Pov_Unit")]

Official.Pov = rbind(subset(AggSimpCEOPoor, Off_Poor == 1), subset(AggSimpCEOSafe, Off_Poor == 1))

Rescued.Off.Pov = subset(Official.Pov, CEO_Inc > Off_Thresh)

nrow(Rescued.Off.Pov)/nrow(Official.Pov)

HRescued.Off.Pov = subset(Official.Pov, CEO_Inc+MOOP > Off_Thresh) #The same variable as above, except with Medical Out Of Pocket Expenses added back into the CEO's reported income. 
nrow(HRescued.Off.Pov)/nrow(Official.Pov)

Indi.Off.Pov = subset(`2013.NYC.ACS.CEO`, Official_Poverty == 1) #We need to capture individual level data for this part of the analysis, so we're back to pulling data from the original frame - no aggregation.

NoRescued.Off.Pov = subset(Indi.Off.Pov, CEO_Income+MOOP+Commuting < Official_Threshold) #Let's leave large Medical Expenses and Commuting expenses out of the analysis. We'll keep other expenses because some benefits are meant to offset those anyway.

Indi.Rescued.Off.Pov = subset(Indi.Off.Pov, CEO_Income+MOOP+Commuting > Official_Threshold)

ChronPoorDemos = table(NoRescued.Off.Pov$Boro, NoRescued.Off.Pov$HousingStatus, NoRescued.Off.Pov$Ethnicity)

SumsOfChronPoor = addmargins(ChronPoorDemos, FUN=sum)
SumsOfChronPoor

ObsRaceDist = c(867, 1306, 1612, 168, 1578) #The racial distribution of those not lifted by benefits.
PopRaceDist = c(9858, 14425, 16283, 1962, 24422)/66950 #The racial proportions of the survey respondents.

chisq.test(ObsRaceDist,p=PopRaceDist)

table(`2013.NYC.ACS.CEO`$Ethnicity)/66950
table(NoRescued.Off.Pov$Ethnicity)/5531

ObsBoroDist = c(1041, 2143, 617, 1466, 264)
PopBoroDist = c(9828, 23880, 8423, 20780, 4039)/66950
chisq.test(ObsBoroDist,p=PopBoroDist)
table(`2013.NYC.ACS.CEO`$Boro)/66950
table(NoRescued.Off.Pov$Boro)/5531
#Bronx and Brooklyn residents more likely than population.
ObsHouseDist = c(343, 1678, 57, 91, 141, 773, 291, 1440, 717)
PopHouseDist = c(9483, 14467, 481, 1042, 1044, 19280, 2852, 13934, 4367)/66950
chisq.test(ObsHouseDist,p=PopHouseDist)
table(`2013.NYC.ACS.CEO`$HousingStatus)/66950
table(NoRescued.Off.Pov$HousingStatus)/5531
# Market, Mitchell Lama, Other Regulated, Rent Regulated, and Subsidized more likely than population.
ObsEducDist = c(855, 1180, 2318, 960)
PopEducDist = c(18243, 12859, 21191, 12368)/64661
chisq.test(ObsEducDist,p=PopEducDist)
table(`2013.NYC.ACS.CEO`$EducAttain)/64661
table(NoRescued.Off.Pov$EducAttain)/5313
# HS, No HS, are more likely than population.
ObsCitzDist = c(3379, 878, 1274)
PopCitzDist = c(41923, 14814, 10213)/66950
chisq.test(ObsCitzDist,p=PopCitzDist)
table(`2013.NYC.ACS.CEO`$CitizenStatus)/66950
table(NoRescued.Off.Pov$CitizenStatus)/5531
# More likely to not be a citizen.
ObsWorkDist = c(314, 4026, 1191)
PopWorkDist = c(22177, 32619, 12154)/66950
chisq.test(ObsWorkDist,p=PopWorkDist)
table(`2013.NYC.ACS.CEO`$WorkExpIndiv)/66950
table(NoRescued.Off.Pov$WorkExpIndiv)/5531
#Less likely to hold full time employment.
#Testing the demos of those not rescued from benefits against the total officially poor.
OffPoorEthnDist = c(1789, 2797, 4283, 297, 2869)/12035 #Figures derived from a table of the "Indi.Off.Pov." variable
chisq.test(ObsRaceDist, p=OffPoorEthnDist)
table(NoRescued.Off.Pov$Ethnicity)/5531
table(Indi.Off.Pov$Ethnicity)/12053
# Non rescued more likely than the total poor to be Asian, White, or 'Other', less likely to be Latino.
OffPoorBoroDist = c(2743, 4769, 1403, 2676, 444)/12035
chisq.test(ObsBoroDist, p=OffPoorBoroDist)
table(NoRescued.Off.Pov$Boro)/5531
table(Indi.Off.Pov$Boro)/12035
#Non rescued more likely than the total poor to be from the Bronx and Brooklyn, less likely to be from Queens or Staten Island
OffPoorEduDist = c(1290, 2379, 6024, 1777)/11470
chisq.test(ObsEducDist, p=OffPoorEduDist)
#Non rescued more likely than the total poor to have a high school or above education and less likely to have no high school education
OffPoorCitzDist = c(7503, 2036, 2496)/12035
chisq.test(ObsCitzDist,p=OffPoorCitzDist)
table(NoRescued.Off.Pov$CitizenStatus)/5531
table(Indi.Off.Pov$CitizenStatus)/12035
#Non rescued more likely than the total poor to not be citizens
OffPoorWorkExpDist = c(855, 8960, 2220)/12035
chisq.test(ObsWorkDist,p=OffPoorWorkExpDist)
table(NoRescued.Off.Pov$WorkExpIndiv)/5531
table(Indi.Off.Pov$WorkExpIndiv)/12035
#Overwhelmingly likely to have no work, but marginally more likely than regular poor population to have a full time job.

#T Testing the mean incomes of the total poor and chronically poor, before and after benefits, to see if there's a statistically significant difference.

mean(`2013.NYC.ACS.CEO`$PreTaxIncome)
mean(Official.Pov$Pretax)
mean(AggSimpCEOPoor$Pretax)
mean(Indi.Rescued.Off.Pov$PreTaxIncome)
mean(NoRescued.Off.Pov$PreTaxIncome)

# Computing the T Statistic of a Mean

meanTtest = function(x, m, s, n){ # x = observed mean of sample, m = estimated mean of the population,s = standard error, n = number of observations
t = (x - m)/(s/sqrt(n))
DoT = pt(t, df=n-1) # DoT = Distribution function of T
return(DoT)
}

sd(NoRescued.Off.Pov$PreTaxIncome) # SD is huge. Checking next summary statistics of this data.
summary(NoRescued.Off.Pov$PreTaxIncome)
boxplot(NoRescued.Off.Pov$PreTaxIncome) # Looks like many outliers potentially above $20k income according to the box plot.

NoOutsNoResOffPov = subset(NoRescued.Off.Pov, PreTaxIncome < 25000) #Gets rid of a majority of the outliers
boxplot(NoOutsNoResOffPov)

OPM = mean(Indi.Off.Pov$PreTaxIncome) #Comparing individuals.
ROPM = mean(Indi.Rescued.Off.Pov$PreTaxIncome)
x = mean(NoOutsNoResOffPov$PreTaxIncome)
s = sd(NoOutsNoResOffPov$PreTaxIncome)
n = nrow(NoOutsNoResOffPov)
meanTtest(x = x, m = OPM, s = s, n = n)
meanTtest(x = x, m = ROPM, s = s, n = n)

# Are total benefits larger or smaller on average for individuals not lifted above the poverty line by them?

MeanBenefitsRes = sum(mean(Indi.Rescued.Off.Pov$HEAP), mean(Indi.Rescued.Off.Pov$WIC), mean(Indi.Rescued.Off.Pov$FoodStamps), mean(Indi.Rescued.Off.Pov$SchoolLunch), mean(Indi.Rescued.Off.Pov$SchoolBreakfast))
MeanBenefitsNoRes = sum(mean(NoOutsNoResOffPov$HEAP), mean(NoOutsNoResOffPov$WIC), mean(NoOutsNoResOffPov$FoodStamps), mean(NoOutsNoResOffPov$SchoolLunch), mean(NoOutsNoResOffPov$SchoolBreakfast))
MeanBenefitsRes
MeanBenefitsNoRes

# Are the results significant?

sum(var(NoOutsNoResOffPov$HEAP), var(NoOutsNoResOffPov$WIC), var(NoOutsNoResOffPov$FoodStamps), var(NoOutsNoResOffPov$SchoolLunch), var(NoOutsNoResOffPov$SchoolBreakfast))
SDBenefitsNoRes = sqrt(6641635/5)
# Took the square root of the average variance of all the benefits to estimate the standard error.
meanTtest(x=MeanBenefitsNoRes, m=MeanBenefitsRes, s=SDBenefitsNoRes, n=n)

# Does poverty unit family composition have anything to do with this?

table(NoRescued.Off.Pov$Povrel)/nrow(NoRescued.Off.Pov)
table(Indi.Rescued.Off.Pov$Povrel)/nrow(Indi.Rescued.Off.Pov)

c(mean(NoOutsNoResOffPov$HEAP), mean(NoOutsNoResOffPov$WIC), mean(NoOutsNoResOffPov$FoodStamps), mean(NoOutsNoResOffPov$SchoolLunch), mean(NoOutsNoResOffPov$SchoolBreakfast))
c(mean(Indi.Rescued.Off.Pov$HEAP), mean(Indi.Rescued.Off.Pov$WIC), mean(Indi.Rescued.Off.Pov$FoodStamps), mean(Indi.Rescued.Off.Pov$SchoolLunch), mean(Indi.Rescued.Off.Pov$SchoolBreakfast))

table(Indi.Rescued.Off.Pov$CitizenStatus)/nrow(Indi.Rescued.Off.Pov)
table(NoRescued.Off.Pov$CitizenStatus)/nrow(NoRescued.Off.Pov)
