library(car)
library(tidyverse)
Data <- read.csv("Data.csv", header= TRUE)
Data <- Data %>% dplyr::select(ResponseId, Q1:Q8.1_8)
Questions <- Data[1,]
Responses <- Data[-c(1,2),]

#write.csv(t(Questions), file="Questions.csv")


# function to drop excess levels for Likert-scale variables and convert to 1-5
Recode_Scale <- function(vec){
  vec <- factor(vec)
  vec <- vec %>% droplevels()
  vec <- recode(vec, `Strongly Disagree` = "1", `Disagree` = "2", `Neither Agree nor Disagree`="3", 
                `Agree`="4", `Strongly Agree`="5")
  vec <- as.numeric(as.character(vec))
  return(vec)
}


#Function to convert to numeric for Q8
Recode_Scale2 <- function(vec){
  vec <- factor(vec)
  vec <- vec %>% droplevels()
  vec <- recode(vec,   
  `I consider men of this group to be very unattractive.`="1",
  `I consider men of this group to be less attractive than those of other groups.`="2", 
  `I consider men of this group to be neither attractive nor unattractive.` ="3",
  `I consider men of this group to be more attractive than those of other groups.` = "4", 
  `I consider men of this group to be very attractive.` = "5"
  )
  vec <- as.numeric(as.character(vec))
  return(vec)
}

#function to drop extraneous levels of factor variables
Drop_Levels <- function(vec){
  vec <- factor(vec)
  vec <- vec %>% droplevels()
  return(vec)
}


#Convert questions (9.1_1 through 8.1_8) to scale 1-5 
Responses[, 3:38] <- apply(Responses[, 3:38], MARGIN=2, FUN=Drop_Levels)
Responses[,47:68] <- apply(X=Responses[,47:68], MARGIN=2, FUN=Recode_Scale)
Responses[, 39:46] <- apply(X=Responses[,39:46], MARGIN=2, FUN=Recode_Scale2)
Responses <- Responses %>% mutate_all(as.factor)

# need to reverse code 9.1.4, 9.1.5, 9.1.7, 9.1.8, 9.1.10, 9.1.14

#function to reverse-code questions
Reverse_Code <- function(Question){
  Question <- 6-Question
  return(Question)
}

Responses[, 39:68] <- apply(Responses[, 39:68], 2, as.character)
Responses[, 39:68] <- apply(Responses[, 39:68], 2, as.numeric)


# High values on QDI should indicate more openness
Responses$Q9.1_4 <- Reverse_Code(Responses$Q9.1_4)
Responses$Q9.1_5 <- Reverse_Code(Responses$Q9.1_5)  #wasn't reverse-coded in original paper. seems like it should be. 
Responses$Q9.1_7 <- Reverse_Code(Responses$Q9.1_7)
Responses$Q9.1_8 <- Reverse_Code(Responses$Q9.1_8)
Responses$Q9.1_10 <- Reverse_Code(Responses$Q9.1_10)
Responses$Q9.1_14 <- Reverse_Code(Responses$Q9.1_14)

#High values on OSR Indicate less openness
Responses$Q8.1_3 <- Reverse_Code(Responses$Q8.1_3)
Responses$Q8.1_4 <- Reverse_Code(Responses$Q8.1_4)  #3 and 4 are same question. Ask about this. 
Responses$Q8.1_5 <- Reverse_Code(Responses$Q8.1_5)

#Questions Q8.1_3 are same. Take average
Responses$Q8.1_3 <- (Responses$Q8.1_3 + Responses$Q8.1_4)/2

#for Q9- QDI, higher values indicate more openness 
#for Q8-ORS, higher values indicate less openness
# code below reverses QDI (question 9 answers) so higher values indicate less openness

#Calculate QDI score out of 70
Responses$QDI <- apply(Responses[, 47:60], 1, sum) 
#Calculate OSR score out of 35
Responses$OSR <- apply(Responses[, c(61:63, 65:68)], 1, sum) #don't include 64 since it was the same question as 63 and we averaged them. The average is now in place of Q63.

## At this point high values indicate more openness in QDI and less in OSR

## OSR is scored 0-4, rather than 1-5 for some reason. We'll switch to 0-4 to be consistent with original paper. 
Responses$OSR <- Responses$OSR - 7

summary(Responses$QDI)
summary(Responses$OSR)

############################################################################################

#create more descriptive names for questions that make up explanatory variables
names(Responses)[1:38] <- c("ID", "Disclaimer", "Age", "Gender", "Gender_Text", "US", 
                      "Profile", "Hear_of_Study", "Hear_of_Study_Text", "Education", 
                      "Education_Text", "Ethnic_Background", "Ethnic_Text", "Relationship",
                      "Relationship_Text", "Sexuality", "Sexuality_Text", "HIV", 
                      "Broad_Eth", "Broad_Eth_Text", "Websites", "Website_Frequency", 
                      "Website_Frequency_Text", "Freq_Sex_through_websites", 
                      "Internet_most_common", "Profile_Info_Type", "Profile_Indicate_Ethnic_Group",
                      "Ethnic_Most_Interested_In","Profile_Indicates_Not_Interested", "Ethnic_Groups_Not_Interested",
                      "Ethnic_most_reflected", "Ethnic_more_likely_network",
                      "Ethnic_less_likely_network", "Ethnic_easiest", "Ethnic_hardest",
                      "Ethnic_less_included", "Experienced_Sexual_Racism", "Viewed_Discriminating_Profile")

###################################################################################################

###############################################################################################################################
# Combine/consolidate categories
Responses$University <- factor(Responses$Education %in% c("University Degree/Diploma", "Postgraduate Degree"))
Responses$HIVPositive <- Responses$HIV %in% c("HIV-Positive", "HIV-Positive undetectable")
Responses$In_Relationship <- Responses$Relationship == "In a relationship"
Responses$Own_Profile_Discriminates <- Responses$Profile_Indicate_Ethnic_Group == "Yes" | Responses$Profile_Indicates_Not_Interested == "Yes"
Responses$Use_Website_Monthly_or_more_Frequently <- !(Responses$Website_Frequency %in% c("Less than once per month", "Not at all"))
Responses$Use_Website_for_Sex_Frequently <- !(Responses$Website_Frequency %in% c("Not at all", "Once or twice a year")) 
Responses$Weekly_Freq_Sex_through_websites <- !(Responses$Freq_Sex_through_websites %in% c( "Never", "Once or twice a year"))
Responses$Ethnic_Background <-Responses$Ethnic_Background %>% fct_collapse(Asian =c("Central Asian (e.g., Russian)", 
                                                                                    "East Asian (e.g., Chinese, Japanese)",
                                                                                    "Pacific Islander",
                                                                                    "Southeast Asian (e.g., Malaysian, Thai, Vietnamese)",
                                                                                    "South Asian (e.g., Indian/Bangladeshi/Pakistani)"
                                                                                    ),
                                                                           Other_Mixed = c("Mixed", "Other"))
Responses$Black <- Responses$Ethnic_Background == "Black/Caribbean/African"
Responses$Asian <- Responses$Ethnic_Background == "Asian"
Responses$Indigenous <- Responses$Ethnic_Background == "Native/Indigenous American"
Responses$Latino <- Responses$Ethnic_Background == "Latino/Non-White Hispanic"
Responses$`Middle Eastern` <- Responses$Ethnic_Background == "Middle Eastern"
Responses$`Southern European/Mediterranean` <- Responses$Ethnic_Background == "Southern European/Mediterranean"
Responses$White <- Responses$Ethnic_Background == "White/Caucasian/Anglo"
Responses$`Other/Mixed` <- Responses$Ethnic_Background == "Other_Mixed"
Responses$SexualIdentity <- as.character(Responses$Sexuality)
Responses$SexualIdentity[Responses$SexualIdentity=="Straight/Heterosexual"] <- "1Straight/Heterosexual" #want Straight/Heterosexual to show up first so it can be baseline group in regression
Responses$SexualIdentity <- factor(Responses$SexualIdentity)
summary(Responses$SexualIdentity)
#######################################################################################################################
#Investigate relationship between QDI and OSR scores

#Responses$QDIsc <- Responses$QDI/14 
#Responses$OSRsc <- Responses$OSR/7 

Responses$QDIsc <- scale(Responses$QDI)
Responses$OSRsc <- scale(Responses$OSR) 


cor(Responses$QDI, Responses$OSR) #positive correlation between QDI and OSR
summary(lm(Responses$OSR~Responses$QDI)) 
corlm<-summary(lm(Responses$OSR~Responses$QDI))
ggplot(data=Responses, aes(x=QDI, y=OSR)) + geom_point()+ geom_smooth(method= lm, se=FALSE) + ylab("Modified OSR")
#Relationship is "statistically significant" (p-value of 0.0000172)
#R^2=0.0167 indicates that 1.7% of variation in OSR score is explained by QDI score. 
# So, while there is a positive association between QDI and OSR scores it is a week one, and 
# there is a lot of variability in OSR scores not explained by QDI scores. 
######################################################################################################################

#Analysis using explanatory variables individually

#Age
Responses %>% group_by(Age)%>%summarize(MeanQDI=mean(QDI), MeanOSR=mean(OSR))
summary(lm(data=Responses, cbind(QDI, OSR)~-1+Age))
#pairwise.t.test(x=Responses$QDI, g=Responses$Age, p.adjust.method = "none")
#pairwise.t.test(x=Responses$OSR, g=Responses$Age, p.adjust.method = "none")

#Ethnic Background

# Do the factor lump just for the summary table, but keep all groups for the actual analysis
Responses <- Responses %>% mutate(Ethnic_Background_Lmp = fct_lump(Ethnic_Background, 4, other_level = "Other/Mixed"))
Responses <- Responses %>% mutate(SexualIdentity = fct_lump(SexualIdentity, 3, other_level = "Other"))


Responses %>% group_by(Ethnic_Background_Lmp)%>%summarize(MeanQDI=mean(QDI), MeanOSR=mean(OSR))
summary(lm(data=Responses, cbind(QDI, OSR)~-1+Ethnic_Background_Lmp))
#pairwise.t.test(x=Responses$QDI, g=Responses$Ethnic_Background, p.adjust.method = "none")
#pairwise.t.test(x=Responses$OSR, g=Responses$Ethnic_Background, p.adjust.method = "none")

# binary variables
summary(lm(data=Responses, cbind(QDI, OSR)~ University))
#summary(lm(data=Responses, cbind(QDI, OSR)~ Queer))
summary(lm(data=Responses, cbind(QDI, OSR)~ HIVPositive))
summary(lm(data=Responses, cbind(QDI, OSR)~ In_Relationship))
summary(lm(data=Responses, cbind(QDI, OSR)~ Experienced_Sexual_Racism))
summary(lm(data=Responses, cbind(QDI, OSR)~ Viewed_Discriminating_Profile))
summary(lm(data=Responses, cbind(QDI, OSR)~ Own_Profile_Discriminates))
summary(lm(data=Responses, cbind(QDI, OSR)~ Use_Website_for_Sex_Frequently))
summary(lm(data=Responses, cbind(QDI, OSR)~ Use_Website_Monthly_or_more_Frequently))

table(Responses$QDI<2.5, Responses$OSR>3)

table(Responses$QDI>4.5, Responses$OSR<3)




#multivariate regression model to investigate which explanatory variables are associated with QDI/OSR

Model <- lm(data=Responses, cbind(QDI, OSR) ~  Age + University + SexualIdentity + HIVPositive + In_Relationship + Experienced_Sexual_Racism + 
              Viewed_Discriminating_Profile + Own_Profile_Discriminates + Use_Website_Monthly_or_more_Frequently + Use_Website_for_Sex_Frequently + Ethnic_Background)
summary(Model)
library(car)
fit.manova <- Manova(Model)
fit.manova

#Include variables that show relationship, using Pillai test statistic (exclude HIV positive, use website monthly or more)
Model <- lm(data=Responses, cbind(QDI, OSR) ~  Age + University + SexualIdentity + In_Relationship + Experienced_Sexual_Racism + 
              Viewed_Discriminating_Profile + Own_Profile_Discriminates  + Use_Website_for_Sex_Frequently + Ethnic_Background)
summary(Model)

# Scaled version of scores
Model <- lm(data=Responses, cbind(QDIsc, OSRsc) ~  Age + University + SexualIdentity + In_Relationship + Experienced_Sexual_Racism + 
              Viewed_Discriminating_Profile + Own_Profile_Discriminates  + Use_Website_for_Sex_Frequently + Ethnic_Background)
summary(Model)
