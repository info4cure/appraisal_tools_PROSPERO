#' --- 
#' title: "Preferential usage patterns of methodological quality and bias risk assessment tools of non-Cochrane systematic reviews: A meta-epidemiological study of 20,000 PROSPERO records" 
#' author: "Juan Ruano" 
#' date: "15 April 2018" 
#' institutions: Department of Dermatology, IMIBIC/Reina Sofia University Hospital/University of Cordoba, Cordoba, Spain
#' analysis: 1: NETWORK PLOT;2: NETWORK ARCHITECTURE ANALYSIS;3: NETWORK BY AMSTAR quality level  
#' --- 
#' 
# R version 3.4.4 (2018-03-15)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS High Sierra 10.13.1


# set working directory
setwd("~/Documents/data_analisis_RoB_PROSPERO/PROSPERO_RoB_tools")

# Import data
Id_fechas_plus_RoB_tools_tidy <- read_delim("Id_fechas_plus_RoB_tools_tidy.csv", ";", escape_double = FALSE, 
                                            col_types = cols(AGREEII = col_factor(levels = c("NO","YES")), 
                                                             AMSTAR = col_factor(levels = c("NO","YES")), 
                                                             AQUA = col_factor(levels = c("NO", "YES")), 
                                                             ARRIVE = col_factor(levels = c("NO", "YES")), 
                                                             `Anticipated completion date` = col_date(format = "%d/%m/%Y"),
                                                             `Anticipated or actual start date` = col_date(format = "%d/%m/%Y"),
                                                             `BMJ checklist` = col_factor(levels = c("NO", "YES")), 
                                                             CAMARADES = col_factor(levels = c("NO","YES")), 
                                                             CASP = col_factor(levels = c("NO", "YES")), 
                                                             `CHEC-list` = col_factor(levels = c("NO","YES")),  
                                                            CHEERS = col_factor(levels = c("NO", "YES")), 
                                                           `CLEAR NPT` = col_factor(levels = c("NO","YES")), 
                                                           `CMS/MCMS` = col_factor(levels = c("NO","YES")), 
                                                           `COCHRANE RISK OF BIAS` = col_factor(levels = c("NO","YES")), 
                                                           CONSORT = col_factor(levels = c("NO", "YES")), 
                                                           COREQ = col_factor(levels = c("NO","YES")), 
                                                           `COSMIN CHECLIST` = col_factor(levels = c("NO", "YES")), 
                                                           `Date of publication of this version` = col_date(format = "%d/%m/%Y"),
                                                           `Date of registration in PROSPERO` = col_date(format = "%d/%m/%Y"),
                                                           `Downs and Black` = col_factor(levels = c("NO","YES")), 
                                                           `Drummond checklist` = col_factor(levels = c("NO","YES")), 
                                                           EPHPP = col_factor(levels = c("NO", "YES")),  
                                                           GRACE = col_factor(levels = c("NO","YES")),
                                                           GRADE = col_factor(levels = c("NO", "YES")), 
                                                           ISPOR = col_factor(levels = c("NO","YES")), 
                                                           Id = col_character(),
                                                           Id_1 = col_character(), 
                                                           JADAD = col_factor(levels = c("NO","YES")), 
                                                           JBI = col_factor(levels = c("NO","YES")), 
                                                           LOE = col_factor(levels = c("NO","YES")), 
                                                           MCMASTER = col_factor(levels = c("NO", "YES")), 
                                                           MERSQI = col_factor(levels = c("NO", "YES")), 
                                                           MINORS = col_factor(levels = c("NO", "YES")), 
                                                           MMAT = col_factor(levels = c("NO", "YES")), 
                                                           MOOSE = col_factor(levels = c("NO", "YES")), 
                                                           `NEWCASTLE-OTTAWA` = col_factor(levels = c("NO","YES")), 
                                                           `OCEBM/CEBM` = col_factor(levels = c("NO","YES")), 
                                                           OQAQ = col_factor(levels = c("NO","YES")), 
                                                           `PEDRO SCALE` = col_factor(levels = c("NO","YES")), 
                                                           PRISMA = col_factor(levels = c("NO","YES")), 
                                                           `Phillips checklist` = col_factor(levels = c("NO","YES")), 
                                                           `Q-COH` = col_factor(levels = c("NO","YES")), 
                                                           QATSDD = col_factor(levels = c("NO","YES")), 
                                                           QATSO = col_factor(levels = c("NO","YES")), 
                                                           `QUADAS/QUADAS-2` = col_factor(levels = c("NO","YES")), 
                                                           QUIPS = col_factor(levels = c("NO","YES")), 
                                                           QUOROM = col_factor(levels = c("NO","YES")), 
                                                           `RE-AIM` = col_factor(levels = c("NO","YES")), 
                                                           REBIP = col_factor(levels = c("NO", "YES")), 
                                                           `ROBINS-I` = col_factor(levels = c("NO","YES")), 
                                                           ROBIS = col_factor(levels = c("NO","YES")), 
                                                           RoBANS = col_factor(levels = c("NO","YES")), 
                                                           SAQOR = col_factor(levels = c("NO","YES")), 
                                                           SEQES = col_factor(levels = c("NO","YES")), 
                                                           `SIGN / SIGN-50` = col_factor(levels = c("NO","YES")), 
                                                           SRQR = col_factor(levels = c("NO","YES")), 
                                                           STARD = col_factor(levels = c("NO","YES")), 
                                                           STROBE = col_factor(levels = c("NO","YES")), 
                                                           year = col_factor(levels = c("2010","2011", "2012", "2013", "2014","2015", "2016", "2017", "2018", "2019"))), trim_ws = TRUE)



# Tidy dataset


# Descriptive analysis


######## frequency heatmap

install.packages("dplyr")
install.packages("tidyverse")

library(ggplot2)
require(dplyr)
library(reshape2)
library(tidyverse)


freq_heatmap.data <- subset(Id_fechas_plus_RoB_tools_tidy, select = c(year, Country, AGREEII:STROBE))
db_1 <- gather(data = freq_heatmap.data[,-2], 
             AGREEII:STROBE,
             key = "tool",
             value = cases)
db_1_YES<-subset(db_1, cases=="YES" | cases=="yes")

db_1_YES$year<-as.factor(db_1_YES$year)
db_1_YES$cases<-as.factor(db_1_YES$cases)

res <- db_1_YES %>% 
  group_by(year) %>% 
  summarise(Freq=n()
  )

ggplot(data = db_1_YES) +
  geom_count(mapping = aes(x = year, y = tool, size = ..prop.., group = res$Freq))+
  theme_bw()

#  count of protocols  including any tool per year
res_year <- db_1_YES %>% 
      group_by(year) %>% 
      summarise(Freq=n()
                )

#  count of protocols including per tool
res_tool <- db_1_YES %>% 
       group_by(tool) %>% 
      summarise(Freq=n()
  )
arrange(res_tool, desc(Freq))


#  differences of countries between yes and no tools
db_country <- gather(data = freq_heatmap.data, 
               AGREEII:STROBE,
               key = "tool",
               value = cases)
db_country_YES<-subset(db_country, cases=="YES")
db_country_NO<-subset(db_country, cases=="NO")

res_country_YES <- db_country_YES %>% 
  group_by(Country) %>% 
  summarise(Freq=n()
  )
arrange(res_country_YES, desc(Freq))


res_country_NO <- db_country_NO %>% 
  group_by(Country) %>% 
  summarise(Freq=n()
  )
arrange(res_country_NO, desc(Freq))



school.freq <- table(na.omit(db_1_YES))
school.relfreq = school.freq *100/ nrow(freq_heatmap.data)
db_2_YES <- gather(data = , 
               AGREEII:STROBE,
               key = "tool") 

as.data.frame(school.relfreq) %>%  
  ggplot(mapping = aes(x = year, y = tool)) +
  geom_point(mapping = aes(fill = Freq))



# Set columns ready for ggplot
df$Freq<-as.factor(df$Freq)
df$charCount<-as.character(df$charCount) %>% as.numeric()
df$wordCount<-as.character(df$wordCount) %>% as.numeric()

# plot using ggplot
ggplot(df,aes(x=charCount,y=wordCount)) +
  geom_tile(aes(fill=Freq)) +
  geom_text(aes(label=Freq))
