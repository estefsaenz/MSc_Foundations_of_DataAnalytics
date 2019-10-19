setwd("C:/Users/estef/OneDrive/Warwick - MSc in Data Analytics/CS910 Foundations of Data Analytics/Final Project")
library(ggplot2)
library(plyr)
library(gridExtra)
library(Hmisc)
library(factoextra)
library(stringr)
library(data.table)
library(PerformanceAnalytics)
library(corrplot)

tema <- theme_bw() 
theme_set(tema)


business <- read.csv('Bussiness/businesses-in-london-1.csv')
data <- read.csv('london-borough-profiles.csv')

# table(unique(business$ward)  %in% unique(data$New.code))


# wards.n <- unique(business$ward)[!(unique(business$ward)%in% unique(data$New.code))]
# wards_codes <- read.csv('Bussiness/wards_codes.csv')
# names(wards_codes) <- c('ward','name','FID')

# table(wards.n %in% wards_codes$ward)

# Keep only valid London ward codes
# wards <- unique(business$ward)[unique(business$ward)  %in% unique(data$New.code)]
# city_london <- data$New.code[data$Ward.name == 'City of London']
# business$ward <- as.character(business$ward)
# business$ward[business$ward %in% city_london] <-  'E09000001'

names(business)[names(business) == 'laua'] <- 'Code'

business.1 <- join(business, data[,c('Code','Area_name')], by='Code')
types <- unique(business.1$SICCode.SicText_1 )


# write.csv(types, 'Bussiness/business-types.csv')

# Keep only Active Business (got rid of all companies with liquidity problems)

# "Active - Proposal to Strike off"  
# You can close down your limited company by getting it 'struck off' the Companies Register, but only if it:

# hasn't traded or sold off any stock in the last 3 months
# hasn't changed names in the last 3 months
# isn't threatened with liquidation
# has no agreements with creditors, eg a Company Voluntary Arrangement (CVA)

business.2 <- business.1[business.1$CompanyStatus == 'Active',]

# Just keep the variables we need
vars.int <- c('CompanyNumber', 'CompanyStatus','SICCode.SicText_1',
              'Code','Area_name', 'lat','long')
business.3 <- business.2[,vars.int]
names(business.3)[3] <- 'SIC_code'

# ============================
# BUSINESS TYPE HOMOLOGATION
# ============================


business.3$class <-  tolower(str_split_fixed(as.character(business.3$SIC_code), ' - ',2)[,2])
business.3$class<- gsub('[[:punct:] ]+','', business.3$class)

homo <- read.csv('Bussiness/homo_types.csv')
homo$class <- tolower(as.character(homo$class))
homo$class<- gsub('[[:punct:] ]+','',homo$class)

table(unique(business.3$class) %in% homo$class)
# unique(business.3$class)[!(business.3$class %in% homo$class)]

business.4 <- join(business.3, homo[,c('section', 'type', 'type_1', 'class')], by='class')

# Manually classify 200 SIC_codes with different names:

#temp <- unique(business.4[is.na(business.4$type),c('SIC_code','class')])
#write.csv(temp, 'Bussiness/classes-without-type-1.csv')

# Get rid of None-supplied SIC_code
business.5 <- business.4[business.4$SIC_code != 'None Supplied',]
table(is.na(business.5$type))

business.6 <- ddply(business.5, c('Code','Area_name','section','type'), summarise, n = length(CompanyNumber))

# names(data)[names(data) == 'New.code'] <- 'ward'
# data$Ward.name <- trimws(as.character(data$Ward.name), 'both')


temp <- ddply(business.5, 'type', summarise, n = length(CompanyNumber))
temp$type <- factor(temp$type, levels = temp$type[order(-temp$n)])

ggplot(temp, aes(x=type,y=n)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  theme(legend.position = "top") +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

temp <- ddply(business.5, c('Code','Area_name'), summarise, n = length(CompanyNumber))
temp$Area_name <- factor(temp$Area_name, levels = temp$Area_name[order(-temp$n)])

write.csv(temp, 'Bussiness/No_businesses.csv')

ggplot(temp, aes(x=Area_name, y=n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(legend.position = "top") +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())


# grid.arrange(g1, g2, nrow = 1)



#Standarize per Borough

bus.temp <- ddply(business.6, c('Code'), summarise, mean=mean(n), sd=sd(n))

business.7 <- join(business.6, bus.temp, by='Code')
business.7$n.int <- (business.7$n- business.7$mean)/business.7$sd

business.8 <- dcast(business.7, Code~type, value.var='n.int', fill=0)

write.csv(business.8, 'Bussiness/base.csv')

# =====
# SVM
# =====

# =
# Mutlicolitniarity
# =


my_data <- dcast(business.7, Code~section, value.var='n.int', fill=0)
chart.Correlation(my_data[,-1], histogram=TRUE, pch=19)

rquery.cormat(my_data[,-1])

vars.int <-  setdiff(names(business.8), c('Code','X'))

comps <- princomp(business.8[,vars.int])
summary(comps)
biplot(comps)
biplot(comps, expand=2)
biplot(comps, choices = c(3,4))
