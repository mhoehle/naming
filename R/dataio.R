######################################################################
## Author: Michael Höhle <http://www.math.su.se/~hoehle>
## Date:   2017-04-28, modified 2017-09-20 for 2016 data
##
## R code to download and read the 2015 or 2016 ONS baby names data
## from the ONS web site. The ONS data only contains names with more
## than two uses in the year. As a consequence, an inquiry with the
## ONS customer support was made to get the number of names occurring
## once and twice for girls and boys. We add these as artificial names
## to the regular ONS dataset.
######################################################################

library("dplyr")
library("readxl")
library("magrittr")
##Which year to analyze
year <- 2015
##year <- 2016

##Download (if necessary) and load boys and girls tables in order to
##join them into one table
newborn_not12 <- bind_rows(
  lapply( c(girls="girls",boys="boys"), function(sex) {
    ##Load CSV file
    destfile <- file.path("..","Data",paste0(year,sex,"namesfinal.xls"))
    if (!file.exists(destfile)) {
      filename <- paste0(year,sex,"names.xls")
      download.file(paste0("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/livebirths/datasets/babynamesenglandandwalesbabynamesstatistics",sex,"/",year,"/",filename),destfile=destfile)
    }
    df <- read_excel(path=destfile,sheet="Table 6",skip=4) %>% select(Rank,Name,Count)
    ##Remove empty rows and convert types
    df <- df %>% filter(!is.na(Name)) %>%
      mutate(Count=as.numeric(Count), Rank=as.numeric(Rank), Sex=sex, Name=gsub(" ", "", Name, fixed = TRUE))
    return(df)
  })
)

##Count totals for boys and girls in the 2015 data. Note: These data
##do not contain names with just 1 or 2 uses in that year
total_not12 <- newborn_not12 %>% group_by(Sex) %>% summarise(Count_not12=sum(Count))
total_not12

##Total number of boys & girls with names in the UK and Wales
##according to the textual info in the Sheet 6 of each Excel file
if (year == 2015) {
  total <- data.frame(sex2=c("boys","girls"), Count=c(358136 - 14,339716 - 10))
}
## 2016 info is in the bulletin https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/livebirths/bulletins/babynamesenglandandwales/2016#which-names-entered-the-top-100-in-2016. Number of kids without names
if (year == 2016) {
  total <- data.frame(sex2=c("boys","girls"), Count=c(357046 - 12, 339225 - 8))
}

##Deduce what's missing
missing <- bind_cols(total,total_not12) %>% mutate(missing=Count - Count_not12)
missing %>% select(Sex,missing)

if (year == 2015) {
  ##Data from Email VSOB customer service team (vsob@ons.gsi.gov.uk) on 2017-02-15
  ##names12 <- read.csv(file=file.path("..","Data","names-1or2occurrences.csv"))
  names12 <- read.csv(textConnection("What,Sex,Factor,Count
names_occurring_twice,boys,2,3050
names_occurring_once,boys,1,18741
names_occurring_twice,girls,2,3999
names_occurring_once,girls,1,23733"
))
}

if (year == 2016) {
  ##Data for 2016 of those with names occurring once and twice only are
  ##now given directly in the ONS bulletin https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/livebirths/bulletins/babynamesenglandandwales/2016#which-names-entered-the-top-100-in-2016
  names12 <- read.csv(textConnection("What,Sex,Factor,Count
names_occurring_twice,boys,2,6048
names_occurring_once,boys,1,19005
names_occurring_twice,girls,2,7874
names_occurring_once,girls,1,24198"
))
}


names12 <- names12 %>% mutate(TotalCount = Factor * Count)
names12
names12 %>% group_by(Sex) %>% summarise(n=sum(TotalCount))

newborn_12 <- names12 %>% group_by(Sex, Factor) %>% do( {
  data.frame( Rank=1e9, Name=paste0(toupper(.$Sex),"#",sprintf("%.5d",seq_len(.$Count)),"#f",.$Factor))
}) %>% rename(Count = Factor)

newborn <- bind_rows(newborn_not12, newborn_12)
##Check for doubles!!
stopifnot(newborn %>% group_by(Name,Sex) %>% summarise(n=n()) %>% filter(n>1) %>% nrow() == 0)

##Deduce empirical proportions per sex
newborn <- newborn %>% group_by(Name,Sex) %>% summarise(Count=sum(Count)) %>%
  mutate(p=Count/sum(Count))

##Store resulting data.frame for further analysis in the Rmd of the article
save(file=file.path("..","Data",paste0("newborn-",year,".RData")),list=c("newborn"))
