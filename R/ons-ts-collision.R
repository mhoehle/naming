######################################################################
## Author: Michael Höhle <http://www.math.su.se/~hoehle>
## Date:   2017-04-23
##
## Description:
##  Create bonus material plot containing the time series of the UK baby
##  name collision probability.
######################################################################

library("readxl")
library("dplyr")
library("ggplot2")
##devtools::install_github("hoehleatsu/birthdayproblem")
library("birthdayproblem")

######################################################################
## ONS Data files have to be manually downloaded from
## https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/livebirths/datasets/babynamesenglandandwalesbabynamesstatisticsgirls
## and
## https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/livebirths/datasets/babynamesenglandandwalesbabynamesstatisticsboys. Alternatively,
## one can run the following R code:
######################################################################
filenames <- readLines("filenames.txt")
for (filename in filenames) {
  ##Extract year and sex
  sex <- gsub("^([0-9]{4})(girls|boys).*","\\2",filename)
  year <- gsub("^([0-9]{4})(girls|boys).*","\\1",filename)
  ##Download the file if it doesn't exist
  destfile <- file.path("..","Data",filename)
  if (!file.exists(destfile)) {
    download.file(paste0("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/livebirths/datasets/babynamesenglandandwalesbabynamesstatistics",sex,"/",year,"/",filename),destfile=destfile)
  }
}

##Loop over all available files in the Data2 directory. Some warnings
##are to be expected, because the excel file contains text passages at
##the end informing about special removals etc.
names <- NULL
for (sex in c("Girls","Boys")) {
  files <- list.files(file.path("..","Data"), pattern=paste0("[0-9]{4}",tolower(sex)))
  for (i in seq_len(length(files))) {
    name <- files[i]
    filePath <- file.path("..","Data",name)
    ##Find name of the sheet containing the NAME & COUNT information
    sheets <- readxl::excel_sheets(path=filePath)
    sheetName <- grep(paste0(sex," names - E&W"),sheets, value=TRUE)
    if (length(sheetName) == 0) {
      sheetName <- tail(grep("Table [0-9]",sheets, value=TRUE),n=1)
    }

    x <- readxl::read_excel(path=filePath,sheet=sheetName,skip=4)
    x <- x %>% filter(!is.na(Rank)) %>% mutate(year=substr(name,1,4),sex=tolower(sex))
    names <- rbind(names, x)
  }
}

##Add empirical relative frequencies per year
names$Name <- gsub("[ ]+$","",names$Name)
names <- names %>% group_by(year) %>% mutate(p = Count/sum(Count))

######################################################################
##Compute and visualize the collision probablity
######################################################################

## Compute collision prob for different group sizes
collision <- names %>% group_by(year) %>% do({
  n <- 27L
  p <- sapply(n, function(n) birthdayproblem::pbirthday_up(n=n, .$p ,method="mase1992")$prob)
  data.frame(n=n, p=p)
})

##Make the time series plot
p <- ggplot(collision, aes(x=as.numeric(year),y=p)) + geom_line(size=1.2,color="lightsalmon2") + xlab("Time (year)") + ggtitle("Probability of a name clash in a group of 27 kids born in year YYYY in the UK and Wales") + ylab("Probability")+  scale_y_continuous(limits=c(0,1),labels=scales::percent) +  scale_colour_discrete(name  ="n") + geom_point(data=data.frame(year=2015,p=0.458),color="lightsalmon4") + scale_x_continuous(breaks=seq(min(as.numeric(collision$year)),max(as.numeric(collision$year)),2))
## Transparent background in ggplot - see http://stackoverflow.com/questions/31550435/issue-saving-r-plot-with-transparent-background
p + theme_bw() + theme(plot.background = element_rect(fill = "transparent",colour = NA))

##Store to file.
ggsave(filename="timeseries.png", dpi=300, width=8, height=4, bg = "transparent")


######################################################################
## Word clouds
######################################################################

require("wordcloud")
#source("mywordcloud.R") 

names2015 <- names %>% filter(year==2015)
boys2015 <- names %>% filter(year==2015, sex == "boys")
girls2015 <- names %>% filter(year==2015, sex == "girls")

# set.seed(123)
# pdf(file="wordcloud-girls.pdf",width=10,height=10)
# par(mar=c(0,0,0,0))
# pal <- brewer.pal(9, "PuRd")[-c(1:2)]
# wordcloud(girls2015$Name,girls2015$Count,colors=pal,min.freq=50,random.order=FALSE)
# dev.off()
# pdf(file="wordcloud-boys.pdf",width=10,height=10)
# par(mar=c(0,0,0,0))
# pal <- brewer.pal(9, "Blues")[-c(1:2)]
# wordcloud(boys2015$Name,boys2015$Count,colors=pal,min.freq=50,random.order=FALSE)
# dev.off()

set.seed(123)
png(file="wordclouds.png",width=800,height=400,res=72,bg = "transparent")
par(mar=c(0,0,0,0), mfcol=c(1,2))
pal <- brewer.pal(9, "PuRd")[-c(1:2)]
wordcloud(girls2015$Name,girls2015$Count,colors=pal,min.freq=50,random.order=FALSE)
pal <- brewer.pal(9, "Blues")[-c(1:2)]
wordcloud(boys2015$Name,boys2015$Count,colors=pal,min.freq=50,random.order=FALSE)
dev.off()



