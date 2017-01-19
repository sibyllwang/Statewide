###### source and formatting of the pop6015 dataset


## get rid of "," in population values and make values numeric
NST_EST2015_01[,2]<-as.numeric(gsub(",", "", NST_EST2015_01[,2]))
## write a function to rm comma and multiple by 1000
rmComma<-function(vec){as.numeric(gsub(",", "", vec))}
mul1000<-function(vec){vec*1000}


## get rid of "." in front of state names
NST_EST2015_01[,1]<-gsub("\\.", "", NST_EST2015_01[,1])

## getting all population values from df into a vector
## http://stackoverflow.com/questions/2545228/converting-a-dataframe-to-a-vector-by-rows
allPop1960_15<-as.vector(t(pop1960_15[,1:56]))

## create year and state variables using rep
state<-rep(pop1960_15[,58], each=56)
year<-rep(1960:2015, 52)

## df of pop 2010-15
pop6015<-data.frame(state=state, year=year, population=allPop1960_15)

## merge pop1015 with foods
## http://stackoverflow.com/questions/6709151/how-do-i-combine-two-data-frames-based-on-two-columns
## note this only preserves the data whose years appear in pop1015
temp<-merge(foods, pop6015, by=c("year", "state"))

## join 2000-2009 and 2010-2015 data
NST_EST2000_15<-left_join(NST_EST2015_01,NST_EST2009_01)

## sort by column values
NST_EST2000_15<-NST_EST2000_15[,sort(names(NST_EST2000_15))]


################# 1980s data
## 1980s data
## https://www.census.gov/popest/data/state/asrh/1980s/tables/st8090ts.txt
## save in a .txt file then read.table() from local .txt file

## make row names the first column
library(data.table)
pop19801<-setDT(pop19801, keep.rownames = TRUE)[]
pop19802<-setDT(pop19802, keep.rownames = TRUE)[]


##################1900-1990 population by state
##https://www.census.gov/popest/data/state/asrh/1980s/80s_st_totals.html







## 1990-2000 data 
## https://www.census.gov/popest/data/state/totals/1990s/tables/ST-99-03.txt
## http://www.census.gov/popest/data/intercensal/st-co/files/CO-EST2001-12-00.pdf

