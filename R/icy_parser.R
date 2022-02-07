#######################################################################
######### set working directory, this is where your data is ###########
#########    remember to change it every time you want to   ###########
#########                 process new data                  ###########
#######################################################################


setwd("/Volumes/iss01.haik-potier/alz_potier/commun/AMAL/Amal_Laura/Analyse-CathB-size4/Folders-LEA/2020.09.24/20200924-IHC H030-DS-D M15_results")


######################################################################
####### This will automatically install any library you need #########
######################################################################

librairies <- c(
        "readxl",
        "janitor",
        "stringr"
)


load_libraries <- function(librairies) { #function for loading of libraries
        for (l in librairies) {
                if (!(l %in% installed.packages()[, "Package"])) #if package not installed
                        utils::install.packages(l, type = "source")
                suppressPackageStartupMessages(library(
                        l,
                        character.only = TRUE,
                        warn.conflicts = FALSE,
                        quietly = TRUE
                ))
        }
}

load_libraries(librairies) 

rm(librairies,load_libraries)

######################################################################
######## First step: load all the dataframes to the space ############
######################################################################

temp = list.files(pattern="*.xls")


for (i in 1:length(temp)) {
        name<-temp[i]
        df<-read_excel(name)
        df<-janitor::clean_names(df) 
        if(str_detect(name, "CONTOUR")==TRUE){
                df$data<-rep("contour")
        }
        if(str_detect(name, "All-CathepsinB")==TRUE){
                df$data<-rep("all CATHB")
        }
        if(str_detect(name, "Exclusive-CathepsinB")==TRUE){
                df$data<-rep("excl CATHB")
        }
        if(str_detect(name, "MinusLipofuscin")==TRUE){
                df$data<-rep("minus LF")
        }
        if(str_detect(name, "ICY_Lipofuscin")==TRUE){
                df$data<-rep("LF")
        }
        name<-paste(name,collapse = "")
        assign(name,df)
        rm(df,name)
}

rm(i,temp)

rm(list=ls(pattern="negative")) #removes all files that include the name word negative

#####################################################################
######## Second step: put together those that are the same ##########
#####################################################################

list<-ls()
cells<-vector()

for(i in 1:length(list)){
        cell<-str_sub(list[i],14,39) #look for the length only the last number 
        cells<-c(cells,cell)
}

rm(cell,i)

cells<-unique(cells) #keeps unique identifiers, check that the length is equal to the number of cells if not redo from cells<-vector()

for(i in 1:length(cells)){
        df<-do.call(rbind, mget(ls(pattern = cells[i])))
        df$cell<-rep(cells[i])
        name<-paste(cells[i])
        assign(name,df)
        rm(df,name)
}

rm(list=ls(pattern=".xlsx"))

###################################################################
############## Third step: put everything together ################
###################################################################

dfs = sapply(.GlobalEnv, is.data.frame) 
data<-do.call(rbind, mget(names(dfs)[dfs]))

name<-str_sub(cells[1],1,14)

write.csv(data,file=paste(name,".csv"))

rm(list=ls())

