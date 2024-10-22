#######################################################################
######### set working directory, this is where your data is ###########
#########    remember to change it every time you want to   ###########
#########                 process new data                  ###########
#######################################################################

folder <- file.path(
    "Analyse-CathB-size4_2021.11.25",
    "Folders-LEA_previous-new-analysis",
    "2020.09.24",
    "20200924-IHC H030-DS-D M15_results"
)
paths <- file.path("inst", "extdata")

######################################################################
######## First step: load all the dataframes to the space ############
######################################################################

# Get all Excel files (with an .xls extension)
files <- list.files(file.path(paths, folder), pattern = "*.xls") # , recursive = TRUE)

# Removes all files that include the name word negative
negative_files <- grepl("negative", files)
files <- files[!negative_files]

rm(i, temp, folder, paths)

# length(temp[!grepl("negative", temp)]) / 5
# 150 / 5 = 30
# 30 - 3 (2 cells) = 27

#####################################################################
######## Second step: put together those that are the same ##########
#####################################################################

list <- ls()
cells <- vector()

# For each of these files: extract the portion of their name between the 14th
# and the 39th character.
for (i in seq_along(list)) {
    # look for the length only the last number
    cell <- str_sub(list[i], 14, 39)
    cells <- c(cells, cell)
}

rm(cell, i)

# keeps unique identifiers, check that the length is equal to the number
# of cells if not redo from cells<-vector()
cells <- unique(cells)

# Get all objects corresponding to the same cell ID, bind them,
# and add a cell ID column.
for (i in seq_along(cells)) {
    df <- do.call(rbind, mget(ls(pattern = cells[i])))
    df$cell <- rep(cells[i])
    name <- paste(cells[i])
    assign(name, df)
    rm(df, name)
}

rm(list = ls(pattern = ".xlsx"))

###################################################################
############## Third step: put everything together ################
###################################################################

dfs <- sapply(mget(ls()), is.data.frame)

# Get all tables and bind them
data <- do.call(rbind, mget(names(dfs)[dfs]))

# Get the sample ID and save a file with this name
name <- str_sub(cells[1], 1, 14)

write.csv(data, file = paste(name, ".csv"))

rm(list = ls())
