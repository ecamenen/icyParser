# For each of these files: get their name, read them, format their columns,
# assign them a different metadata according to the type of data
# (among contour, all, exclusive, minus or lipofuscin)

library(stringr)
library(janitor)
library(openxlsx)
library(parallel)

pathos <- c(c("CTRL", "Crtl", "Ctrl", "ctrl"), "[sS]AD", "D[uU][pP][- ]?APP", "APP(?:mutation)?", "APP point mutation", "DS", "DS-D")
pathos <- paste(pathos, collapse = "|")
exp_reg <- paste0("[-_](", pathos, ")[-_ ](.+)")
meta_reg <- "(^\\d{8})-(IHC) (H\\d{3})(.*)(\\.lif(.*))? ?- ?(?:Series0)?(\\d{1,2})(?:\\.cells)? ?[-_]+ICY( -CELL_(\\d))?(_DATA)?_([-\\w]+).xlsx$"

extract_metada <- function(file_name) {
    metadatas <- str_replace(
        basename(file_name),
        meta_reg,
        "\\1;\\2;\\3;\\4;\\7;\\9;\\11"
    )
    (metadatas <- str_split(metadatas, ";")[[1]])

    exp <- gsub(exp_reg, "\\1;\\2", metadatas[4])
    exp <- gsub("(\\.lif ?)|(Series0)|(mutation)|[-_ .]|(point)", "", exp)
    exp <- str_split(exp, ";")[[1]]

    grp <- toupper(gsub("", "", exp[1]))
    if (grp == "CRTL")
        grp <- "CTRL"
    brain_id <- exp[2]
    if (brain_id == "BBN9608")
        brain_id <- "A5197BBN9608"
    if (metadatas[3] == "H038" && brain_id == "IB6125")
        brain_id <- "IB6125-9"

    c(metadatas[seq(3)], grp, brain_id, metadatas[5:7])
}

path <- file.path("~", "DATA", "icy")
folder <- basename(list.dirs(path, recursive = FALSE))[2]
file_names <- list.files(path = file.path(path, folder), pattern = "^\\d{8}.*.xls", recursive = TRUE)
file_names <- file_names[!grepl("(egative)|(ctrl ?-?neg)", file_names)]

files <- list()

types_to_modify <- c("CONTOUR", "All-CathepsinB", "Exclusive-CathepsinB", "MinusLipofuscin", "Lipofuscin")
types_modified <- c("contour", "all CATHB", "excl CATHB", "minus LF", "LF")

var_names <- c("contour_px", "interior_px", "perimeter_um", "area_um2")
keys <- c("date", "analysis", "exp_id", "disease_grp", "brain_id", "image_id", "cell_id", "modality")
col_names <- c(keys[seq(6)], var_names, keys[7:8])

CST <- 0.08154

format_icy <- function(file_name) {

    df <- read_xlsx(file.path(path, folder, file_name))
    df <- clean_names(df)

    metadatas <- extract_metada(file_name)
    i_type <- which(types_to_modify == metadatas[8])
    if (nrow(df) < 1) {
        df[1, ] <- rep(NA, 6)
    }

    df$modality <- rep(types_modified[i_type])

    for (j in seq_along(metadatas[-8])) {
        df[[keys[j]]] <- rep(metadatas[j])
    }

    df$date <- convert_to_date(df$date)
    df$image_id <- as.integer(df$image_id)
    df$perimeter_um <- df$contour_px * CST
    df$area_um2 <- df$interior_px * CST^2
    df$dataset <- NULL
    df$name <- NULL

    df[, c(keys[-2], var_names)]
}

files <- mclapply(
    seq_along(file_names),
    FUN = function(i) format_icy(file_names[i]),
    mc.cores = detectCores()
)

data <- Reduce(rbind, files)

path <- file.path("inst", "extdata")
file <- "icy_size"
n_size <- "3"
save(data, file = paste0(file.path(path, file), n_size, ".RData"))

# write.table(data, file = "icy_size4.csv", sep = "\t", row.names = FALSE)
# write.xlsx(data, file = file.path(path, file))
