data[row.names(errors_temp[[14]]), ]

data[data$disease_grp == "APP" &
                    data$brain_id == "A18898" &
                     data$image_id == "6" & data$modality == "contour", ]

data[row.names(errors_temp[[2]]), ]


data[data$disease_grp == "APP" &
                data$brain_id == "A5197BBN9608" &
                 data$image_id == "1" & data$modality == "contour", ]

data[row.names(errors_temp[[8]]), ]
data[data$disease_grp == "CTRL" &
                data$brain_id == "IB6125-9" &
                 data$image_id == "13" & data$modality == "contour", ]

d <- "CTRL"
b <- "A1101620"
r <- "4"
m <- "all CATHB"

d <- "APP"
b <- "A18898"
r <- "6"
m <- "all CATHB"
condition <- data$disease_grp == d &
    data$brain_id == b &
    data$image_id == r

df <- data[condition & data$modality == m, ]
# df_contour <- data[condition & data$modality == "contour", ]
# max_cell <- max(as.numeric(as.character(df_contour$cell_id)))

cols_poncta <- paste(
    "nb_poncta",
    paste0(var_names, "_weighted"),
    sep = "_"
)

cell <- 1
#nb_ponctas <- c()
dates <- sort(unique(df$date))
for (da in seq_along(dates)) {
    if (da > 1) {
        temp <- df[df$date == dates[da], ]
        for (c in unique(temp$cell_id)) {
            condition_dupl <- condition & data$date == dates[da] & data$cell_id == c
            cell <- cell + 1
            df_contour <- data[condition_dupl & data$modality == "contour", ]
            roi_rows <- which.max(df_contour$contour_px)
            roi <- df_contour[df_contour$cell_id == c, var_names][roi_rows, ]
            for (m in types_modified) {
                row_match <- condition_dupl & data$modality == m
                data[row_match, "cell_id"] <- paste0(cell)
                if (m != "contour") {
                    nb_poncta <- nrow(data[row_match, ])
                    #nb_ponctas <- c(nb_ponctas, nb_poncta)
                    data[row_match, c("nb_poncta", cols_poncta)] <- c(nb_poncta, nb_poncta/roi)
                    for (v in c(var_names, "nb_poncta")) {
                        data[
                            row_match, paste0(v, "_weighted")
                        ] <- data[row_match, v] / roi[v]
                    }
                }
            }
        }
    }
}
