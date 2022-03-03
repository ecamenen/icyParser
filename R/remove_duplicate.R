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
df_contour <- data[condition & data$modality == "contour", ]
# max_cell <- max(as.numeric(as.character(df_contour$cell_id)))

cell <- 1
dates <- sort(unique(df_contour$date))
for (da in seq_along(dates)) {
    if (da > 1) {
        temp <- df_contour[df_contour$date == dates[da], ]
        for (c in unique(temp$cell_id)) {
            condition_dupl <- condition & data$date ==  dates[da] & data$cell_id == c
            cell <- cell + 1
            roi_rows <- which.max(data[condition_dupl & data$modality == "contour", ]$contour_px)
            df_contour[df_contour$cell_id == c, var_names]
            for (m in types_modified)
                data[condition_dupl & data$modality == m, ]$cell_id <- paste0(cell)
        }
    }
}
