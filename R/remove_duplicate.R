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
