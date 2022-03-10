cols <- paste0(
    c("mean_", "median_", "sd_"),
    rep(
        c(var_names, paste0(var_names, "_weighted")),
        each = 3)
)

# load(paste0(file.path(path_out, file), n_size, ".RData"))
# data <- read.xlsx(file.path(path_out, file))
# data[, 1] <- convert_to_date(data[, 1])

data <- data.frame(data)
for (i in 2:7)
    data[, i] <- as.factor(data[, i])

data$cell_id <- factor(data$cell_id, levels = seq(7))

list_tables <- list(data)

count_poncta <- function(i)
    nrow(df[df$cell_id == i, ])

calculate_stats <- function(df) {
    if (m == "LF")
        f1 <- f2 <- sum
    else {
        f1 <- mean
        f2 <- median
    }
    unlist(
        lapply(
            c(var_names, paste0(var_names, "_weighted")),
            function(k)
                sapply(
                    c(f1, f2, sd),
                    function(j) do.call(j, list(df[, k], na.rm = TRUE)))
        )
    )
}

types <- types_modified[-1]
diseases <- levels(data$disease_grp)

cols_poncta <- paste(
    "nb_poncta",
    paste0(var_names, "_weighted"),
    sep = "_"
)

nb_tot_cells <- 6

# date exp_id disease_grp brain_id image_id cell_id   modality
# 2021-06-10   H037          DS   A07716       17       1  all CATHB, excl CATHB, minus LF
# options(warn = 1)
errors_temp <- list()
(init <- Sys.time())
tables <- list()
i <- 0
for (m in types) {
    for (d in diseases) {
        bs <- unique(data[data$disease_grp == d, ]$brain_id)
        for (b in bs) {
            rs <- sort(unique(data[data$brain_id == b, ]$image_id))
            for (r in rs) {
                i <- i + 1
                condition <- data$disease_grp == d &
                    data$brain_id == b &
                    data$image_id == r

                df <- data[condition & data$modality == m, ]

                cell <- 0
                nb_ponctas <- rep(NA, nb_tot_cells)
                nb_ponctas_weighted <- lapply(
                    seq(nb_tot_cells),
                    function(j) sapply(var_names, function(k) NA)
                )
                dates <- sort(unique(df$date))
                for (da in seq_along(dates)) {
                        temp <- df[df$date == dates[da], ]
                        for (c in unique(temp$cell_id)) {
                            condition_dupl <- condition & data$date == dates[da] & data$cell_id == c
                            cell <- cell + 1
                            df_contour <- data[condition_dupl & data$modality == "contour", ]
                            roi_rows <- which.max(df_contour$contour_px)
                            roi <- df_contour[df_contour$cell_id == c, var_names][roi_rows, ]
                            if (nrow(roi) > 0) {
                                for (m2 in types_modified) {
                                    row_match <- condition_dupl & data$modality == m2
                                    list_tables[[1]][row_match, "cell_id"] <- as.factor(paste0(cell))
                                    if (m2 != "contour") {
                                        nb_ponctas[cell] <- nrow(data[row_match, ])
                                        nb_ponctas_weighted[[cell]] <- nb_ponctas[cell]/roi
                                        list_tables[[1]][row_match, "nb_poncta"] <- nb_ponctas[cell]
                                        for (z in seq(cols_poncta))
                                            list_tables[[1]][row_match, cols_poncta[z]] <- nb_ponctas_weighted[[cell]][z]
                                        for (v in var_names) {
                                            list_tables[[1]][
                                                row_match, paste0(v, "_weighted")
                                            ] <- list_tables[[1]][row_match, v] / as.numeric(roi[v])
                                        }
                                    }
                                }
                            } else {
                                errors_temp <- c(errors_temp, list(temp[1, ]))
                            }
                        }
                    }

                nb_poncta_weighted <- unlist(nb_ponctas_weighted)
                sum_poncta_weighted <- sapply(
                    var_names,
                    function(v2) sum(nb_poncta_weighted[names(nb_poncta_weighted) == v2], na.rm = TRUE)
                )
                stats <- calculate_stats(list_tables[[1]][condition & data$modality == m, ])
                tables[[i]] <- c(d, b, r, m, t(nb_ponctas), sum(nb_ponctas, na.rm = TRUE), as.matrix(nb_poncta_weighted), sum_poncta_weighted, sum(nb_ponctas > 0, na.rm = TRUE), stats)
            }
        }
    }
}

Sys.time() - init

cols_p_weight <- paste(
    rep(paste0("nb_poncta_", seq(nb_tot_cells)),  each = 4),
    rep(paste0(var_names, "_weighted"), 4),
    sep = "_"
)

cols_s_p_weight <- paste0("sum_poncta_weighted_", var_names)
cols_cell <- c(paste0("nb_poncta_", seq(nb_tot_cells)), "sum_poncta", cols_p_weight, cols_s_p_weight, "nb_cells")

list_tables[[2]] <- data.frame(Reduce(rbind, tables))
colnames(list_tables[[2]]) <- c(keys[4:6], keys[8], cols_cell, cols)

tables <- list()
i <- 0
for (m in types) {
    for (d in diseases) {
        bs <- unique(data[data$disease_grp == d, ]$brain_id)
        for (b in bs) {
            rs <- sort(unique(data[data$brain_id == b, ]$image_id))
            i <- i + 1
            df <- list_tables[[1]][
                list_tables[[1]]$disease_grp == d &
                list_tables[[1]]$brain_id == b &
                list_tables[[1]]$modality == m, ]
            sum_cells <- sapply(
                cols_cell,
                function(j) {
                df_poncta <- list_tables[[2]][
                    list_tables[[2]]$disease_grp == d &
                        list_tables[[2]]$brain_id == b &
                        list_tables[[2]]$modality == m, j]
                    sum(as.numeric(levels(df_poncta)[df_poncta]), na.rm = TRUE)
                }
            )
            tables[[i]] <- c(d, b, m, sum_cells, length(rs), calculate_stats(df))
        }
    }
}

list_tables[[3]] <- data.frame(Reduce(rbind, tables))
colnames(list_tables[[3]]) <- c(keys[4:5], keys[8], cols_cell, "nb_images", cols)

tables <- list()
i <- 0
for (m in types) {
    for (d in diseases) {
        bs <- unique(data[data$disease_grp == d, ]$brain_id)
            i <- i + 1
            df <- list_tables[[1]][
                list_tables[[1]]$disease_grp == d &
                list_tables[[1]]$modality == m, ]
            sum_cells <- sapply(
                c(cols_cell, "nb_images"),
                function(j) {
                df_poncta <- list_tables[[3]][
                    list_tables[[3]]$disease_grp == d &
                        list_tables[[3]]$modality == m, j]
                    sum(as.numeric(levels(df_poncta)[df_poncta]), na.rm = TRUE)
                }
            )
            tables[[i]] <- c(d, m, sum_cells, length(bs), calculate_stats(df))
    }
}

list_tables[[4]] <- data.frame(Reduce(rbind, tables))
colnames(list_tables[[4]]) <- c(keys[4], keys[8], cols_cell, "nb_images", "nb_brains", cols)

library(openxlsx)
sheets <- c("Raw", "Images", "Brains", "Groups")
wb <- createWorkbook()
for (i in seq_along(sheets)) {
    addWorksheet(wb, sheets[i])
    writeData(wb, sheets[i], list_tables[[i]])
}

saveWorkbook(wb, file = paste0(file.path(path_out, file), n_size, ".xlsx"), overwrite = TRUE)
