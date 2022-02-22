var_names <- c("contour_px", "interior_px", "perimeter_um", "area_um2")
cols <- paste(c("mean", "median", "sd"), rep(var_names, each = 3), sep = "_")

load(paste0(file.path(path, file), "4.RData"))
# data <- read.xlsx(file.path(path, file))
# data[, 1] <- convert_to_date(data[, 1])

data <- data.frame(data)
for (i in 2:8)
    data[, i] <- as.factor(data[, i])

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
            var_names,
            function(k)
                sapply(
                    c(f1, f2, sd),
                    function(j) do.call(j, list(df[, k], na.rm = TRUE)))
        )
    )
}

types <- types_modified[-1]
diseases <- levels(data$disease_grp)

tables <- list()
i <- 0
for (m in types) {
    for (d in diseases) {
        bs <- unique(data[data$disease_grp == d, ]$brain_id)
        for (b in bs) {
            rs <- sort(unique(data[data$brain_id == b, ]$roi_id))
            for (r in rs) {
                # nb_cells <- max(as.integer(df$cell_id))
                i <- i + 1
                df <- data[
                    data$disease_grp == d &
                    data$brain_id == b &
                    data$roi_id == r &
                    data$modality == m, ]
                nb_cells <- sapply(seq(3), count_poncta)
                tables[[i]] <- c(d, b, r, m, t(nb_cells), sum(nb_cells), calculate_stats(df))
            }
        }
    }
}

list_tables[[2]] <- data.frame(Reduce(rbind, tables))
colnames(list_tables[[2]]) <- c(keys[4:6], keys[8], paste0("nb_poncta_", seq(3)), "nb_poncta_tot", cols)

tables <- list()
i <- 0
for (m in types) {
    for (d in diseases) {
        bs <- unique(data[data$disease_grp == d, ]$brain_id)
        for (b in bs) {
            rs <- sort(unique(data[data$brain_id == b, ]$roi_id))
            i <- i + 1
            df <- data[
                data$disease_grp == d &
                data$brain_id == b &
                data$modality == m, ]
            tables[[i]] <- c(d, b, m, length(rs), calculate_stats(df))
        }
    }
}

list_tables[[3]] <- data.frame(Reduce(rbind, tables))
colnames(list_tables[[3]]) <- c(keys[4:5], keys[8], "nb_cells", cols)

tables <- list()
i <- 0
for (m in types) {
    for (d in diseases) {
        bs <- unique(data[data$disease_grp == d, ]$brain_id)
            i <- i + 1
            df <- data[
                data$disease_grp == d &
                    data$modality == m, ]
            tables[[i]] <- c(d, m, length(bs), calculate_stats(df))
    }
}

list_tables[[4]] <- data.frame(Reduce(rbind, tables))
colnames(list_tables[[4]]) <- c(keys[4], keys[8], "nb_patients", cols)

sheets <- c("Raw", "Cells", "Patients", "Groups")
wb <- createWorkbook()
for (i in seq_along(sheets)) {
    addWorksheet(wb, sheets[i])
    writeData(wb, sheets[i], list_tables[[i]])
}

saveWorkbook(wb, file = paste0(file.path(path, file), "4.xlsx"), overwrite = TRUE)
