df_t <- data.frame(t(seq(8)))
for (i in seq_along(file_names)) {
    df_t[i, ] <- extract_metada(file_names[i])
}

colnames(df_t) <- keys
df_t[, 1] <- janitor::convert_to_date(df_t[, 1] )
temp <- par()$mar
par(mar = c(10, par()$mar[-1]))
for (i in seq(8)) {
    # df_t[, i] <- as.factor(df_t[, i])
    df <- table(df_t[, i])
    print(round(sort(prop.table(df)), 3))
    plot(df, type = "h", las = 2, ylab = colnames(df_t)[i], col = "red", ylim = c(min(df), max(df)))
}
par(mar = temp)

# Brain ids
brain_ids <- unique(df_t[, 5])
df <- sapply(brain_ids, function(i) df_t[df_t$brain_id == i, ]$disease_grp[1])
write.table(df, file = "brain_id.csv", sep = "\t", row.names = TRUE)

# Tests
length(unique(df_t$brain_id)) == 54
length(unique(df_t$disease_grp)) == 6

# Brain IDs per GRP
brain_ids_grp <- lapply(unique(df_t$disease_grp), function(d) sort(unique(df_t[df_t$disease_grp == d, ]$brain_id)))
names(brain_ids_grp) <- unique(df_t$disease_grp)
lapply(brain_ids_grp, length)

library(ggplot2)

data_to_analyze <- list_tables[[1]][!list_tables[[1]]$modality == "contour", ]
var <- "area_um2"
# Group effect
ggplot(
    data_to_analyze,
    aes(
        x = disease_grp,
        y = get(var),
        color = disease_grp
    )
) +
facet_wrap(~ modality) +
scale_y_log10() +
theme_bw() +
theme(
    axis.text.x = element_text(angle = 90),
    axis.title = element_text(size = 18, face = "italic", hjust = 0.5),
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5)
) +
geom_boxplot(outlier.shape = NA) +
guides(color = "none") +
labs(title = paste("Group by", var), y = var, x = "disease_grp")

# Brain ID effect
for (m in unique(data_to_analyze$modality)) {
    p <- ggplot(
        data_to_analyze[data_to_analyze$modality == m, ],
        aes(
            x = brain_id,
            y = get(var),
            color = brain_id
        )
    ) +
    facet_wrap(~ disease_grp, scales = 'free') +
    coord_cartesian(ylim = quantile(data_to_analyze[data_to_analyze$modality == m, var], c(0.1, 0.9), na.rm = TRUE)) +
    # scale_y_log10() +
    theme_bw() +
    theme(
        axis.text.x = element_text(angle = 90),
        axis.title = element_text(size = 18, face = "italic", hjust = 0.5),
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5)
    ) +
    geom_boxplot() +
    guides(color = "none") +
    labs(title = paste(toupper(m), ": Group by", var), y = var, x = "disease_grp")
    plot(p)
}

data_to_analyze$brain_id <- factor(data_to_analyze$brain_id, levels = unlist(brain_ids_grp))
ggplot(
    data_to_analyze,
    aes(
        x = brain_id,
        y = get(var),
        color = disease_grp
    )
) +
facet_wrap(~ modality, scales = 'free') +
coord_cartesian(ylim = quantile(data_to_analyze[, var], c(0.1, 0.9), na.rm = TRUE)) +
# scale_y_log10() +
theme_bw() +
theme(
    axis.text.x = element_text(angle = 90),
    axis.title = element_text(size = 18, face = "italic", hjust = 0.5),
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5)
) +
geom_boxplot() +
# guides(color = "none") +
labs(title = paste("Group by", var), y = var, x = "disease_grp")

# CTRL plot
data_control <- list_tables[[1]][list_tables[[1]]$disease_grp == "CTRL", ]
# Create low/median/high groups
quantile_grp <- function(df, i) {
    stats <- quantile(df[, i], c(0.25, 0.5, 0.75), na.rm = TRUE)
    df$level <- NA
    df[df[, i] < stats[1] & !is.na(df[, i]), ]$level <- "low"
    df[df[, i] < stats[3] & df[, i] >= stats[1] & !is.na(df[, i]), ]$level <- "median"
    df[df[, i] >= stats[3] & !is.na(df[, i]), ]$level <- "high"
    # df$brain_id <- factor(data_to_analyze$brain_id, levels = unlist(brain_ids_grp))
    df$level <- factor(df$level, levels = c("low", "median", "high"))
    return(df)
}
library("gridExtra")
list_plot <- list()
for (t in seq_along(types)) {
    # m = "all CATHB"
    m <- types[t]
    data_control_m <- data_control[data_control$modality == m, ]
    list_plot[[t]] <- ggplot(
        data_control_m,
        aes(
            x = "",
            fill = quantile_grp(data_control_m, var)$level
        )
    ) +
    facet_wrap(~ modality) +
    theme_bw() +
    theme(
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_text(size = 18, face = "italic", hjust = 0.5),
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5)
    ) +
    geom_bar(position = "stack") +
    guides(fill = "none") +
    labs(title = toupper(m), y = var, x = "", fill = "Grouped by quantiles")
    plot(list_plot[[t]])
}

grid.arrange(list_plot[[1]], list_plot[[2]], list_plot[[3]], list_plot[[4]], ncol = 2, nrow = 2)

metada <- read_xls("~/DATA/icy/Demographics_11-2021_Amal_modified.xlsx")

# Test
data2 <- data[data$disease_grp %in% c("CTRL", "SAD"),]
model <- lmerTest::lmer(area_um2 ~ disease_grp + (disease_grp | brain_id), data = data2)
summary(model)
