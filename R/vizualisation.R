df_t <- data.frame(t(seq(8)))
for (i in seq_along(file_names)) {
    df_t[i, ] <- extract_metada(file_names[i])
}

colnames(df_t) <- keys
df_t[, 1] <- convert_to_date(df_t[, 1] )
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
brain_ids_grp <- lapply(unique(df_t$disease_grp), function(d) unique(df_t[df_t$disease_grp == d, ]$brain_id))
names(brain_ids_grp) <- unique(df_t$disease_grp)
lapply(brain_ids_grp, length)
