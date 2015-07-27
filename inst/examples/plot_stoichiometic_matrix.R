

plot_stoichio_matrix <- function(model, v, p){
  library(ggplot2)

m = model$stoichiometryMatrix(c(v, p, time=0))

df <- data.frame(m)
df$process <- rownames(df)
rownames(df) <- NULL

#df2 <- gather(df, variable, value, -process)

df2 <- reshape(df, direction = "long", v.names="value", idvar="process", ids="process", timevar = "variable"
        , times = (names(df)[-which(names(df)=="process")]), varying = list(names(df)[-which(names(df)=="process")]))
row.names(df2) <- NULL

df2 <- merge(df2, identifiers, by.x="variable", by.y="name")

df2$sign <- sign(df2$value)

#df2$sign <- plyr::mapvalues(df2$sign, from = c(-1, 0, 1), to = c("Down", NA, "UP"))
smbls <- data.frame(sign = c(-1, 0, 1), txt = c("Down", NA, "UP"))
df2 <- merge(df2, smbls)

df2 <- subset(df2, is.na(txt)==FALSE)

plt <- ggplot(data = df2, aes(x="a", y="a", colour=txt, shape=txt, fill=txt))
plt <- plt + facet_grid(process ~ variable)
plt <- plt + geom_point(size=15)
plt <- plt + scale_shape_manual(values = c(25, 24))
plt <- plt + theme_bw() + theme(panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(),
                            axis.text = element_blank(),
                            axis.title = element_blank(),
                            axis.ticks = element_blank(),
                            legend.position = "none")

return(plt)
}

plot_stoichio_matrix(model, v, p)
