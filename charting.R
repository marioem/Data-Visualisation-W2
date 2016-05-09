d1t <- gather(d1, key = month, value = tempdif, -Year, -(J.D:SON))
d1t$tempdif <- d1t$tempdif/100

pal <- colorRampPalette(c("blue", "red"))
ggplot(d1t, aes(x=Year, y=tempdif, color = tempdif)) + geom_line() + scale_colour_gradientn(colours=pal(20)) + facet_grid(month ~ .)

ggplot(d1t, aes(x=Year, y=tempdif, color = tempdif)) + geom_line() + scale_colour_gradientn(colours=pal(20))

d1b <- d1t %>% group_by(year = Year) %>% summarize(avg = mean(tempdif))

ggplot(d1b, aes(x=year, y = avg, fill = avg)) + geom_bar(stat = "identity") + scale_fill_gradientn(colours=pal(20))

ggplot(d1b, aes(x=year, y = avg, color = avg)) + geom_line() + scale_color_gradientn(colours=pal(20))

d1tmm <- d1t %>% group_by(Year) %>% summarize(max = max(tempdif), min = min(tempdif))

ggplot(d1tmm, aes(Year)) + geom_ribbon(aes(ymin = min, ymax = max, fill = "grey"))

    d1tg <- gather(d1tmm, key = key, value = value, -Year)

ggplot(d1tg, aes(x=Year, y=value, group = key, color = key)) + geom_line() + scale_color_manual(values = c("red", "blue"))
ggplot(d1tg, aes(x=Year, y=value, group = key, color = value)) + geom_line() + scale_colour_gradientn(colours=pal(20))


g <- ggplot(d1tg, aes(x=Year, y=value, group = key, color = value)) + geom_line(alpha = .7)
g <- g + geom_smooth(method = "loess", se = F, color = "gray")
g <- g + scale_colour_gradientn(name = "Temperature\ndeviation", colours=pal(20))
g <- g + ggtitle("Yearly minimum and maximum temperature\ndeviations from the 1951-1980 average.")
g <- g + labs(y = expression(paste("Temperature deviation [",degree~C, "]")))
g <- g + theme_bw() + theme(legend.position = "none")
g

#################################################################################

d2g <- gather(d2, key = key, value = value, -Year)

d2g$stripe[d2g$key == "X64N.90N"] <- "1"
d2g$stripe[d2g$key == "X44N.64N"] <- "2"
d2g$stripe[d2g$key == "X24N.44N"] <- "3"
d2g$stripe[d2g$key == "EQU.24N"] <- "4"
d2g$stripe[d2g$key == "X24S.EQU"] <- "5"
d2g$stripe[d2g$key == "X44S.24S"] <- "6"
d2g$stripe[d2g$key == "X64S.44S"] <- "7"
d2g$stripe[d2g$key == "X90S.64S"] <- "8"

labels <- c("1" = "90N-64N", "2" = "64N-44N", "3" = "44N-24N", "4" = "24N-EQU", "5" = "EQU-24S", "6" = "24S-44S", "7" = "44S-64S", "8" = "64S-90S")

ggplot(d2g, aes(x = Year, y = value, color = key)) + geom_line()

ggplot(d2g, aes(x = Year, y = value, color = key)) + geom_smooth(method = "loess", se = F)

d2gst <- d2g[!is.na(d2g$stripe),]
d2gst$value <- d2gst$value/100

pal2 <- colorRampPalette(c("#d53e4f", "yellow", "#3288bd"), space = "rgb")

g <- ggplot(d2gst, aes(x = Year, y = value, color = factor(stripe)))
g <- g + geom_smooth(method = "loess", se = F) + geom_line() + scale_color_hue(h.start = 225, direction = 1)
g <- g + facet_grid(stripe ~ ., labeller = labeller(stripe = labels))
g <- g + ggtitle("Yearly temperature deviations from the 1951-1980 average\nover latitude zones with superimposed trend.")
g <- g + labs(y = expression(paste("Temperature deviation [", degree~C, "]")))
g <- g + theme_bw() + theme(legend.position = "none", strip.text = element_text(size = 8), panel.background = element_rect(fill = "transparent", colour = NA),
                            plot.background = element_rect(fill = "transparent", colour = NA))
g
ggsave("Assignment.png", plot = g, device = "png", bg = "transparent", width = 5.555, height = 3.91, units = "in")
png('Assignment.png',width=800,height=563,units="px",bg = "transparent")
print(g)
dev.off()

d2gs <- d2g[d2g$key == "Glob" | d2g$key == "NHem" | d2g$key == "SHem",]

ggplot(d2gs, aes(x = Year, y = value, color = key)) + geom_point() + geom_line()

ggplot(d2gs, aes(x = Year, y = value, color = key)) + geom_smooth(method = "loess", se = F)

