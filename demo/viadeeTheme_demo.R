source("R/viadeeTheme.R")

viadeePlot_fire(ggplot(mpg, aes(displ, hwy, colour = class))) +
  geom_point()

viadeePlot_fire(ggplot(mpg, aes(hwy, group=class, fill=class))) +
  geom_histogram()

viadeePlot(ggplot(mpg, aes(hwy, group=class, fill=class))) +
  geom_histogram()

viadeePlot_purple(ggplot(mpg, aes(hwy, group=class, fill=class))) +
  geom_histogram()
