
library(ggplot2)
library(dplyr)
library(data.table)

ny = fread('new_york_city.csv', stringsAsFactors=FALSE)
wash = fread('washington.csv', stringsAsFactors=FALSE)
chi = fread('chicago.csv', stringsAsFactors=FALSE)

head(ny)
str(ny)
str(wash)
str(chi)

ny$City = "New York"
wash$City = "Washington"
chi$City = "Chicago"

df = full_join(full_join(ny, chi), wash)
table(df$City, exclude = NULL)


class(df$Trip.Duration)
summary(df$Trip.Duration)

df$Trip.Duration.min = df$Trip.Duration/60
cat("\n Transformed into minutes")
summary(df$Trip.Duration.min)
by(df$Trip.Duration.min, df$City, summary)

ggplot(df, aes(x = City, y = Trip.Duration.min)) + geom_boxplot() + theme_minimal() # simple plot to see distribution

df[which(df$Trip.Duration.min > 1440),"Trip.Duration.min"] = NA
summary(df$Trip.Duration.min)

ggplot(df, aes(x=Trip.Duration.min)) + geom_histogram(bins=1500) + coord_cartesian(xlim = c(0, 250)) + theme_minimal()
by(df$"Trip.Duration.min", df$City, summary) # simple plot to see distribution

table(df$User.Type, exclude = NULL)
names(table(df$User.Type, exclude = NULL))

df[which(df$User.Type == ""),"User.Type"] = NA
table(df$User.Type, exclude = NULL)
cat("\n User.Type by City")
by(df$User.Type, df$City, table)

table(df$Gender, exclude = NULL)
names(table(df$Gender, exclude = NULL))

# We are goint to change those "" into NA, so they are not represented.
df[which(df$Gender == ""),"Gender"] = NA
table(df$Gender, exclude = NULL)

cat("\n Gender by City")
by(df$Gender, df$City, table, exclude = NULL)

summary(df$Birth.Year)
df[which(df$Birth.Year < 1903),"Birth.Year"] = NA

class(df$Start.Time)
head(df$Start.Time)

df$Start.Time.2 = strptime(df$Start.Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
head(df$Start.Time)

df$Start.Month = strftime(df$Start.Time.2, "%b", tz = "UTC")
head(df$Start.Month)
df$Start.Month = as.factor(df$Start.Month)
levels(df$Start.Month)
df$Start.Month = factor(df$Start.Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun")) #
# we change it to factor and reordered to plot it later on

df$Start.Hour = strftime(df$Start.Time.2, "%H:%M:%S", tz = "UTC")
df$Start.Hour.2 = as.POSIXct(round(as.POSIXct(df$Start.Hour, format = "%H:%M:%S", tz = "UTC") , units="hours"))

head(df$Start.Hour)
head(df$Start.Hour.2)
class(df$Start.Hour.2) # format POSIXct to plot with ggplot2 time

breaks = sort(c(unique(round(df$Birth.Year, digits = -1)), max(round(df$Birth.Year+5, digits = -1), na.rm = TRUE)))
# we create this vector so we can make breaks every 5 years.
fd = subset(df, !(df$City == "Washington")) # subseted as it does not have Gender information

ggplot(subset(fd, !is.na(fd$User.Type)), aes(x=Birth.Year, fill=User.Type)) +
geom_histogram(binwidth = 1)+
    scale_x_continuous(breaks=seq(min(breaks), max(breaks), by = 5), limits = c(min(breaks), max(breaks)))+
    labs(title = "Birth Year Distribution by User Type and City", y = "User count", x = "Birth Year") +
    theme(panel.background = element_blank(),
          panel.grid.major.y = element_line(size = 0.5, linetype = "dashed", colour = "darkgrey"),
          axis.line.x = element_line(size = 0.5),
          axis.line.y = element_line(size = 0.5),
          axis.text.x = element_text(size=8,angle=45, hjust = 1),
          plot.title = element_text(size=16),
          legend.position = "none") +
    scale_fill_manual(values = c("Customer" = "#f1b9a5", "Subscriber" = "#83c5be")) +

    facet_wrap(~User.Type + City, scales = "free")

by(df$User.Type, df$City, table)
summary(df$Birth.Year)


fd = subset(df, !(df$City == "Washington")) # subseted as it does not have Gender information
fd = subset(fd, !is.na(fd$User.Type) & !is.na(fd$Gender)) # removing missings

ggplot(fd, aes(x = Gender, fill = Gender)) +
geom_bar() +
    scale_fill_manual(values = c("Female" = "#5D3A9B", "Male" = "#E66100")) +
    labs(title = "Gender Distribution by User Type and City", y = "User count", x = element_blank()) +
    theme(legend.position = "none",
          panel.background = element_blank(),
          panel.grid.major.y = element_line(size = 0.5, linetype = "dashed", colour = "darkgrey"),
          strip.background = element_rect(fill = "#ededed", colour = "#ededed", size = 1),
          strip.text.x = element_text(size=12),
          axis.text.x = element_text(size=9),
          axis.text.y = element_text(size=9),
          plot.title = element_text(size=16)) +

    facet_wrap(~User.Type + City)

by(fd$Gender, list(fd$User.Type, fd$City), table)

fd = subset(df, !(df$City == "Washington") & !is.na(df$User.Type) & !is.na(df$Gender))
# removing missings

ggplot(subset(fd, !is.na(fd$User.Type)), aes(x=Birth.Year, fill=User.Type)) +
geom_histogram(binwidth = 1)+
  scale_x_continuous(breaks=seq(min(breaks), max(breaks), by = 10), limits = c(min(breaks), max(breaks)))+
  labs(title = "Birth Year Distribution by User Type, City and Sex", y = "User count", x = "Birth Year") +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, linetype = "dashed", colour = "darkgrey"),
        axis.line.x = element_line(size = 0.5),
        axis.line.y = element_line(size = 0.5),
        axis.text.x = element_text(size=8, angle=45, hjust = 1),
        plot.title = element_text(size=16),
       legend.position = "none") +
  scale_fill_manual(values = c("Customer" = "#f1b9a5", "Subscriber" = "#83c5be")) +
  facet_wrap(~User.Type + City + Gender, ncol = 2, scales = "free")

fd = df
fd$User.Type = "Both" # to compare everything
fd = rbind(df, fd)

ggplot(subset(fd, !is.na(User.Type)), aes(x = City, y = Trip.Duration.min, fill = City)) +
geom_boxplot(outlier.colour = "#a9a9a9") +
  labs(title = "Trip Duration Distribution by User Type and City", y = "Trip Duration (min.)", x = element_blank()) +
  scale_fill_manual(values = c("Chicago" = "#70D357", "New York" = "#DFC721", "Washington" = "#3EB9CE")) +
  coord_cartesian(ylim = c(0, 120)) + # to omit outliers
  facet_wrap(~User.Type) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, linetype = "dashed", colour = "darkgrey"),
        strip.background = element_rect(fill = "#ededed", colour = "#ededed", size = 1),
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=9, vjust = 7),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size=16))

by(df$"Trip.Duration.min">120, df$City, summary)

by(df$"Trip.Duration.min", list(df$City, df$User.Type), summary)

fd = subset(df, !is.na(df$User.Type)) # removing missings

ggplot(fd, aes(x=Start.Hour.2, fill = User.Type)) +
geom_histogram(stat = "count") +
  scale_x_datetime(date_labels = "%Hh",breaks = "2 hours") +
  labs(title = "Start Time Distribution by User Type and City",
       x = "Hours", y = "Start Time") +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(size = 0.1, linetype = "dashed", colour = "darkgrey"),
        panel.grid.minor.x = element_line(size = 0.1, linetype = "dashed", colour = "darkgrey"),
        axis.line.x = element_line(size = 0.5),
        axis.line.y = element_line(size = 0.5),
        axis.text.x = element_text(size=8, angle=45, hjust = 1),
        plot.title = element_text(size=16),
       legend.position = "none") +
  scale_fill_manual(values = c("Customer" = "#f1b9a5", "Subscriber" = "#83c5be")) +

facet_wrap(~User.Type + City, scales = "free", ncol = 3)

fd$Start.Hour.3 = format(strptime(fd$Start.Hour.2,"%Y-%m-%d %H:%M:%S"),'%H')
by(fd$Start.Hour.3, fd$User.Type, table)
by(fd$Start.Hour.3, list(fd$User.Type,fd$City), table)

system('python -m nbconvert Explore_bikeshare_data.ipynb')
