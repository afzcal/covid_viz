# install.packages(c("xts", 'zoo'))
# install.packages(c('ggplot2', 'shadowtext', 'ggrepel'))
library(dplyr)
library(ggplot2)
library(reshape2)
library(shadowtext)
library(ggrepel)
library(xts)
library(zoo)
# -- County
# library(dplyr)
# library(ggplot2)
# library(reshape2)
# library(shadowtext)
library(tidyverse)
library(urbnmapr)
library(RColorBrewer)

log10_minor_break = function (...){
  function(x) {
    minx         = floor(min(log10(x), na.rm=T))-1;
    maxx         = ceiling(max(log10(x), na.rm=T))+1;
    n_major      = maxx-minx+1;
    major_breaks = seq(minx, maxx, by=1)
    minor_breaks =
      rep(log10(seq(1, 9, by=1)), times = n_major)+
      rep(major_breaks, each = 9)
    return(10^(minor_breaks))
  }
}


SAVE_PLOTS <- TRUE
PNG_RES <- 600
d_major_break <- "10 day"
d_minor_break <- "2 day"
x_ax_sz <- 6
exp_lab_shift <- 10
x_major_spacing <- 10
x_minor_spacing <- 2

curr_date <- Sys.Date() - 1
curr_date_str <- gsub('-','_', curr_date)
curr_time_base <- Sys.time() #- 1
curr_time <- Sys.time() #- 1
curr_time_str <- gsub(' ','-', gsub('-|:','_', curr_time))
fname_nyt <- paste0(paste('data/corona_nyt', curr_time_str, sep='-'), '.csv')
fname_nyt

# https://github.com/nytimes/covid-19-data
if(!file.exists(fname_nyt)) {
  nyt_us_states_url <- 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv'
  download.file(url = nyt_us_states_url, destfile = fname_nyt)
}
nyt_df <- read.csv(fname_nyt)

# CA ----------------------------------------------------------------------
ca_df <- nyt_df %>%
  filter(state == 'California')
ca_df$date <- as.Date(ca_df$date)
ca_df

nr_ca <- nrow(ca_df)
date_shift <- 19
exp_steps_ca <- 56
exp_ca <- 1.21
ca_df$exp <- c(rep(NA, date_shift), 
               exp_ca^(1:(exp_steps_ca)),
               rep(NA, nr_ca-exp_steps_ca-date_shift))

ca_df2 <- melt(data = ca_df, id.vars = "date", 
               measure.vars = c("cases", "deaths", "exp"),
               value.name = 'occurences')


x_max_ca <- max(ca_df2$date) - exp_lab_shift
y_min_ca <- 1
exp_label_ca <- paste0(c("Exp base: ", as.character(exp_ca), ""), collapse = '')

ca_melt_df <- ca_df2 %>% 
  filter(variable %in% c("cases", "deaths"))
# corona_df2
time_str <- gsub(':', '_', gsub(' ', '+', Sys.time()))
ca_cases_fname <- paste0(c(time_str,'_corona_CA_wDeaths',  '.png'), collapse = '')
# ca_cases_fname
if(SAVE_PLOTS) {
  png(paste0('images/', ca_cases_fname),
      width=7, height=5, units='in', res=PNG_RES)
}
ggplot(ca_melt_df, aes(x=date, y=occurences, color=variable)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_breaks = d_major_break, date_labels = "%m-%d",
               date_minor_breaks = d_minor_break) +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=x_ax_sz))  +
  theme(legend.position="right") +
  ggtitle(paste("CA: Reported Coronavirus Cases and Deaths", curr_date, sep= ' - '))
if(SAVE_PLOTS) dev.off()

y_breaks_ca <- as.vector(c(1) %o% 10^(0:6)) 
ca_cases_log_fname <- paste0(c(time_str,'_corona_CA_log_wDeaths',  '.png'), collapse = '')

if(SAVE_PLOTS) {
  png(paste0('images/', ca_cases_log_fname),
      width=7, height=5, units='in', res=PNG_RES)
}
ggplot(ca_df2, aes(x=date, y=occurences, color=variable)) +
  geom_line() +
  geom_point(size=1) +
  scale_x_date(date_breaks = d_major_break, date_labels = "%m-%d",
               date_minor_breaks = d_minor_break) +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=x_ax_sz))  +
  theme(legend.position="right") +
  scale_y_log10(breaks=y_breaks_ca, minor_breaks=log10_minor_break()) +
  annotate(geom="label", x=x_max_ca, y=y_min_ca, label=exp_label_ca, color="#619CFF", size=2.75) +
  ggtitle(paste("CA: Reported Coronavirus Cases and Deaths [Log Scale]", curr_date, sep= ' - '))
if(SAVE_PLOTS) dev.off()

# *New* Cases (CA) ------------------------------------------------------------
ca_ts <- xts(order.by=ca_df$date, ca_df[,c("cases", "deaths")])
ca_new_ts <- diff(ca_ts)
ca_new_df <- fortify(ca_new_ts)
names(ca_new_df)[1] <- "date"
ca_melt_new_df <- melt(data = ca_new_df, id.vars = "date",
                       measure.vars = c("cases", "deaths"),
                       value.name = 'occurences')

ca_new_log_fname <- paste0(c(time_str,'_corona_CA_new_log',  '.png'), collapse = '')
if(SAVE_PLOTS) {
  png(paste0('images/', ca_new_log_fname),
      width=7, height=5, units='in', res=PNG_RES)
}
ggplot(ca_melt_new_df, aes(x=date,y=occurences,colour=variable)) + 
  geom_line() +
  geom_point(size=1) + 
  theme(legend.position="right") +
  scale_x_date(date_breaks = d_major_break, date_labels = "%m-%d",
               date_minor_breaks = d_minor_break) +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=x_ax_sz))  +
  theme(legend.position="right") +
  # scale_y_continuous(trans='log10') +
  scale_y_log10(minor_breaks=log10_minor_break()) +
  ggtitle(paste("CA: *New* Coronavirus Cases/Deaths [Log Scale]", curr_date, sep= ' - '))
if(SAVE_PLOTS) dev.off()

# NY ----------------------------------------------------------------------
ny_df <- nyt_df %>%
  filter(state == 'New York')
ny_df$date <- as.Date(ny_df$date)
ny_df

nr_ny <- nrow(ny_df)
i_start <- 7
exp_steps_ny <- 31
exp_ny <- 1.41
ny_df$exp <- c(exp_ny^(i_start:(i_start+exp_steps_ny)),
               rep(NA, nr_ny-exp_steps_ny-1))

ny_df2 <- melt(data = ny_df, id.vars = "date",
               measure.vars = c("cases", "deaths", "exp"),
               value.name = 'occurences')
x_max_ny <- max(ny_df2$date) - exp_lab_shift
y_min_ny <- 1 #
exp_label_ny <- paste0(c("Exp base: ", as.character(exp_ny), ""), collapse = '')

ny_melt_df <- ny_df2 %>% 
  filter(variable %in% c("cases", "deaths"))

time_str <- gsub(':', '_', gsub(' ', '+', Sys.time()))
ny_cases_fname <- paste0(c(time_str,'_corona_NY_wDeaths',  '.png'), collapse = '')
# ny_cases_fname

if(SAVE_PLOTS) {
  png(paste0('images/', ny_cases_fname),
      width=7, height=5, units='in', res=PNG_RES)
}
ggplot(ny_melt_df, aes(x=date, y=occurences, color=variable)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_breaks = d_major_break, date_labels = "%m-%d",
               date_minor_breaks = d_minor_break) +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=x_ax_sz))  +
  theme(legend.position="right") +
  ggtitle(paste("NY: Reported Coronavirus Cases and Deaths", curr_date, sep= ' - '))
if(SAVE_PLOTS) dev.off()

y_breaks_ny <- as.vector(c(1) %o% 10^(0:6))

ny_cases_log_fname <- paste0(c(time_str,'_corona_NY_log_wDeaths',  '.png'), collapse = '')
if(SAVE_PLOTS) {
  png(paste0('images/', ny_cases_log_fname),
      width=7, height=5, units='in', res=PNG_RES)
}
ggplot(ny_df2, aes(x=date, y=occurences, color=variable)) +
  geom_line() +
  geom_point(size=1) +
  scale_x_date(date_breaks = d_major_break, date_labels = "%m-%d",
               date_minor_breaks = d_minor_break) +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=x_ax_sz))  +
  theme(legend.position="right") +
  scale_y_log10(breaks=y_breaks_ny, minor_breaks=log10_minor_break()) +
  annotate(geom="label", x=x_max_ny, y=y_min_ny, label=exp_label_ny, color="#619CFF", size=2.75) +
  ggtitle(paste("NY: Reported Coronavirus Cases and Deaths [Log Scale]", curr_date, sep= ' - ')) 
if(SAVE_PLOTS) dev.off()

# *New* Cases (NY) ------------------------------------------------------------
ny_ts <- xts(order.by=ny_df$date, ny_df[,c("cases", "deaths")])
ny_new_ts <- diff(ny_ts)
ny_new_df <- fortify(ny_new_ts)
names(ny_new_df)[1] <- "date"
ny_melt_new_df <- melt(data = ny_new_df, id.vars = "date",
                       measure.vars = c("cases", "deaths"),
                       value.name = 'occurences')
# ny_melt_new_df

# ca_melt_new_df
ny_new_log_fname <- paste0(c(time_str,'_corona_NY_new_log',  '.png'), collapse = '')
if(SAVE_PLOTS) {
  png(paste0('images/', ny_new_log_fname),
      width=7, height=5, units='in', res=PNG_RES)
}
ggplot(ny_melt_new_df, aes(x=date,y=occurences,colour=variable)) + 
  geom_line() +
  geom_point(size=1) + 
  theme(legend.position="right") +
  scale_x_date(date_breaks = d_major_break, date_labels = "%m-%d",
               date_minor_breaks = d_minor_break) +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=x_ax_sz))  +
  theme(legend.position="right") +
  # scale_y_continuous(trans='log10') +
  scale_y_log10(minor_breaks=log10_minor_break()) +
  ggtitle(paste("NY: *New* Coronavirus Cases/Deaths [Log Scale]", curr_date, sep= ' - '))
if(SAVE_PLOTS) dev.off()

# All US ------------------------------------------------------------------
us_df <- nyt_df %>%
  group_by(date) %>%
  summarise(cases= sum(cases), deaths=sum(deaths)) 
us_df$date <- as.Date(us_df$date)
us_df

nr_us <- nrow(us_df)

exp_us <- 1.32
date_shift_us <- 25
exp_steps_us <- 49
us_df$exp <- c(rep(NA, date_shift_us), 
               exp_us^(1:(exp_steps_us)),
               rep(NA, nr_us-exp_steps_us-date_shift_us)) #[1:(nr_ca)] #- 10000


us_df2 <- melt(data = us_df, id.vars = "date",
               measure.vars = c("cases", "deaths", "exp"),
               value.name = 'occurences')

x_max_us <- max(us_df2$date) - exp_lab_shift
y_min_us <- 1 #min(us_df2$occurences) + 1
exp_label_us <- paste0(c("Exp base: ", as.character(exp_us), ""), collapse = '')

us_melt_df <- us_df2 %>%
  filter(variable %in% c("cases", "deaths"))

time_str <- gsub(':', '_', gsub(' ', '+', Sys.time()))
us_cases_fname <- paste0(c(time_str,'_corona_US_wDeaths',  '.png'), collapse = '')

if(SAVE_PLOTS) {
  png(paste0('images/', us_cases_fname),
      width=7, height=5, units='in', res=PNG_RES)
}
ggplot(us_melt_df, aes(x=date, y=occurences, color=variable)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_breaks = d_major_break, date_labels = "%m-%d",
               date_minor_breaks = d_minor_break) +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=x_ax_sz))  +
  theme(legend.position="right") +
  ggtitle(paste("US: Reported Coronavirus Cases and Deaths", curr_date, sep= ' - '))
if(SAVE_PLOTS) dev.off()


y_breaks_us <- as.vector(c(1) %o% 10^(0:7))

us_cases_log_fname <- paste0(c(time_str,'_corona_US_log_wDeaths2',  '.png'), collapse = '')
if(SAVE_PLOTS) {
  png(paste0('images/', us_cases_log_fname),
      width=7, height=5, units='in', res=PNG_RES)
}
ggplot(us_df2, aes(x=date, y=occurences, color=variable)) +
  geom_line() +
  geom_point(size=1) +
  scale_x_date(date_breaks = d_major_break, date_labels = "%m-%d",
               date_minor_breaks = d_minor_break) +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=x_ax_sz))  +
  theme(legend.position="right") +
  scale_y_log10(breaks=y_breaks_us, minor_breaks=log10_minor_break()) +
  annotate(geom="label", x=x_max_us, y=y_min_us, label=exp_label_us, color="#619CFF", size=2.75) +
  ggtitle(paste("US: Reported Coronavirus Cases and Deaths [Log Scale]", curr_date, sep= ' - '))
if(SAVE_PLOTS) dev.off()

# *New* Cases (US) ------------------------------------------------------------
us_ts <- xts(order.by=us_df$date, us_df[,c("cases", "deaths")])
us_new_ts <- diff(us_ts)
us_new_df <- fortify(us_new_ts)
names(us_new_df)[1] <- "date"
us_melt_new_df <- melt(data = us_new_df, id.vars = "date",
                       measure.vars = c("cases", "deaths"),
                       value.name = 'occurences')
# us_melt_new_df

us_new_log_fname <- paste0(c(time_str,'_corona_US_new_log',  '.png'), collapse = '')
if(SAVE_PLOTS) {
  png(paste0('images/', us_new_log_fname),
      width=7, height=5, units='in', res=PNG_RES)
}
ggplot(us_melt_new_df, aes(x=date,y=occurences,colour=variable)) + 
  geom_line() +
  geom_point(size=1) + 
  theme(legend.position="right") +
  scale_x_date(date_breaks = d_major_break, date_labels = "%m-%d",
               date_minor_breaks = d_minor_break) +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=x_ax_sz))  +
  theme(legend.position="right") +
  # scale_y_continuous(trans='log10') +
  scale_y_log10(minor_breaks=log10_minor_break()) +
  ggtitle(paste("US: *New* Coronavirus Cases/Deaths [Log Scale]", curr_date, sep= ' - '))
if(SAVE_PLOTS) dev.off()

# -------------------------------------------------------------------------
# States Cases ------------------------------------------------------------
# -------------------------------------------------------------------------

us_df <- nyt_df %>%
  group_by(date) %>%
  summarise(cases= sum(cases), deaths=sum(deaths)) 

nyt_df

state_cases_df <- nyt_df %>%
  select("date", "state", "cases")
state_cases_df$date <- as.Date(state_cases_df$date)

state_max <- state_cases_df %>%
  group_by(state) %>%
  summarize(max=max(cases))
elim_region <- state_max %>% 
  filter(max < 100)

state_cases_df <- state_cases_df %>%
  filter(!(state %in% elim_region$state))

filtered_cases_df <- state_cases_df %>%
  filter(cases >= 10)

filtered_cases_df <- filtered_cases_df %>%
  group_by(state) %>%
  mutate(days_since_10=row_number(date))
filtered_cases_df$days_since_10 <- filtered_cases_df$days_since_10 - 1

# breaks=c(10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000, 100000)
# y_breaks <- c(as.vector(c(1) %o% 10^(1:4)), 10^5)
y_max <- max(filtered_cases_df$cases)
y_breaks <- c(as.vector(c(1, 2, 5) %o% 10^(1:6)))
exp_last <- which.max(y_breaks[y_breaks<= y_max]) + 1
y_breaks <- y_breaks[1:exp_last]
x_max <- max(filtered_cases_df$days_since_10)
x_max_pad <- x_max + 10
x_breaks <- seq(0, x_max_pad+1, x_major_spacing)
x_minor <-seq(0, x_max_pad+1, x_minor_spacing)

time_str <- gsub(':', '_', gsub(' ', '+', Sys.time()))
state_cases_fname <- paste0(c(time_str,'_corona_US_state_cases_standard',  '.png'), collapse = '')

# Standard Plot -----------------------------------------------------------
if(SAVE_PLOTS) {
  png(paste0('images/', state_cases_fname),
      width=7, height=5, units='in', res=PNG_RES)
}
ggplot(filtered_cases_df, aes(x=days_since_10, y=cases, color=state)) +
  geom_line(size = 0.8) +
  # geom_point(pch = 21, size = 1) +
  geom_shadowtext(aes(label = paste0(" ",state)),
                  hjust=0, vjust = 0, angle = -15,
                  data = . %>% group_by(state)
                  %>% top_n(1, days_since_10),
                  bg.color = "white", size=2.2) +
  # formatting
  scale_y_log10(expand = expansion(add = c(0, 0.1)), 
                minor_breaks=log10_minor_break(), 
                breaks = y_breaks, labels = y_breaks) +
  scale_x_continuous(expand = expansion(add = c(0, 1)),
                     breaks=x_breaks, minor_breaks = x_minor,
                     limits=c(0, x_max_pad)) +
  coord_cartesian(clip = "off") +
  theme(axis.text.x=element_text(angle=0, hjust=1, size=x_ax_sz))  +
  theme(# panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(3,15,3,3,"mm")) +
  labs(x = "Number of days since 10th case", y = "", 
       subtitle = paste("Total Number of Reported Cases by State [Log Scale]", 
                        curr_date, sep= ' - '))
if(SAVE_PLOTS) dev.off()

# Emphasis Plot -----------------------------------------------------------
emph_thresh <- 400000
bg_states <- state_max %>% 
  filter((max >= 100) & (max < emph_thresh)) %>%
  select(state)

emph_states <- state_max %>% 
  filter(max >= emph_thresh) %>%
  select(state)

bg_states_df <- filtered_cases_df %>%
  filter(state %in% bg_states$state)
emph_states_df <- filtered_cases_df %>%
  filter(state %in% emph_states$state)
bg_color = 'gray80'
llwd = .7
x_repel_lim <- c(35, NA)

state_cases_emph_fname <- paste0(c(time_str,'_corona_US_state_cases_emphasis',  '.png'), collapse = '')

if(SAVE_PLOTS) {
  png(paste0('images/', state_cases_emph_fname),
      width=7, height=5, units='in', res=PNG_RES)
}
# Bg States
ggplot(bg_states_df, aes(x=days_since_10, y=cases)) +
  geom_line(aes(group=state), size = llwd, colour=bg_color, alpha=.35) +
  geom_shadowtext(aes(label = paste0(" ",state)),
                  hjust=0, vjust = 0, angle = -15,
                  data = . %>% group_by(state)
                  %>% top_n(1, days_since_10),
                  color = bg_color,
                  bg.color = "white", size=2.2) +
  # Emphasis States
  geom_line(data=emph_states_df, aes(colour=state), size = llwd) +
  geom_point(data=emph_states_df %>%
               group_by(state) %>%
               arrange(days_since_10) %>%
               slice(n()) %>%
               ungroup, 
             aes(colour=state), pch = 21, size = 1.25) +
  
  geom_label_repel(data = emph_states_df %>% group_by(state)
                   %>% top_n(1, days_since_10),
                   aes(label = paste0("",state), colour=state),
                   xlim=x_repel_lim, size=2.75, 
                   segment.colour = 'black', segment.alpha = .5,
                   label.padding = 0.15) +
  # Formatting
  scale_y_log10(expand = expansion(add = c(0, 0.1)),
                minor_breaks=log10_minor_break(), 
                breaks = y_breaks, labels = y_breaks) +
  scale_x_continuous(expand = expansion(add = c(0, 1)),
                     breaks=x_breaks, minor_breaks = x_minor,
                     limits=c(0, x_max_pad))+
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=0, hjust=1, size=x_ax_sz))  +
  theme(legend.position = "none",
        plot.margin = margin(3,15,3,3,"mm")) +
  # xlim(0, x_max+10) +
  labs(x = "Number of days since 10th case", y = "", 
       subtitle = paste("Total Number of Reported Cases by State [Log Scale]", 
                        curr_date, sep= ' - '))
if(SAVE_PLOTS) dev.off()

# -------------------------------------------------------------------------
# States Deaths ------------------------------------------------------------
# -------------------------------------------------------------------------

# start from corona_states_plot.R
state_deaths_df <- nyt_df %>%
  select("date", "state", "deaths")
state_deaths_df$date <- as.Date(state_deaths_df$date)

death_thresh <- 10
state_death_max <- state_deaths_df %>%
  group_by(state) %>%
  summarize(max=max(deaths))

elim_region_deaths <- state_death_max %>% 
  filter(max < death_thresh)

state_deaths_df <- state_deaths_df %>%
  filter(!(state %in% elim_region_deaths$state))

filtered_deaths_df <- state_deaths_df %>%
  filter(deaths >= 1)

filtered_deaths_df <- filtered_deaths_df %>%
  group_by(state) %>%
  mutate(days_since_1=row_number(date))
filtered_deaths_df$days_since_1 <- filtered_deaths_df$days_since_1 - 1

# breaks=c(10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000, 100000)
# y_breaks <- c(as.vector(c(1) %o% 10^(1:4)), 10^5)
# y_death_breaks <- c(as.vector(c(1, 2, 5) %o% 10^(0:4)))#, 10^5, 2*10^5)
y_max <- max(filtered_deaths_df$deaths)
y_death_breaks <- c(as.vector(c(1, 2, 5) %o% 10^(0:6)))
exp_last <- which.max(y_death_breaks[y_death_breaks <= y_max]) + 1
y_death_breaks <- y_death_breaks[1:exp_last]

x_death_max <- max(filtered_cases_df$days_since_10)
x_max_pad <- 10
x_death_breaks <- seq(0, x_death_max+x_max_pad, x_major_spacing)
x_death_minor <-seq(0, x_death_max+x_max_pad, x_minor_spacing)
x_limits <- c(0, x_death_max+x_max_pad)

time_str <- gsub(':', '_', gsub(' ', '+', Sys.time()))
state_deaths_fname <- paste0(c(time_str,'_corona_US_state_deaths_standard',  '.png'), collapse = '')

# Standard Plot -----------------------------------------------------------
if(SAVE_PLOTS) {
  png(paste0('images/', state_deaths_fname),
      width=7, height=5, units='in', res=PNG_RES)
}
ggplot(filtered_deaths_df, aes(x=days_since_1, y=deaths, color=state)) +
  geom_line(size = 0.8) +
  # geom_point(pch = 21, size = 1) +
  geom_shadowtext(aes(label = paste0(" ",state)),
                  data = . %>% group_by(state)
                  %>% top_n(1, days_since_1),
                  hjust=0, vjust = 0, angle = -15,
                  bg.color = "white", size = 2.2) +
  # formatting
  scale_y_log10(expand = expansion(add = c(0, 0.1)), 
                minor_breaks=log10_minor_break(), 
                breaks = y_death_breaks, labels = y_death_breaks) +
  scale_x_continuous(expand = expansion(add = c(0, 1)),
                     breaks=x_death_breaks, minor_breaks = x_death_minor,
                     limits=x_limits) +
  coord_cartesian(clip = "off") +
  theme(axis.text.x=element_text(angle=0, hjust=1, size=x_ax_sz))  +
  theme(# panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(3,15,3,3,"mm")) +
  labs(x = "Number of days since 1st death", y = "", 
       subtitle = paste("Total Number of Reported Deaths by State [Log Scale]", 
                        curr_date, sep= ' - '))
if(SAVE_PLOTS) dev.off()

# Emphasis Plot -----------------------------------------------------------
emph_death_thresh <- 8000
bg_death_states <- state_death_max %>% 
  filter((max >= death_thresh) & (max < emph_death_thresh)) %>%
  select(state)

emph_death_states <- state_death_max %>% 
  filter(max >= emph_death_thresh) %>%
  select(state)

bg_death_states_df <- filtered_deaths_df %>%
  filter(state %in% bg_death_states$state)
emph_death_states_df <- filtered_deaths_df %>%
  filter(state %in% emph_death_states$state)
bg_color = 'gray80'
llwd = .7
# x_death_repel_lim <- c(35, NA)

state_deaths_emph_fname <- paste0(c(time_str,'_corona_US_state_deaths_emphasis',  '.png'), collapse = '')

if(SAVE_PLOTS) {
  png(paste0('images/', state_deaths_emph_fname),
      width=7, height=5, units='in', res=PNG_RES)
}
# Bg States
ggplot(bg_death_states_df, aes(x=days_since_1, y=deaths)) +
  geom_line(aes(group=state), size = llwd, colour=bg_color, alpha=.35) +
  geom_shadowtext(aes(label = paste0(" ",state)),
                  hjust=0, vjust = 0, angle = -15,
                  data = . %>% group_by(state)
                  %>% top_n(1, days_since_1),
                  color = bg_color,
                  bg.color = "white", size=2.2) +
  # Emphasis States
  geom_line(data=emph_death_states_df, aes(colour=state), size = llwd) +
  geom_point(data=emph_death_states_df %>%
               group_by(state) %>%
               arrange(days_since_1) %>%
               slice(n()) %>%
               ungroup, 
             aes(colour=state), pch = 21, size = 1.25) +
  geom_label_repel(data = emph_death_states_df %>% 
                       group_by(state) %>% 
                       top_n(1, days_since_1),
                   aes(label = paste0("", state), colour=state),
                   nudge_x = 4,
                   # xlim=x_death_repel_lim, 
                   size=2, 
                   segment.colour = 'black', 
                   segment.alpha = .5,
                   label.padding = 0.13) +
  # Formatting
  scale_y_log10(expand = expansion(add = c(0, 0.1)),
                minor_breaks=log10_minor_break(), 
                breaks = y_death_breaks, labels = y_death_breaks) +
  scale_x_continuous(expand = expansion(add = c(0, 1)),
                     breaks=x_death_breaks, minor_breaks = x_death_minor,
                     limits=x_limits) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=0, hjust=1, size=x_ax_sz))  +
  theme(legend.position = "none",
        plot.margin = margin(3,15,3,3,"mm")) +
  labs(x = "Number of days since 1st death", y = "", 
       subtitle = paste("Total Number of Reported Deaths by State [Log Scale]", 
                        curr_date, sep= ' - '))
if(SAVE_PLOTS) dev.off()

# -------------------------------------------------------------------------
# Cases Per Capita Plot ---------------------------------------------------
# -------------------------------------------------------------------------
# pop_census_url <- 'http://www2.census.gov/programs-surveys/popest/datasets/2010-2019/national/totals/nst-est2019-alldata.csv'
# download.file(url = pop_census_url, destfile = fname_pop)
fname_pop <- paste('data/us_pop_data.csv')#', curr_time_str, sep='-')
# fname_pop
pop_df <- read.csv(fname_pop)
colnames(pop_df)

# pop_census_age_url <- 'https://www2.census.gov/programs-surveys/popest/tables/2010-2018/state/asrh/sc-est2018-agesex-civ.csv'
# download.file(url = pop_census_age_url, destfile = fname_pop_age)
fname_pop_age <- paste('data/us_pop_age_data.csv') #, curr_time_str, sep='-')
# fname_pop_age
age_df <- read.csv(fname_pop_age)
colnames(age_df)

states_df <- pop_df %>% 
  filter(STATE > 0)%>%
  select(c(NAME, POPESTIMATE2019))

cases_pop_df <- merge(x=state_cases_df, y=states_df, 
                      by.x='state', by.y='NAME', all.x=TRUE)
colnames(cases_pop_df) <- c('state', 'date', 'cases', 'pop_2019')
case_scaling <- 100000
cases_pop_df <- cases_pop_df %>% 
  mutate(cases_per_100k_people = cases / pop_2019 * case_scaling)
# cases_pop_df


#    cases       100000 people
# ----------- x ----------- 
# per person     per 10,000 people

p1_pc <- 1
case_sc_comma <- format(case_scaling, big.mark = ',', scientific=F)
xlab_title <- paste0(c('Number of days since ', p1_pc ,
                       ' [Case Per ', case_sc_comma, ' People]'), collapse='') 

state_max <- cases_pop_df %>%
  group_by(state) %>%
  summarize(max=max(cases_per_100k_people))

cases_pop_filtered_df <- cases_pop_df %>%
  filter(cases_per_100k_people >= p1_pc)

cases_pop_filtered_df <- cases_pop_filtered_df %>%
  group_by(state) %>%
  mutate(days_since_p1_pc=row_number(date))

cases_pop_filtered_df$days_since_p1_pc <- cases_pop_filtered_df$days_since_p1_pc - 1

# time_str <- gsub(':', '_', gsub(' ', '+', Sys.time()))
state_cases_pc_fname <- paste0(c(time_str,
                                 '_corona_US_state_cases_pc_std',
                                 '.png'), collapse = '')
# Standard per capita plot ------------------------------------------------
bg_color = 'gray80'
llwd = .5
x_repel_lim <- c(35, NA)

y_breaks <- c(as.vector(c(1, 2, 5) %o% 10^(-1:6)))
y_max <- max(cases_pop_filtered_df$cases_per_100k_people)
exp_last <- which.max(y_breaks[y_breaks <= y_max]) + 1
y_breaks <- y_breaks[1:exp_last]
y_limits <- c(p1_pc, y_max*1.1)

x_max <- max(cases_pop_filtered_df$days_since_p1_pc)
x_max_pad <- x_max + 10
x_breaks <- seq(0, x_max_pad+1, x_major_spacing)
x_minor <-seq(0, x_max_pad+1, x_minor_spacing)
tt_txt <- paste0(c('Reported Cases Per ', case_sc_comma, ' People by State [Log Scale] - ', 
                   as.character(curr_date)), collapse = '') 

if(SAVE_PLOTS) {
  png(paste0('images/', state_cases_pc_fname),
      width=7, height=5, units='in', res=PNG_RES)
}
ggplot(cases_pop_filtered_df, 
       aes(x=days_since_p1_pc, y=cases_per_100k_people, color=state)) +
  geom_line(size = llwd) +
  # geom_point(pch = 21, size = 1) +
  geom_shadowtext(aes(label = paste0(" ",state)),
                  hjust=0, vjust = 0, angle = -15,
                  data = . %>% group_by(state)
                  %>% top_n(1, days_since_p1_pc),
                  bg.color = "white", size=2.2) +
  # formatting
  scale_y_log10(expand = expansion(add = c(0, 0.1)),
                minor_breaks=log10_minor_break(), 
                breaks = y_breaks, labels = y_breaks,
                limits=y_limits) +
  scale_x_continuous(expand = expansion(add = c(0, 1)),
                     breaks=x_breaks, minor_breaks = x_minor,
                     limits=c(0, x_max+10)) +
  coord_cartesian(clip = "off") +
  theme(axis.text.x=element_text(angle=0, hjust=1, size=x_ax_sz))  +
  theme(# panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(3,15,3,3,"mm")) +
  labs(x = xlab_title,
       y = "", subtitle = tt_txt)
if(SAVE_PLOTS) dev.off()

# Emphasis Plot -----------------------------------------------------------
emph_thresh <- 7000
bg_states <- state_max %>% 
  filter((max >= p1_pc) & (max < emph_thresh)) %>%
  select(state)

emph_states <- state_max %>% 
  filter(max >= emph_thresh) %>%
  select(state)

bg_states_df <- cases_pop_filtered_df %>%
  filter(state %in% bg_states$state)
emph_states_df <- cases_pop_filtered_df %>%
  filter(state %in% emph_states$state)

# Standard per capita plot ------------------------------------------------
state_cases_pc_emph_fname <- paste0(c(time_str,
                                      '_corona_US_state_cases_pc_emph',
                                      '.png'), collapse = '')

if(SAVE_PLOTS) {
  png(paste0('images/', state_cases_pc_emph_fname),
      width=7, height=5, units='in', res=PNG_RES)
}
ggplot(bg_states_df, aes(x=days_since_p1_pc, y=cases_per_100k_people)) +
  geom_line(aes(group=state), size = llwd, colour=bg_color, alpha=.35) +
  geom_shadowtext(aes(label = paste0(" ",state)),
                  hjust=0, vjust = 0, angle = -15,
                  data = . %>% group_by(state)
                  %>% top_n(1, days_since_p1_pc),
                  color = bg_color,
                  bg.color = "white", size=2.2) +
  # Emphasis States
  geom_line(data=emph_states_df, aes(colour=state), size = llwd) +
  geom_point(data=emph_states_df %>%
               group_by(state) %>%
               arrange(days_since_p1_pc) %>%
               slice(n()) %>%
               ungroup, 
             aes(colour=state), pch = 21, size = 1.25) +
  
  geom_label_repel(data = emph_states_df %>% group_by(state)
                   %>% top_n(1, days_since_p1_pc),
                   aes(label = paste0("",state), colour=state),
                   xlim=x_repel_lim, size=2.25, 
                   segment.colour = 'black', segment.alpha = .5,
                   label.padding = 0.15) +
  # Formatting
  # xlim(0, x_max+10) +
  scale_y_log10(expand = expansion(add = c(0, 0.1)),
                minor_breaks=log10_minor_break(), 
                breaks = y_breaks, labels = y_breaks,
                limits=y_limits) +
  scale_x_continuous(expand = expansion(add = c(0, 1)),
                     breaks=x_breaks, minor_breaks = x_minor,
                     limits=c(0, x_max+10)) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=0, hjust=1, size=x_ax_sz))  +
  theme(legend.position = "none",
        plot.margin = margin(3, 15, 3, 3, "mm")) +
  labs(x = xlab_title, 
       y = "", subtitle = tt_txt)
if(SAVE_PLOTS) dev.off()

# -------------------------------------------------------------------------
# Deaths Per Capita Plot ---------------------------------------------------
# -------------------------------------------------------------------------

deaths_pop_df <- merge(x=state_deaths_df, y=states_df, 
                       by.x='state', by.y='NAME', all.x=TRUE)
colnames(deaths_pop_df) <- c('state', 'date', 'deaths', 'pop_2019')
death_scaling = 10000
deaths_pop_df <- deaths_pop_df %>% 
  mutate(deaths_per_10k_people = deaths / pop_2019 * death_scaling)

# deaths_pop_df

#    deaths       100000 people
# ----------- x ----------- 
# per person     per 10,000 people

p1_pc <- .1
death_sc_comma <- format(death_scaling, big.mark = ',')
xlab_title <- paste0(c('Number of days since ', p1_pc ,
                       ' [Deaths Per ', death_sc_comma, ' People]'), collapse='')
# "Number of days since .1 [Deaths Per 10K People]"

state_max <- deaths_pop_df %>%
  group_by(state) %>%
  summarize(max=max(deaths_per_10k_people))

deaths_pop_filtered_df <- deaths_pop_df %>%
  filter(deaths_per_10k_people >= p1_pc)

deaths_pop_filtered_df <- deaths_pop_filtered_df %>%
  group_by(state) %>%
  mutate(days_since_p1_pc=row_number(date))

deaths_pop_filtered_df$days_since_p1_pc <- deaths_pop_filtered_df$days_since_p1_pc - 1

# time_str <- gsub(':', '_', gsub(' ', '+', Sys.time()))
state_deaths_pc_fname <- paste0(c(time_str,
                                  '_corona_US_state_deaths_pc_std',
                                  '.png'), collapse = '')
# Standard per capita plot ------------------------------------------------
bg_color = 'gray80'
llwd = .5
x_repel_lim <- c(35, NA)

y_breaks <- c(as.vector(c(1, 2, 5) %o% 10^(-1:6)))
y_max <- max(deaths_pop_filtered_df$deaths_per_10k_people)
exp_last <- which.max(y_breaks[y_breaks <= y_max]) + 1
y_breaks <- y_breaks[1:exp_last]
y_limits <- c(p1_pc, y_max*1.1)

x_max <- max(deaths_pop_filtered_df$days_since_p1_pc)
x_breaks <- seq(0, x_max+1, x_major_spacing)
x_minor <-seq(0, x_max+1, x_minor_spacing)

tt_txt <- paste0(c('Reported Deaths Per ', death_sc_comma, ' People by State [Log Scale] - ', 
                as.character(curr_date)), collapse = '') 

if(SAVE_PLOTS) {
  png(paste0('images/', state_deaths_pc_fname),
      width=7, height=5, units='in', res=PNG_RES)
}
ggplot(deaths_pop_filtered_df, 
       aes(x=days_since_p1_pc, y=deaths_per_10k_people, color=state)) +
  geom_line(size = llwd) +
  # geom_point(pch = 21, size = 1) +
  geom_shadowtext(aes(label = paste0(" ",state)),
                  hjust=0, vjust = 0, angle = -15,
                  data = . %>% group_by(state)
                  %>% top_n(1, days_since_p1_pc),
                  bg.color = "white", size=2.2) +
  # formatting
  scale_y_log10(expand = expansion(add = c(0, 0.1)),
                minor_breaks=log10_minor_break(), 
                breaks = y_breaks, labels = y_breaks,
                limits=y_limits) +
  scale_x_continuous(expand = expansion(add = c(0, 1)),
                     breaks=x_breaks, minor_breaks = x_minor) +
  coord_cartesian(clip = "off") +
  theme(axis.text.x=element_text(angle=0, hjust=1, size=x_ax_sz))  +
  theme(# panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(3,15,3,3,"mm")) +
  labs(x = xlab_title, y = "", subtitle = tt_txt)
if(SAVE_PLOTS) dev.off()

# Emphasis Plot -----------------------------------------------------------
emph_thresh <- 10
bg_states <- state_max %>% 
  filter((max >= p1_pc) & (max < emph_thresh)) %>%
  select(state)

emph_states <- state_max %>% 
  filter(max >= emph_thresh) %>%
  select(state)

bg_states_df <- deaths_pop_filtered_df %>%
  filter(state %in% bg_states$state)
emph_states_df <- deaths_pop_filtered_df %>%
  filter(state %in% emph_states$state)

# Standard per capita plot ------------------------------------------------
state_deaths_pc_emph_fname <- paste0(c(time_str,
                                       '_corona_US_state_deaths_pc_emph',
                                       '.png'), collapse = '')
if(SAVE_PLOTS) {
  png(paste0('images/', state_deaths_pc_emph_fname),
      width=7, height=5, units='in', res=PNG_RES)
}
ggplot(bg_states_df, aes(x=days_since_p1_pc, y=deaths_per_10k_people)) +
  geom_line(aes(group=state), size = llwd, colour=bg_color, alpha=.35) +
  geom_shadowtext(aes(label = paste0(" ",state)),
                  hjust=0, vjust = 0, angle = -15,
                  data = . %>% group_by(state)
                  %>% top_n(1, days_since_p1_pc),
                  color = bg_color,
                  bg.color = "white", size=2.2) +
  # Emphasis States
  geom_line(data=emph_states_df, aes(colour=state), size = llwd) +
  geom_point(data=emph_states_df %>%
               group_by(state) %>%
               arrange(days_since_p1_pc) %>%
               slice(n()) %>%
               ungroup, 
             aes(colour=state), pch = 21, size = 1.25) +
  
  geom_label_repel(data = emph_states_df %>% group_by(state)
                   %>% top_n(1, days_since_p1_pc),
                   aes(label = paste0("",state), colour=state),
                   xlim=x_repel_lim, size=2.25, 
                   segment.colour = 'black', segment.alpha = .5,
                   label.padding = 0.15) +
  # Formatting
  scale_y_log10(expand = expansion(add = c(0, 0.1)),
                minor_breaks=log10_minor_break(),
                breaks = y_breaks, labels = y_breaks,
                limits=y_limits) +
  scale_x_continuous(expand = expansion(add = c(0, 1)),
                     breaks=x_breaks, minor_breaks = x_minor) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=0, hjust=1, size=x_ax_sz))  +
  theme(legend.position = "none",
        plot.margin = margin(3, 15, 3, 3, "mm")) +
  labs(x = xlab_title, 
       y = "", subtitle = tt_txt)
if(SAVE_PLOTS) dev.off()

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# CA & NY County Level Info -----------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# setwd('personal/projects/corona/')

curr_date <- Sys.Date() - 1
curr_date_str <- gsub('-','_', curr_date)
curr_time <- Sys.time() #- 1
curr_time_str <- gsub('-|:','_', curr_time)
curr_time_str <- gsub(' ','-', curr_time_str)

fname_jhu <- paste0(paste('data/corona_jhu', curr_time_str, sep='-'), '.csv')
fname_jhu

if(!file.exists(fname_jhu)) {
  # nyt_us_states_url <- 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv'
  # https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv
  jhu_url <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv'
  download.file(url = jhu_url, destfile = fname_jhu)
}
jhu_df <- read.csv(fname_jhu)

# Deaths Github Page: https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv
# Deaths CSV: https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv

# head(jhu_df)
ny_county_df <- jhu_df %>% 
  filter(Admin2 != '',
         Province_State == 'New York',
         !(Admin2 %in% c('Out of NY', 'Unassigned')))

dim(ny_county_df)
# ny_county_df$Admin2
# head(ny_county_df)
# colnames(ny_county_df)

df_ids <- c("UID", "iso2", "iso3", "code3", "FIPS", "Admin2", "Province_State", 
            "Country_Region", "Lat", "Long_", "Combined_Key" )
new_ny_df <- melt(ny_county_df, id=df_ids)
new_ny_df <- new_ny_df %>% 
  rename(date = variable,
         cases = value)
new_ny_df$date <- as.Date(gsub('X', '', new_ny_df$date), format = "%m.%d.%y")
# head(new_ny_df)

filtered_ny_df <- new_ny_df %>%
  filter(
    # cases > 0
    date >= '2020-03-01'
  )

# All NY Counties Plot -----------------------------------------------------------
# x_ax_sz <- 6
x_max <- max(filtered_ny_df$date)
x_min <- min(filtered_ny_df$date)
x_max_pad <- x_max + 56
# x_minor <-seq(0, x_max_pad+1, x_minor_spacing)

y_max <- max(filtered_ny_df$cases)
y_break_max <- 10^(ceiling(log10(y_max)))
y_break_max_power <- log10(y_break_max)
y_breaks <- c(as.vector(c(1, 2, 5) %o% 10^(0:y_break_max_power)))
y_limits = c(1, y_max * 1.3)

plot_title <-  paste("Cumulative Number of Cases by NY County [Log Scale]",
                     curr_date, sep= ' - ')

time_str <- gsub(':', '_', gsub(' ', '+', Sys.time()))
ny_counties_fname <- paste0(c(time_str,'_corona_NY_Counties_Cases',  '.png'), collapse = '')
if(SAVE_PLOTS) {
  png(paste0('images/', ny_counties_fname),
      width=7, height=5, units='in', res=PNG_RES)
}
ggplot(filtered_ny_df, aes(x=date, y=cases, color=Admin2)) +
  geom_line(size = 0.8) +
  # geom_shadowtext(aes(label = paste0(" ", Admin2)),
  #                 hjust=0, vjust = 0, angle = -15,
  #                 data = . %>% group_by(Admin2)
  #                          %>% top_n(1, date),
  # bg.color = "white", size=2.2) +
  # geom_text_repel(data = . %>% group_by(Admin2)
  geom_label_repel(data = . %>% group_by(Admin2)
                   %>% top_n(1, date),
                   aes(label = paste0("", Admin2), colour=Admin2),
                   size=1.5,
                   # direction = "x",
                   nudge_x = 10,
                   hjust = 0,
                   force = 1.5, 
                   xlim = c(x_max, x_max_pad),
                   # show.legend = FALSE,
                   segment.color = 'black',
                   segment.alpha = .3,
                   label.padding = 0.1,
                   max.iter=6000,
                   seed = 123) +
  # formatting
  scale_x_date(expand = expansion(add = c(0, 1)),
               date_breaks = "10 day", date_labels = "%m/%d",
               limits=c(x_min, x_max_pad)) + 
  scale_y_log10(expand = expansion(add = c(0, 0.1)),
                minor_breaks=log10_minor_break(),
                breaks = y_breaks, labels = y_breaks,
                limits=y_limits) +
  coord_cartesian(clip = "off") +
  theme(axis.text.x=element_text(angle=-45, hjust=1, size=x_ax_sz))  +
  theme(legend.position = "none",
        plot.margin = margin(3,15,3,3,"mm")) + 
  labs(x = "Date", y = "", subtitle = plot_title)
if(SAVE_PLOTS) dev.off()

# Emphasis Plot -----------------------------------------------------------

# Map Data ----------------------------------------------------------------
date_max <- max(new_ny_df$date)
ny_recent_df <- new_ny_df %>% 
  filter(date == date_max)
ny_recent_tab <- tibble(ny_recent_df)
ny_recent_tab$FIPS <- as.character(ny_recent_tab$FIPS)

counties <- urbnmapr::counties %>% 
  rename(FIPS = county_fips)
# counties
# legend

ny_case_breaks <- round(quantile(ny_recent_tab$cases, probs = seq(0, 1, by= 0.1)))

time_str <- gsub(':', '_', gsub(' ', '+', Sys.time()))
ny_counties_map_fname <- paste0(c(time_str,'_corona_NY_Counties_Cases_map',  '.png'), collapse = '')
plot_title <-  paste0(c("Cumulative Number of Covid-19 Cases by NY County (as of ", 
                        as.character(curr_date),")"), collapse='')

if(SAVE_PLOTS) {
  png(paste0('images/', ny_counties_map_fname),
      width=7, height=5, units='in', res=PNG_RES)
}
ny_recent_tab %>% 
  left_join(counties, by = "FIPS") %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = cases)) +
  geom_polygon(color = "#ffffff", size = .25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  # scale_fill_gradientn(labels = scales::percent,
  #                      guide = guide_colorbar(title.position = "top")) +
  # coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  # theme(legend.title = element_text(),
  #       legend.key.width = unit(.5, "in")) +
  # scale_fill_gradient(
  scale_fill_gradientn(
    colors=rev(brewer.pal(n=11, name='RdYlBu')),
    breaks=as.vector(ny_case_breaks),
    label=scales::comma,
    #guide = guide_colorbar(title.position = "top")
    # oob=scales::squish
    trans = "log"
  ) +
  theme(# legend.key.size = 5
    legend.key.height = unit(.65, "in"),
    legend.key.width = unit(.4, "in"), 
    legend.text = element_text(size = 9, angle=0),
    legend.title = element_text(vjust = 4)
    # legend.title.align=1
  ) +
  labs(fill = "Number of Cases", x = "", y = "", subtitle = plot_title)
if(SAVE_PLOTS) dev.off()

# + theme_urban_map()

# NY New Covid-19 Cases by Coumty --------------------------------------------------
ny_counties_new_cases_df <- filtered_ny_df %>%
  arrange(Admin2, date) %>%
  group_by(Admin2) %>%
  mutate(new_cases=c(NA, diff(cases))) %>%
  select(c('Admin2', 'FIPS', 'date','new_cases')) %>%
  mutate(FIPS=as.character(FIPS)) %>% 
  filter(!is.na(new_cases))
ny_counties_new_cases_df[ny_counties_new_cases_df$new_cases < 0, 'new_cases'] <- 0
# dim(ny_counties_new_cases_df)
# head(ny_counties_new_cases_df)
summary(ny_counties_new_cases_df)

# Moving Average to denoise data
ma_ks = c(1, 7, 14)
for (k in ma_ks){
  print(k)
  NA_vec <- rep(NA, k-1)
  ny_county_means_df <- ny_counties_new_cases_df %>%
    group_by(Admin2) %>%
    mutate(ma_cases=c(NA_vec, rollmean(new_cases, k=k, align='right'))) %>%
    filter(!is.na(ma_cases))
  
  # x_ax_sz <- 6
  # a_df <- ny_county_means_df
  x_max <- max(ny_county_means_df$date)
  x_min <- min(ny_county_means_df$date)
  x_max_pad <- x_max + 70
  # x_minor <-seq(0, x_max_pad+1, x_minor_spacing)
  
  y_max <- max(ny_county_means_df$ma_cases)
  y_break_max <- 10^(ceiling(log10(y_max)))
  y_break_max_power <- log10(y_break_max)
  y_breaks <- c(as.vector(c(1, 2, 5) %o% 10^(0:y_break_max_power)))
  y_limits = c(1, y_max * 1.3)
  
  plot_title <-  paste(c(k, 
                         '-day Moving Average of New Covid-19 Cases by NY County [Log Scale] - ',
                         as.character(curr_date)), collapse='')
  
  time_str <- gsub(':', '_', gsub(' ', '+', Sys.time()))
  ny_counties_new_fname <- paste0(c(time_str,
                                    '_corona_NY_Counties_New_Cases_',
                                    'MA', k,
                                    '.png'), collapse = '')
  if(SAVE_PLOTS) {
    png(paste0('images/', ny_counties_new_fname),
        width=7, height=5, units='in', res=PNG_RES)
  }
  gplot <- ggplot(ny_county_means_df, 
                  aes(x=date, y=ma_cases, color=Admin2)) +
    geom_line(size = 0.5, alpha=0.8) +
    geom_label_repel(data = . %>% group_by(Admin2)
                     %>% top_n(1, date),
                     aes(label = paste0("", Admin2), colour=Admin2),
                     size=1.5,
                     # direction = "x",
                     nudge_x = 10,
                     hjust = 0,
                     force = 1.5, 
                     xlim = c(x_max, x_max_pad),
                     # show.legend = FALSE,
                     segment.color = 'black',
                     segment.alpha = .3,
                     label.padding = 0.1,
                     max.iter=6000,
                     seed = 123) +
    # formatting
    scale_x_date(expand = expansion(add = c(0, 1)),
                 date_breaks = "14 day", date_labels = "%m/%d",
                 limits=c(x_min, x_max_pad)) + 
    scale_y_log10(expand = expansion(add = c(0, 0.1)),
                  minor_breaks=log10_minor_break(),
                  breaks = y_breaks, labels = y_breaks,
                  limits=y_limits) +
    coord_cartesian(clip = "off") +
    theme(axis.text.x=element_text(angle=-45, hjust=1, size=x_ax_sz))  +
    theme(legend.position = "none",
          plot.margin = margin(3,15,3,3,"mm")) + 
    labs(x = "Date", y = "", subtitle = plot_title)
  print(gplot)
  if(SAVE_PLOTS) dev.off()
}

# NY New Cases Map -----------------------------------------------------------
max_date <- max(ny_counties_new_cases_df$date)
max_new_ny_df <- ny_counties_new_cases_df %>%
  filter(date==max_date)

ny_new_case_breaks <- round(quantile(max_new_ny_df$new_cases, probs = seq(0, 1, by= 0.1)))

time_str <- gsub(':', '_', gsub(' ', '+', Sys.time()))
ny_cty_new_map_fname <- paste0(c(time_str,'_corona_NY_Counties_New_Cases_map',  '.png'), collapse = '')
plot_title <-  paste0(c("Number of New Covid-19 Cases by NY County (as of ", 
                        as.character(curr_date),")"), collapse='')

if(SAVE_PLOTS) {
  png(paste0('images/', ny_cty_new_map_fname),
      width=7, height=5, units='in', res=PNG_RES)
}
max_new_ny_df %>% 
  left_join(counties, by = "FIPS") %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = new_cases)) +
  geom_polygon(color = "#ffffff", size = .25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  # scale_fill_gradient(
  scale_fill_gradientn(
    colors=rev(brewer.pal(n=11, name='RdYlBu')),
    breaks=as.vector(ny_new_case_breaks),
    label=scales::comma,
    #guide = guide_colorbar(title.position = "top")
    # oob=scales::squish
    trans = "log"
  ) +
  theme(# legend.key.size = 5
    legend.key.height = unit(.65, "in"),
    legend.key.width = unit(.4, "in"), 
    legend.text = element_text(size = 9, angle=0),
    legend.title = element_text(vjust = 4)
    # legend.title.align=1
  ) +
  labs(fill = "Number of Cases", x = "", y = "", subtitle = plot_title)
if(SAVE_PLOTS) dev.off()



# California County -------------------------------------------------------
ca_county_df <- jhu_df %>% 
  filter(Admin2 != '',
         Province_State == 'California',
         !(Admin2 %in% c('Out of CA', 'Unassigned')))

# https://www.google.com/search?q=how+many+ca+counties
dim(ca_county_df)
# ca_county_df$Admin2
# head(ca_county_df)
# colnames(ca_county_df)

# df_ids <- c("UID", "iso2", "iso3", "code3", "FIPS", "Admin2", "Province_State", 
# "Country_Region", "Lat", "Long_", "Combined_Key" )
new_ca_df <- melt(ca_county_df, id=df_ids)
new_ca_df <- new_ca_df %>% 
  rename(date = variable,
         cases = value)
new_ca_df$date <- as.Date(gsub('X', '', new_ca_df$date), format = "%m.%d.%y")
# head(new_ny_df)
filtered_ca_df <- new_ca_df %>%
  filter(
    # cases > 0
    date >= '2020-03-01'
  )

# All CA Counties Plot -----------------------------------------------------------
# x_minor <-seq(0, x_max_pad+1, x_minor_spacing)
# x_ax_sz <- 6
x_max <- max(filtered_ca_df$date)
x_min <- min(filtered_ca_df$date)
x_max_pad <- x_max + 70

y_max <- max(filtered_ca_df$cases)
y_break_max <- 10^(ceiling(log10(y_max)))
y_break_max_power <- log10(y_break_max)
y_breaks <- c(as.vector(c(1, 2, 5) %o% 10^(0:y_break_max_power)))
y_limits = c(1, y_max * 1.4)

plot_title <-  paste("Cumulative Number of Cases by CA County [Log Scale]",
                     curr_date, sep= ' - ')
time_str <- gsub(':', '_', gsub(' ', '+', Sys.time()))
ca_counties_fname <- paste0(c(time_str,'_corona_CA_Counties_Cases',  '.png'), collapse = '')

if(SAVE_PLOTS) {
  png(paste0('images/', ca_counties_fname),
      width=7, height=5, units='in', res=PNG_RES)
}
ggplot(filtered_ca_df, aes(x=date, y=cases, color=Admin2)) +
  geom_line(size = 0.8) +
  # geom_shadowtext(aes(label = paste0(" ", Admin2)),
  #                 hjust=0, vjust = 0, angle = -15,
  #                 data = . %>% group_by(Admin2)
  #                          %>% top_n(1, date),
  # bg.color = "white", size=2.2) +
  # geom_text_repel(data = . %>% group_by(Admin2)
  geom_label_repel(data = . %>% group_by(Admin2)
                   %>% top_n(1, date),
                   aes(label = paste0("", Admin2), colour=Admin2),
                   size=1.5,
                   # direction = "x",
                   nudge_x = 10,
                   hjust = 0,
                   force = 2.0, 
                   xlim = c(x_max, x_max_pad),
                   # show.legend = FALSE,
                   segment.color = 'black',
                   segment.alpha = .3,
                   label.padding = 0.1,
                   max.iter=6000,
                   seed = 123) +
  # formatting
  scale_x_date(expand = expansion(add = c(0, 1)),
               date_breaks = "10 day", date_labels = "%m/%d",
               limits=c(x_min, x_max_pad)) + 
  scale_y_log10(expand = expansion(add = c(0, 0.1)),
                minor_breaks=log10_minor_break(),
                breaks = y_breaks, labels = y_breaks,
                limits=y_limits) +
  coord_cartesian(clip = "off") +
  theme(axis.text.x=element_text(angle=-45, hjust=1, size=x_ax_sz))  +
  theme(legend.position = "none",
        plot.margin = margin(3,15,3,3,"mm")) + 
  labs(x = "Date", y = "", subtitle = plot_title)
if(SAVE_PLOTS) dev.off()

# Map Data ----------------------------------------------------------------
date_max <- max(new_ca_df$date)
ca_recent_df <- new_ca_df %>% 
  filter(date == date_max)
ca_recent_tab <- tibble(ca_recent_df)
ca_recent_tab$FIPS <- as.character(ca_recent_tab$FIPS)
ca_recent_tab$FIPS <- paste0('0',ca_recent_tab$FIPS)
ca_counties <- counties %>% filter(state_abbv == 'CA')
# counties <- urbnmapr::counties %>%
#   rename(FIPS = county_fips)
# counties
# legend

ca_case_breaks <- round(quantile(ca_recent_tab$cases, probs = seq(0, 1, by= 0.1)))

time_str <- gsub(':', '_', gsub(' ', '+', Sys.time()))
ca_counties_map_fname <- paste0(c(time_str,'_corona_CA_Counties_Cases_map',  '.png'), collapse = '')
plot_title <-  paste0(c("Cumulative Number of Covid-19 Cases by CA County (as of ", 
                        as.character(curr_date),")"), collapse='')

if(SAVE_PLOTS) {
  png(paste0('images/', ca_counties_map_fname),
      width=7, height=5, units='in', res=PNG_RES)
}
ca_recent_tab %>% 
  left_join(ca_counties, by = "FIPS") %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = cases)) +
  geom_polygon(color = "#ffffff", size = .25) +
  # scale_fill_gradient(
  scale_fill_gradientn(
    colors=rev(brewer.pal(n=11, name='RdYlBu')),
    breaks=as.vector(ca_case_breaks),
    label=scales::comma,
    #guide = guide_colorbar(title.position = "top")
    # oob=scales::squish
    trans = "log"
  ) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(# legend.key.size = 5
    legend.key.height = unit(.65, "in"),
    legend.key.width = unit(.4, "in"), 
    legend.text = element_text(size = 9, angle=0),
    legend.title = element_text(vjust = 4)
    # legend.title.align=1
  ) +
  labs(fill = "Number of Cases", x = "", y = "", subtitle = plot_title)
if(SAVE_PLOTS) dev.off()


# CA New Covid-19 Cases by Coumty --------------------------------------------------
ca_counties_new_cases_df <- filtered_ca_df %>%
  arrange(Admin2, date) %>%
  group_by(Admin2) %>%
  mutate(new_cases=c(NA, diff(cases))) %>%
  select(c('Admin2', 'FIPS', 'date','new_cases')) %>%
  mutate(FIPS=as.character(FIPS)) %>% 
  filter(!is.na(new_cases))
ca_counties_new_cases_df[ca_counties_new_cases_df$new_cases < 0, 'new_cases'] <- 0
# dim(ca_counties_new_cases_df)
# head(ca_counties_new_cases_df)
# summary(ca_counties_new_cases_df)

# Moving Average to denoise data
ma_ks = c(1, 7, 14)
for (k in ma_ks){
  print(k)
  # k=14
  NA_vec <- rep(NA, k-1)
  ca_county_means_df <- ca_counties_new_cases_df %>%
    group_by(Admin2) %>%
    mutate(ma_cases=c(NA_vec, rollmean(new_cases, k=k, align='right'))) %>%
    filter(!is.na(ma_cases))
  
  # x_ax_sz <- 6
  # a_df <- ny_county_means_df
  x_max <- max(ca_county_means_df$date)
  x_min <- min(ca_county_means_df$date)
  x_max_pad <- x_max + 56
  # x_minor <-seq(0, x_max_pad+1, x_minor_spacing)
  
  y_max <- max(ca_county_means_df$ma_cases)
  y_break_max <- 10^(ceiling(log10(y_max)))
  y_break_max_power <- log10(y_break_max)
  y_breaks <- c(as.vector(c(1, 2, 5) %o% 10^(0:y_break_max_power)))
  y_limits = c(1, y_max * 1.3)
  
  plot_title <-  paste(c(k, 
                         '-day Moving Average of New Covid-19 Cases by CA County [Log Scale] - ',
                         as.character(curr_date)), collapse='')
  
  time_str <- gsub(':', '_', gsub(' ', '+', Sys.time()))
  ca_counties_new_fname <- paste0(c(time_str,
                                    '_corona_CA_Counties_New_Cases_',
                                    'MA', k,
                                    '.png'), collapse = '')
  
  if(SAVE_PLOTS) {
    png(paste0('images/', ca_counties_new_fname),
        width=7, height=5, units='in', res=PNG_RES)
  }
  gplot <- ggplot(ca_county_means_df, 
                  aes(x=date, y=ma_cases, color=Admin2)) +
    geom_line(size = 0.5, alpha=0.8) +
    geom_label_repel(data = . %>% group_by(Admin2)
                     %>% top_n(1, date),
                     aes(label = paste0("", Admin2), colour=Admin2),
                     size=1.5,
                     # direction = "x",
                     nudge_x = 10,
                     hjust = 0,
                     force = 1.5, 
                     xlim = c(x_max, x_max_pad),
                     # show.legend = FALSE,
                     segment.color = 'black',
                     segment.alpha = .3,
                     label.padding = 0.1,
                     max.iter=6000,
                     seed = 123) +
    # formatting
    scale_x_date(expand = expansion(add = c(0, 1)),
                 date_breaks = "10 day", date_labels = "%m/%d",
                 limits=c(x_min, x_max_pad)) + 
    scale_y_log10(expand = expansion(add = c(0, 0.1)),
                  minor_breaks=log10_minor_break(),
                  breaks = y_breaks, labels = y_breaks,
                  limits=y_limits) +
    coord_cartesian(clip = "off") +
    theme(axis.text.x=element_text(angle=-45, hjust=1, size=x_ax_sz))  +
    theme(legend.position = "none",
          plot.margin = margin(3,15,3,3,"mm")) + 
    labs(x = "Date", y = "", subtitle = plot_title)
  print(gplot)
  if(SAVE_PLOTS) dev.off()
}

# CA New Cases Map -----------------------------------------------------------
max_date <- max(ca_counties_new_cases_df$date)
max_new_ca_df <- ca_counties_new_cases_df %>%
  filter(date==max_date)
max_new_ca_df$FIPS <- paste0('0', max_new_ca_df$FIPS)
ca_new_case_breaks <- round(quantile(max_new_ca_df$new_cases, 
                                     probs = seq(0, 1, by= 0.1)))

time_str <- gsub(':', '_', gsub(' ', '+', Sys.time()))
ca_cty_new_map_fname <- paste0(c(time_str,
                                 '_corona_CA_Counties_New_Cases_map',
                                 '.png'), collapse = '')
plot_title <-  paste0(c("Number of New Covid-19 Cases by CA County (as of ", 
                        as.character(curr_date),")"), collapse='')

if(SAVE_PLOTS) {
  png(paste0('images/', ca_cty_new_map_fname),
      width=7, height=5, units='in', res=PNG_RES)
}
max_new_ca_df %>% 
  left_join(ca_counties, by = "FIPS") %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = new_cases)) +
  geom_polygon(color = "#ffffff", size = .25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  # scale_fill_gradient(
  scale_fill_gradientn(
    colors=rev(brewer.pal(n=11, name='RdYlBu')),
    breaks=as.vector(ca_new_case_breaks),
    label=scales::comma,
    #guide = guide_colorbar(title.position = "top")
    # oob=scales::squish
    # trans = "log"
    trans="pseudo_log"
  ) +
  theme(# legend.key.size = 5
    legend.key.height = unit(.65, "in"),
    legend.key.width = unit(.4, "in"), 
    legend.text = element_text(size = 9, angle=0),
    legend.title = element_text(vjust = 4)
    # legend.title.align=1
  ) +
  labs(fill = "Number of Cases", x = "", y = "", subtitle = plot_title)
if(SAVE_PLOTS) dev.off()


