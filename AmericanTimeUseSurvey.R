library(tidyverse)

# data from https://www.kaggle.com/bls/american-time-use-survey

setwd(dir = "C:/Users/Mauricio/Documents/UFSM/Semestre I/AmericanTimeUseSurvey")

df.resp <- read_csv('atusresp.csv')
df.act <- read_csv('atusact.csv', col_types=cols(tustarttim = col_character(), tustoptime = col_character())) 
df.sum <- read_csv('atussum.csv')

df.tmp <- df.act %>%
  mutate(activity = case_when(df.act$trtier2p == 1301 ~ 'Exercise', 
                              df.act$trtier1p ==  5 ~ 'Work', 
                              df.act$trtier1p ==  6 ~ 'Education', 
                              df.act$trtier1p ==  18 ~ 'Travel',
                              df.act$trtier1p == 11 ~ 'Eating',
                              df.act$trcodep  %in% c(120303, 120304) ~ 'TV',
                              df.act$trcodep  == 120307 ~ 'Playing games',
                              df.act$trtier1p == 12 ~ 'Relaxing',
                              df.act$trtier2p >= 301 & df.act$trtier2p <= 303 ~ 'Childcare',
                              df.act$trtier2p == 101 ~ 'Sleep',
                              df.act$trtier2p == 202 ~ 'Cooking',
                              df.act$trtier2p == 201 ~ 'Housework')) %>%
  filter(!is.na(activity)) %>%
  inner_join(df.resp, by='tucaseid') %>%
  inner_join(df.sum %>% select(tucaseid, teage), by='tucaseid') %>%
  separate(tustarttim, into=c('hour', 'minute', 'second')) %>%
  mutate(hour = as.integer(hour),
         minute = as.integer(minute)) %>%
  mutate(start.epoch = hour * 60 + minute,
         end.epoch = (start.epoch + tuactdur24)) %>%
  # Roll over midnight; when this happens add one point before midnight and one after
         {
           rbind(filter(., end.epoch >= 24*60) %>%
                   mutate(start.epoch = 0,
                          end.epoch = end.epoch - 24*60),
                 mutate(., end.epoch = pmin(end.epoch, 24*60-1)))
         } %>%
  mutate(activity = factor(activity),
         age = teage)

# For every by= minute interval, figure out what people where doing
# Then normalize by day and group by weekday/weekend
interval <- 30
df.tmp2 <- expand.grid(epoch = seq(0, 24*60-1, by=interval)) %>%
  as.tibble() %>%
  rowwise() %>%
  do({
    search <- .
    df.time <- df.tmp %>%
      filter(start.epoch <= search$epoch, end.epoch > search$epoch) %>%
      group_by(age, activity, tudiaryday) %>%
      summarize(score = sum(tufnwgtp),
                epoch = search$epoch)
    df.time
  }) %>%
  group_by(age, tudiaryday) %>%
  mutate(score = score / max(score)) %>%
  group_by(daytype = ifelse(tudiaryday >= 2 & tudiaryday <= 6, 'weekday', 'weekend'),
           activity, epoch, age) %>%
  summarize(score = sum(score)/n_distinct(tudiaryday))

instructions <- 'Early afternoon travel is teens getting home from school, while working adults get on the road a couple of hours later. Weekday exercise happens after school or work,
until you retire and get out more in the morning. But lunch happens at lunch no matter what age or day... Although it is quicker on weekdays.
Each slice represents an age group doing each activity throughout the day. Time flows left to right and age flows front to back.
To fit both common and rare activities, they are on different scales. Relative height makes sense within each activity.'

df.tmp2 %>%
  filter(age <= 76) %>%
  # Merge every second age, to get a good amount of lines
  group_by(daytype, activity, epoch, age = ceiling(age/2)*2) %>%
  summarize(score = sum(score)) %>%
  ungroup() %>%
  
  # Add a new data point for 24:00, which equals 00:00
  rbind(filter(., epoch == 0) %>% mutate(epoch=24*60)) %>%
  
  # Align start of chart to be 04:00, to fit the format of the data
  mutate(epoch = ifelse(epoch < 4*60, epoch + 24*60, epoch)) %>%
  
  # Add missing datapoints, as well as a start and end point for the polygon to be complete
  complete(daytype, activity, epoch=c(epoch, min(epoch)-0.001, max(epoch) + 0.001), age, fill=list(score=0)) %>%
  
  # Polygons need to be arranged properly to overlap as we'd like them to
  # Here we're definitely being "creative" with ggplot
  arrange(daytype, activity, epoch, age) %>%
  mutate(age.f = reorder(as.character(age), -age)) %>%
  
  # facet headings
  mutate(group_name = paste0(activity, ' - ', daytype)) %>%
  
  # Calculate each points y pos
  mutate(y = age + 150 * score/max(score)) %>%
  
  {
    ggplot(., aes(epoch/60, y)) +
      geom_polygon(aes(group=age.f, fill=activity, alpha=ifelse(activity %in% c('Education', 'Exercise'), 0, 1)), color='white', show.legend=FALSE, size=0.3) +
      geom_line(data=filter(., daytype == 'weekday', activity %in% c('Education', 'Exercise'), age <= 20), aes(group=age.f), color='white', linetype='dotted', alpha=0.3) +
      # Put an invisible dot at the activity's top y value, to make each activity share y scales
      # between weekend and weekday
      geom_point(data=group_by(., activity, group_name) %>%
                   summarize(y=max(y),
                             epoch=min(epoch)) %>%
                   mutate(y=max(y)), color=NA) +
      facet_wrap(~group_name, scales='free', ncol=6) +
      scale_y_continuous(breaks=1:5*15, labels=function(x) {paste0(x, 'yo')}) +
      scale_x_continuous(labels=function(x) {sprintf("%02d:00", as.integer(x %% 24))}, breaks=c(4, 8, 12, 16, 20, 24, 28)) +
      scale_alpha_continuous(range=c(0.7, 1)) +
      labs(x="", y="", title="The daily grind", subtitle=instructions, caption="Source: American Time Use Survey") +
      theme_linedraw(base_family='Arial Narrow') +
      theme(panel.grid = element_blank(),
            strip.text=element_text(size=12),
            plot.title=element_text(size=30),
            plot.subtitle=element_text(face='italic', size=14),
            plot.caption=element_text(face='italic'),
            axis.text = element_text(color='#666666'))
    
  }

ggsave('out.png', width=17, height=10)
