Fifa visualization
================

Data preparation
----------------

``` r
#load data
stats <- read.csv("D:/UDSC/Time series/player_stats.csv")
price <- read.csv("D:/UDSC/Time series/player_price.csv") 

#convert Unix epoch time to date object
price$timestamp <- anydate((price$timestamp/1000))

#convert price to numeric type
price$xbox_price <- as.numeric(price$xbox_price)

#divide cards on normal and special
stats$revision_binary <- ifelse(stats$revision == 'Normal', 'Normal', 'Special')
```

``` r
#extract the date conponents 
price$year_month <- format(as.Date(price$timestamp), '%Y-%m')
price$month <- months(price$timestamp)
price$weekday <- weekdays(price$timestamp)
price$day <- ifelse(price$weekday == 'Saturday' | price$weekday == 'Sunday', 'day off', 'workday')

#group players by their field positions
stats$position_group <- ifelse(stats$position == 'GK', 'Goalkeepers', 
                          ifelse(stats$position == 'CF' | stats$position == 'LW' | 
                          stats$position == 'RW' | stats$position == 'ST', 'Forwards', 
                           ifelse(stats$position == 'CAM' | stats$position == "CDM" |
                            stats$position == 'CM' | stats$position == 'LM', 'Midfielders', 'Backs')))
```

``` r
#merge two datasets
data <- left_join(price, stats, by = 'player_id')
```

Let's plot!
===========

Total day price of different card type

``` r
data %>%  
  group_by(timestamp, colour, revision_binary) %>%
  summarise(sum_xbox = sum(xbox_price)) %>%
  ggplot(aes(x = timestamp, y = sum_xbox, color = colour)) +
  facet_grid(revision_binary ~ ., scales = "free") +
  geom_line(size = 1.0, alpha = 0.8) +
  scale_x_date(date_labels = "%b %Y")+
  theme(legend.position = "right") +
  theme_tq() +
  labs(title = "Total day price of different cards type",
       x = "",
       y = "total xbox price",
       fill = '')+
  theme(plot.title = element_text(hjust = 0.5))
```

<img src=https://github.com/valentinadeshko/Time-series-Fifa2017/blob/master/Fifa_plots/fig-unnamed-chunk-6-1.png >

We can see that the gold cards are most expensive in both categories. That is why we will analize only gold cards price behaviour.

Mean daily xbox price of different revision type players

``` r
data %>% filter(colour == 'gold' & xbox_price != 0) %>%  
  group_by(timestamp, revision_binary) %>% 
  summarise(sum_xbox = mean(xbox_price)) %>%
  ggplot(aes(x = timestamp, y = sum_xbox, color = revision_binary))+
  facet_grid(revision_binary ~ ., scales = "free") +
  geom_line(size = 1.2, alpha = 0.8) +
  scale_color_manual(values = palette_light()) +
  scale_x_date(date_labels = "%b %Y")+
  theme(legend.position="bottom") +
  theme_tq() +
  labs(title = "Mean dayly xbox price of different revision type",
       x = "",
       y = "Mean day xbox price",
       color = "")+
  theme(plot.title = element_text(hjust = 0.5))
```

<img src=https://github.com/valentinadeshko/Time-series-Fifa2017/blob/master/Fifa_plots/fig-unnamed-chunk-7-1.png>

This plot says not too much, lets plot price distribution by categories for 'normal' and 'special' players.

``` r
data %>% filter(colour == 'gold' & xbox_price != 0) %>% 
  filter(revision_binary == "Normal") %>%
  group_by(year_month, revision_binary, xbox_price, position_group) %>% 
  ggplot(aes(x = xbox_price)) +
  facet_grid(position_group~., scale = 'free_y') +
  coord_cartesian(xlim=c(0, 500000)) +
  theme(legend.position="bottom") +
  geom_histogram(aes(fill = position_group), bins = 50)
```

<img src=https://github.com/valentinadeshko/Time-series-Fifa2017/blob/master/Fifa_plots/fig-unnamed-chunk-8-1.png>

If we loof closer, We can see wide price disrtibution for forwards position. Some of them have price higher then 400000. We should analize who these players are and why they have so hight price. Other positions of 'normal' players \#have approximately the same distribution.

``` r
data1 <- data %>% filter(colour == 'gold' & xbox_price > 400000 &
                 revision_binary == 'Normal' & position_group == 'Forwards') 
 print(as.data.table(unique(data1$player_name)))
##                   V1
## 1: Cristiano Ronaldo
## 2:              Bale
## 3:       Ibrahimovic
## 4:             Messi
## 5:           SuГЎrez
## 6:            Neymar
## 7:           AgГјero
```

We see that Top-players even with 'Normal' cards have higher price then others. Maybe we should use it as feature: players just with normal cards and player with both types.

Plot the distribution of 'special' players prices

``` r
data %>% filter(colour == 'gold' & xbox_price != 0) %>% 
  filter(revision_binary == "Special") %>%
  group_by(year_month, revision_binary, xbox_price, position_group) %>% 
  ggplot(aes(x = xbox_price)) +
  facet_grid(position_group~., scale = 'free_y') +
  coord_cartesian(xlim=c(0, 5000000))+
  theme(legend.position="bottom") +
  geom_histogram(aes(fill = position_group), bins = 50) 
```

<img src=https://github.com/valentinadeshko/Time-series-Fifa2017/blob/master/Fifa_plots/fig-unnamed-chunk-10-1.png>

We also can see that forwards have the highest price, but the other categories also have wide distribution and price rages.

Count and plot number of different revision types

``` r
revision_number <- data %>% filter(colour == 'gold' & xbox_price != 0 & year_month != '2017-06') %>% 
group_by(year_month, revision_binary) %>%
count(revision_binary) 
revision_number %>% group_by(year_month, revision_binary) %>% 
summarise(number = sum(n)) %>%
ggplot(aes(x = year_month, y = number, color = revision_binary)) + 
geom_point(size = 3) + geom_line(aes(color = revision_binary, group = revision_binary), size = 1) +
theme_tq() +
labs(title = "Total number of different revision types",
       x = "",
       y = "Number",
       color = "")+
theme(plot.title = element_text(hjust = 0.5))
```

<img src=https://github.com/valentinadeshko/Time-series-Fifa2017/blob/master/Fifa_plots/fig-unnamed-chunk-11-1.png>

For special cards total prise is increases but the mean price is decrease. This is because of increasing number of special cards/players.

Lets check the hypothesis of 'day\_of\_the\_week - price' dependency

``` r
data %>% filter(colour == 'gold' & 
  xbox_price != 0 & year_month != '2016-09' & year_month != '2016-10') %>% 
  group_by(timestamp, day, revision_binary) %>%
  summarise(mean_price = mean(xbox_price)) %>%
  ggplot(aes(x = timestamp, y = mean_price, color = day)) +
  facet_grid(revision_binary ~ ., scales = "free") +
  geom_line(size = 1.0) +
  theme_tq() + 
  labs(title = "Mean price on different week day",
       y = "",
       x = 'Date', 
       color = "") +
  theme(plot.title = element_text(hjust = 0.5))
```

<img src=https://github.com/valentinadeshko/Time-series-Fifa2017/blob/master/Fifa_plots/fig-unnamed-chunk-12-1.png>

We cann't say for sure thre is or there is not some kind of dependency. We can check it during model building.

Overall\_score is one of the most important players characteristics. overall\_score - field\_position dependencies

``` r
data %>% filter(colour == 'gold' & xbox_price != 0 & year_month != '2016-09') %>% 
  group_by(position_group, revision_binary, overall_score) %>%
  summarise(mean_xbox_price = mean(xbox_price)) %>% 
  ggplot(aes(x = overall_score, y = mean_xbox_price, color = position_group)) + 
  facet_grid(revision_binary ~., scale = 'free') + 
  geom_point(size = 2) +
  labs(title = "Mean price and overall score dependency") +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5))
```

<img src=https://github.com/valentinadeshko/Time-series-Fifa2017/blob/master/Fifa_plots/fig-unnamed-chunk-13-1.png>

Using of the general mean price is not the best idea because of daily price changing, but we can see some dependencies between those two variables, so we can try to use these feature in model building.

Basically players on different field positions have different price (e.g. forwards have the highest price) Lets group players by their fiedl position and plot the dependency from price and time

``` r
data %>% filter (colour == 'gold' & xbox_price != 0 & year_month != '2016-09' &   year_month != '2016-10') %>% 
  group_by(position_group, timestamp, revision_binary, position_group) %>%
  summarise(mean_price = mean(xbox_price)) %>%
  ggplot(aes(x = timestamp, y = mean_price, color = position_group)) + 
  facet_grid(revision_binary ~ ., scale = 'free') +
  geom_line(size = 1.2) +
  theme_tq() + 
  theme(axis.text.x = element_text(vjust = 1, hjust = 1)) +
  labs(x = "Date",
       fill = "") +
  labs(title = "Mean daily price of players on different field positions") +
  theme(plot.title = element_text(hjust = 0.5))
```

<img src=https://github.com/valentinadeshko/Time-series-Fifa2017/blob/master/Fifa_plots/fig-unnamed-chunk-14-1.png>

We can see that forwards are the most expensive playeys for both categories. Also analyzing this plot we can see the price spike in the middle of January for all players positions. Let's take a look at top-5 most expensive leagues

``` r
exp <- data %>% filter(colour == 'gold') %>% group_by(league) %>%
   summarise(sum_xbox = sum(xbox_price)) %>%
   arrange(-sum_xbox) %>% slice(1:5)

print(exp)
## # A tibble: 5 x 2
##             league    sum_xbox
##             <fctr>       <dbl>
## 1          Legends 10567225267
## 2 LaLiga Santander  8092011013
## 3   Premier League  6481568458
## 4         Calcio A  2337365020
## 5       Bundesliga  2283993695
```

Interesting how price change in Top-5 leagues. Lets plot their daily mean price changes

``` r
data %>% filter(league == 'LaLiga Santander' | league == 'Legends' | league == 'Premier League' |
                league == 'Calcio A' | league == 'Bundesliga' & xbox_price != 0 & colour == 'gold' 
                & year_month != '2016-09' & year_month != '2016-10') %>% 
  group_by(revision_binary, league, timestamp) %>% 
  summarise(mean_xbox_price = mean(xbox_price)) %>% 
  ggplot(aes(x = timestamp, y = mean_xbox_price, color = league)) + 
  facet_grid(revision_binary ~ ., scale = 'free') + 
  geom_line(size = 0.8) +
  labs(x = "Date",
       fill = "") +
  labs(title = "Mean daily players price of top 5 most expensive leagues") +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5))
```

<img src=https://github.com/valentinadeshko/Time-series-Fifa2017/blob/master/Fifa_plots/fig-unnamed-chunk-16-1.png>

If we do not pay attention to October (when the game has just started) we can see the biggest price spike in LaLiga Santander for special players. Other leagues have small spike of do not have it at all. What has happened in January in Laliga? We can see that in January were added few players with extremely hight price and revision == 'TOTY' (Team of the Year)

``` r
TOTY_players <- stats %>% filter(revision == 'TOTY') %>% 
                select(player_id, name, league, revision)
TOTY_players$xbox_price <- c(8323629, 1615361, 1521657, 1182889, 841543, 673257, 1905613, 4941114, 5973257, 1344389, 546657)

print(TOTY_players)
##    player_id              name           league revision xbox_price
## 1      15953 Cristiano Ronaldo LaLiga Santander     TOTY    8323629
## 2      15985            Modric LaLiga Santander     TOTY    1615361
## 3      15990      Sergio Ramos LaLiga Santander     TOTY    1521657
## 4      15987             Kroos LaLiga Santander     TOTY    1182889
## 5      15993           Marcelo LaLiga Santander     TOTY     841543
## 6      15992        Dani Alves         Calcio A     TOTY     673257
## 7      15989             Neuer       Bundesliga     TOTY    1905613
## 8      15954             Messi LaLiga Santander     TOTY    4941114
## 9      15955           SuГЎrez LaLiga Santander     TOTY    5973257
## 10     15986           Iniesta LaLiga Santander     TOTY    1344389
## 11     15991            PiquГ© LaLiga Santander     TOTY     546657
```

As we can see 9 from 11 players of the year play in LaLiga. And their new January cards explain the price spike in Spanish league. What else happens in January in Fifa game?:)
