library("data.table")
music = fread("/Users/borbe/OneDrive/Bureaublad/tcc_ceds_music.csv")

music
music[, .N, by = list(release_date)]
music[, .N, by = list(genre)]
music[, .N, by = list(topic)]
music[, .N, by = list(artist_name)]
music[, .N, by = list(artist_name)][order(-N)]
artists = music[, .N, by = list(artist_name)][order(-N)]

music[, list(artist_name, track_name, release_date, loudness)][order(-loudness)]
music[artist_name == 'elton john', list(artist_name, track_name, release_date, obscene)]

print(artists, 1:100)

music[, .N, by = list(release_date, genre)][order(release_date)]
jazz_percent = (music[genre == "jazz", .N, by = list(release_date)] / music[, .N, by = list(release_date)]) * 100
print(jazz_percent)

hist(music$valence, breaks = 20, col = 'peachpuff2', xlab = 'test')
hist(music$violence, breaks = 20, col = 'peachpuff2', xlab = 'test')

max(music$violence, na.rm = FALSE)
music[obscene > 0.95, list(release_date, artist_name, track_name, obscene)]

# Making split-up categories for each decade
fiftiesmusic = music[between(release_date, 1950, 1959)]
sixtiesmusic = music[between(release_date, 1960, 1969)]
seventiesmusic = music[between(release_date, 1970, 1979)]
eightiesmusic = music[between(release_date, 1980, 1989)]
ninetiesmusic = music[between(release_date, 1990, 1999)]
zeroesmusic = music[between(release_date, 2000, 2009)]
tensmusic = music[between(release_date, 2010, 2019)]

# Making split-up categories for each genre

jazzmusic = music[genre == 'jazz']
popmusic = music[genre == 'pop']
rockmusic = music[genre == 'rock']
countrymusic = music[genre == 'country']
bluesmusic = music[genre == 'blues']
reggaemusic = music[genre == 'reggae']
hiphopmusic = music[genre == 'hip hop']

#SmoothScatter plots of loudness
smoothScatter(x = music$release_date, y = music$loudness, xlim = c(1950, 2019), ylim = c(0, 1))
smoothScatter(x = popmusic$release_date, y = popmusic$loudness, xlim = c(1950, 2019), ylim = c(0, 1))
smoothScatter(x = jazzmusic$release_date, y = jazzmusic$loudness, xlim = c(1950, 2019), ylim = c(0, 1))
smoothScatter(x = rockmusic$release_date, y = rockmusic$loudness, xlim = c(1950, 2019), ylim = c(0, 1))
smoothScatter(x = countrymusic$release_date, y = countrymusic$loudness, xlim = c(1950, 2019), ylim = c(0, 1))
smoothScatter(x = bluesmusic$release_date, y = bluesmusic$loudness, xlim = c(1950, 2019), ylim = c(0, 1))
smoothScatter(x = reggaemusic$release_date, y = reggaemusic$loudness, xlim = c(1950, 2019), ylim = c(0, 1))
smoothScatter(x = hiphopmusic$release_date, y = hiphopmusic$loudness, xlim = c(1950, 2019), ylim = c(0, 1))

#Sadness x obscene

plot(sadness ~ obscene, data = music, bty = 'l', pch = "▪", col = "lightgrey")
test123 <- lm(sadness ~ obscene, data = music, bty = 'l', pch = "▪", col = "lightgrey")
abline(test123, col=2)


# Creating happinness category
music[, happiness := (1 - sadness)]
music[, list(release_date, artist_name, track_name, sadness, happiness)]

smoothScatter(x = music$release_date, y = music$happiness, colramp = viridisLite::viridis)
summary(music)

plot(music[, mean(happiness), by = list(release_date)], type = 'b,', col = 'tomato2', ylim = c(0.75, 0.95), xlim = c(1950, 1999)) + abline
ggplot(music, aes(release_date, sadness)) + 
         geom_point() +
         geom_smooth()

plot(music[, mean(sadness), by = list(release_date)], type= 'b', col = 'tomato2')


mean_happiness = music[, mean(happiness), by = list(release_date)]
mean_happiness
ggplot(mean_happiness, aes(release_date, V1)) + 
  labs(title = 'Mean Happiness 1950-1999', y = 'Mean Happiness', x = 'Year') + 
  xlim(1950, 1999) + ylim(0.75, 0.95) +
  geom_point() +
  geom_smooth(method = 'gam', col = 'tomato2') + 
  theme_minimal()

# By Genre
mean_jazz_happiness = jazzmusic[, mean(happiness), by = list(release_date)]
mean_country_happiness = countrymusic[, mean(happiness), by = list(release_date)]
mean_pop_happiness = popmusic[, mean(happiness), by = list(release_date)]
mean_rock_happiness = rockmusic[, mean(happiness), by = list(release_date)]
mean_reggae_happiness = reggaemusic[, mean(happiness), by = list(release_date)]
mean_hiphop_happiness = hiphopmusic[, mean(happiness), by = list(release_date)]
mean_blues_happiness = bluesmusic[, mean(happiness), by = list(release_date)]

mhjazz <- ggplot(mean_jazz_happiness, aes(release_date, V1)) + 
  labs(title = 'Mean Happiness in Jazz music, 1950-1999', y = 'Mean Happiness', x = 'Year') + 
  xlim(1950, 1999) + ylim(0.75, 0.95) +
  geom_point() +
  geom_smooth(method = 'gam', col = 'tomato2') + 
  theme_minimal()

mhcountry <- ggplot(mean_country_happiness, aes(release_date, V1)) + 
  labs(title = 'Mean Happiness in Country music, 1950-1999', y = 'Mean Happiness', x = 'Year') + 
  xlim(1950, 1999) + ylim(0.75, 0.95) +
  geom_point() +
  geom_smooth(method = 'gam', col = 'tomato2') + 
  theme_minimal()

mhrock <- ggplot(mean_rock_happiness, aes(release_date, V1)) + 
  labs(title = 'Mean Happiness in Rock music, 1950-1999', y = 'Mean Happiness', x = 'Year') + 
  xlim(1950, 1999) + ylim(0.75, 0.95) +
  geom_point() +
  geom_smooth(method = 'gam', col = 'tomato2') + 
  theme_minimal()

mhpop <- ggplot(mean_pop_happiness, aes(release_date, V1)) + 
  labs(title = 'Mean Happiness in Pop music, 1950-1999', y = 'Mean Happiness', x = 'Year') + 
  xlim(1950, 1999) + ylim(0.75, 0.95) +
  geom_point() +
  geom_smooth(method = 'gam', col = 'tomato2') + 
  theme_minimal()

mhblues <- ggplot(mean_blues_happiness, aes(release_date, V1)) + 
  labs(title = 'Mean Happiness in Blues music, 1950-1999', y = 'Mean Happiness', x = 'Year') + 
  xlim(1950, 1999) + ylim(0.75, 0.95) +
  geom_point() +
  geom_smooth(method = 'gam', col = 'tomato2') + 
  theme_minimal()

mhreggae <- ggplot(mean_reggae_happiness, aes(release_date, V1)) + 
  labs(title = 'Mean Happiness in Reggae music, 1950-1999', y = 'Mean Happiness', x = 'Year') + 
  xlim(1950, 1999) + ylim(0.75, 0.95) +
  geom_point() +
  geom_smooth(method = 'gam', col = 'tomato2') + 
  theme_minimal()

ggplot(mean_hiphop_happiness, aes(release_date, V1)) + 
  labs(title = 'Mean Happiness in Hip Hop music, 1950-1999', y = 'Mean Happiness', x = 'Year') + 
  xlim(1950, 1999) + ylim(0.75, 0.95) +
  geom_point() +
  geom_smooth(method = 'gam', col = 'tomato2') + 
  theme_minimal()

pdf('meanhappinessgenres.pdf', width = 12, height = 8)
mhpop + mhrock + mhcountry + mhjazz + mhblues + mhreggae
dev.off()

# Now for obscenity
mean_jazz_obscene = jazzmusic[, mean(obscene), by = list(release_date)]
mean_country_obscene = countrymusic[, mean(obscene), by = list(release_date)]
mean_pop_obscene = popmusic[, mean(obscene), by = list(release_date)]
mean_rock_obscene = rockmusic[, mean(obscene), by = list(release_date)]
mean_reggae_obscene = reggaemusic[, mean(obscene), by = list(release_date)]
mean_hiphop_obscene = hiphopmusic[, mean(obscene), by = list(release_date)]
mean_blues_obscene = bluesmusic[, mean(obscene), by = list(release_date)]
mean_obscene = music[, mean(obscene), by = list(release_date)]

pdf('meanobscene.pdf', width = 6, height = 6)
ggplot(mean_obscene, aes(release_date, V1)) + 
  labs(title = 'Mean Obscenity in Music, 1950-1999', y = 'Mean Obscenity', x = 'Year') + 
  xlim(1950, 1999) + ylim(0, 0.25) +
  geom_point() +
  geom_smooth(method = 'gam', col = 'tomato2') + 
  theme_minimal()
dev.off()

mopop <- ggplot(mean_pop_obscene, aes(release_date, V1)) + 
  labs(title = 'Mean Obscenity in Pop music, 1950-1999', y = 'Mean Obscenity', x = 'Year') + 
  xlim(1950, 1999) + ylim(0, 0.25) +
  geom_point() +
  geom_smooth(method = 'gam', col = 'tomato2') + 
  theme_minimal()

morock <- ggplot(mean_rock_obscene, aes(release_date, V1)) + 
  labs(title = 'Mean Obscenity in Rock music, 1950-1999', y = 'Mean Obscenity', x = 'Year') + 
  xlim(1950, 1999) + ylim(0, 0.25) +
  geom_point() +
  geom_smooth(method = 'gam', col = 'tomato2') + 
  theme_minimal()

mocountry <- ggplot(mean_country_obscene, aes(release_date, V1)) + 
  labs(title = 'Mean Obscenity in Country music, 1950-1999', y = 'Mean Obscenity', x = 'Year') + 
  xlim(1950, 1999) + ylim(0, 0.25) +
  geom_point() +
  geom_smooth(method = 'gam', col = 'tomato2') + 
  theme_minimal()

mojazz <- ggplot(mean_jazz_obscene, aes(release_date, V1)) + 
  labs(title = 'Mean Obscenity in Jazz music, 1950-1999', y = 'Mean Obscenity', x = 'Year') + 
  xlim(1950, 1999) + ylim(0, 0.25) +
  geom_point() +
  geom_smooth(method = 'gam', col = 'tomato2') + 
  theme_minimal()

moblues <- ggplot(mean_blues_obscene, aes(release_date, V1)) + 
  labs(title = 'Mean Obscenity in Blues music, 1950-1999', y = 'Mean Obscenity', x = 'Year') + 
  xlim(1950, 1999) + ylim(0, 0.25) +
  geom_point() +
  geom_smooth(method = 'gam', col = 'tomato2') + 
  theme_minimal()

moreggae <- ggplot(mean_reggae_obscene, aes(release_date, V1)) + 
  labs(title = 'Mean Obscenity in Reggae music, 1950-1999', y = 'Mean Obscenity', x = 'Year') + 
  xlim(1950, 1999) + ylim(0, 0.25) +
  geom_point() +
  geom_smooth(method = 'gam', col = 'tomato2') + 
  theme_minimal()

pdf('meanobscenitygenres.pdf', width = 12, height = 8)
mopop + morock + mocountry + mojazz + moblues + moreggae
dev.off()

mopop + morock

# Is romance dead?
music[, mean(romantic), by = list(release_date)]
mean_romance = music[, mean(romantic), by = list(release_date)]
mean_romance
ggplot(mean_romance, aes(release_date, V1)) + 
  labs(title = 'Mean Romance 1950-1999', y = 'Mean Romance', x = 'Year') + 
  xlim(1950, 1999) + ylim(0, 0.15) +
  geom_point() +
  geom_smooth(method = 'gam', col = 'tomato2') + 
  theme_minimal()

# Is loud music more violent?
install.packages('patchwork')
library(patchwork)

pdf('loudmusic.pdf', width = 6, height = 6)

ggplot(music ,aes(loudness,violence)) +
  labs(title = 'Loudness and Violence', y = 'Violence', x = 'Loudness') + 
  xlim(0, 1) + ylim(0, 1) +
  geom_point() +
  geom_smooth(method = 'gam') + 
  theme_minimal()

dev.off()

lv50 <- ggplot(fiftiesmusic ,aes(loudness,violence)) +
  labs(title = '1950s', y = 'Violence', x = 'Loudness') + 
  xlim(0, 1) + ylim(0, 1) +
  geom_point() +
  geom_smooth(method = 'gam') + 
  theme_minimal()
lv60 <- ggplot(sixtiesmusic ,aes(loudness,violence)) +
  labs(title = '1960s', y = 'Violence', x = 'Loudness') + 
  xlim(0, 1) + ylim(0, 1) +
  geom_point() +
  geom_smooth(method = 'gam') + 
  theme_minimal()
lv70 <- ggplot(seventiesmusic ,aes(loudness,violence)) +
  labs(title = '1970s', y = 'Violence', x = 'Loudness') + 
  xlim(0, 1) + ylim(0, 1) +
  geom_point() +
  geom_smooth(method = 'gam') + 
  theme_minimal()
lv80 <- ggplot(eightiesmusic ,aes(loudness,violence)) +
  labs(title = '1980s', y = 'Violence', x = 'Loudness') + 
  xlim(0, 1) + ylim(0, 1) +
  geom_point() +
  geom_smooth(method = 'gam') + 
  theme_minimal()
lv90 <- ggplot(ninetiesmusic ,aes(loudness,violence)) +
  labs(title = '1990s', y = 'Violence', x = 'Loudness') + 
  xlim(0, 1) + ylim(0, 1) +
  geom_point() +
  geom_smooth(method = 'gam') + 
  theme_minimal()
lv00 <- ggplot(zeroesmusic ,aes(loudness,violence)) +
  labs(title = '2000s', y = 'Violence', x = 'Loudness') + 
  xlim(0, 1) + ylim(0, 1) +
  geom_point() +
  geom_smooth(method = 'gam') + 
  theme_minimal()

pdf('loudmusicdecades.pdf', width = 12, height = 8)

dev.off()

# Making stack charts
music_summary <- music[, .N, by = .(release_date, genre)]

pdf('StackChart.pdf', width = 10, height = 6)

ggplot(music_summary, aes(x = release_date, y = N, fill = genre)) +
  geom_area(alpha = 0.8) +  
  labs(title = "Stacked Area Chart of Songs per Year by Genre",
       x = "Year",
       y = "Number of Songs",
       fill = "Genre") +
  theme_minimal()

dev.off()

#100% stack chart

pdf('100StackChart.pdf', width = 10, height = 6)

music_summary[, total_per_year := sum(N), by = release_date]
music_summary[, pct := (N / total_per_year) * 100]
ggplot(music_summary, aes(x = release_date, y = pct, fill = genre)) +
  geom_area(alpha = 0.8) +  
  labs(title = "100% Stacked Area Chart of Songs per Year by Genre",
       x = "Year",
       y = "Percentage of Songs (%)",
       fill = "Genre") +
  theme_minimal()

dev.off()

library("ggplot2")
library(ggplot2)
library(dplyr)
library(tidyverse)

hist(music$valence)

music[genre, by = list(release_date)]

# Making a database for music (with data) that has been nominated for a Grammy

library("data.table")
music = fread("/Users/borbe/OneDrive/Bureaublad/tcc_ceds_music.csv")
grammy = fread("/Users/borbe/OneDrive/Bureaublad/the_grammy_awards.csv")
setnames(grammy, old = "artist", new = "artist_name")
setnames(grammy, old = "nominee", new = "track_name")

library(dplyr)

grammy2 <- grammy %>%
  mutate(artist_name = tolower(artist_name))
grammy2 <- grammy2 %>%
  mutate(track_name = tolower(track_name))

grammus <- merge(music, grammy2, by=c("artist_name", "track_name"), all=FALSE)
grammusfil <- unique(grammus, by = "lyrics")
print(grammusfil)

mean(grammusfil$sadness)
summary(grammusfil)
summary(music)

# Making boxplots
library(tidyverse)
install.packages('hrbrthemes')
library(hrbrthemes)
install.packages('viridis')
library(viridis)
install.packages('ggstatsplot')
library(ggstatsplot)
library(palmerpenguins)
library(tidyverse)


data.frame(music)

pdf('violinchart1.pdf', width = 8, height = 8)

ggplot(music, aes(x = genre, y = energy, fill = genre)) + 
  geom_violin()

dev.off()

ggplot(grammusfil, aes(x = genre, y = valence, fill = genre)) + 
  geom_boxplot()

mean(music$valence)
mean(grammusfil$valence)

ggplot(music, aes(x=genre, y=obscene, fill=track_name)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Basic boxplot") +
  xlab("")


# Making a database for music (with data) that has charted on the Billboard Chart

library("data.table")
music = fread("/Users/borbe/OneDrive/Bureaublad/tcc_ceds_music.csv")
billboard = fread("/Users/borbe/OneDrive/Bureaublad/charts.csv")
setnames(billboard, old = "artist", new = "artist_name")
setnames(billboard, old = "song", new = "track_name")

library(dplyr)

billboard2 <- billboard %>%
  mutate(artist_name = tolower(artist_name))
billboard2 <- billboard2 %>%
  mutate(track_name = tolower(track_name))

chartmusic <- merge(music, billboard2, by=c("artist_name", "track_name"), all=FALSE)
chartmusicfil <- unique(chartmusic, by = "lyrics")
print(chartmusicfil)

summary(music)
summary(chartmusicfil)

mean(music$danceability)
mean(chartmusicfil$danceability)
mean(grammusfil$danceability)

# Box plots


# Radar chart
install.packages('fmsb')
library(fmsb)
library(tidyverse)

mus_sum <-music %>% summarise(mean_valence = mean(valence), mean_loudness = mean(loudness), mean_sadness = mean(sadness), mean_energy = mean(energy), mean_obscene = mean(obscene), mean_acousticness = mean(acousticness))%>% tibble()
cha_sum <- chartmusicfil %>% summarise(mean_valence = mean(valence), mean_loudness = mean(loudness), mean_sadness = mean(sadness), mean_energy = mean(energy), mean_obscene = mean(obscene), mean_acousticness = mean(acousticness))%>% tibble()
gra_sum <- grammusfil %>% summarise(mean_valence = mean(valence), mean_loudness = mean(loudness), mean_sadness = mean(sadness), mean_energy = mean(energy), mean_obscene = mean(obscene), mean_acousticness = mean(acousticness))%>% tibble()

mcg_sum <- rbind(mus_sum, cha_sum, gra_sum)
mcg_sum <- data.frame(mcg_sum, row.names = c("All Music", "Chart Music", "Grammy Music"))
mcg_sum

maxminmcg <- data.frame(
  mean_valence = c(0.75, 0), mean_loudness = c(0.75, 0), mean_sadness = c(0.75, 0),
  mean_energy = c(0.75, 0), mean_obscene = c(0.75, 0), mean_acousticness = c(0.75, 0)
)
rownames(maxminmcg) <- c("Max", "Min")

raddata <- rbind(maxminmcg, mcg_sum)
raddata

radarchart(raddata)

radarchart(
  raddata,
  # Customize the polygon
  pcol = 'tomato2', pfcol = scales::alpha('tomato2', 0.25), plwd = 2, plty = 1,
  # Customize the grid
  cglcol = "grey", cglty = 1, cglwd = 0.8,
  # Customize the axis
  axislabcol = "darkgrey", 
  # Variable labels
  title = "Radar chart", vlcex = 0.75
)
#Not really worth it

plot(loudness ~ release_date, data = chartmusicfil)

ggplot(countrymusic, aes(x = release_date, y = feelings)) + 
  geom_line(stat = "summary", fun = "mean", colour = "navyblue", ) +
  xlim(1950, 1999) + ylim(0, 1) + 
  coord_cartesian(ylim=c(0, 0.95)) +
  theme_bw() + labs(x = "Year", y = "Sadness", title = "Sadness in Country music, 1950-1999") + 
  annotate("rect", xmin = c(1950, 1965, 1990), xmax = c(1953, 1973, 1991), ymin = -Inf, ymax = Inf, alpha = .25, fill = c("tomato", "tomato", "tomato"))




geom_rect(aes(xmin=1964, xmax=1973, ymin=0, ymax=1))

topplot + 
  rect(xleft = 1950, xright = 1953, ybottom = 0, ytop = 1, 
       border = NA, col = adjustcolor("tomato", alpha = 0.3)) +
  rect(xleft = 1965, xright = 1973, ybottom = 0, ytop = 1, 
       border = NA, col = adjustcolor("tomato", alpha = 0.3)) +
  rect(xleft = 1990, xright = 1991, ybottom = 0, ytop = 1, 
       border = NA, col = adjustcolor("tomato", alpha = 0.3))

