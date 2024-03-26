library(tidyverse)


#
#
# 1
#
#

df <- read.csv("../../Data_Course_CHRISTENSEN/GEOG_490R/task_6/UTSNTL_ALL_2020_2022_Daily_Wide.csv")

df$Date <- as.Date(df$Date, tryFormats = "%m/%d/%y")


df$freezing <-ifelse(df$Agua.Canyon..907..Air.Temperature.Average..degF. < 32,"Yes","No")

df %>%
  ggplot(aes(x = Date, y = Agua.Canyon..907..Snow.Water.Equivalent..in..Start.of.Day.Values)) +
  geom_ribbon(aes(ymin = 0, ymax = Agua.Canyon..907..Snow.Water.Equivalent..in..Start.of.Day.Values), alpha = 0.75, fill = "lightblue") +
  geom_line(aes(y = Agua.Canyon..907..Air.Temperature.Average..degF./3, color = freezing, group = 1)) +
  scale_y_continuous(name = "SWE (inches)", sec.axis = sec_axis(trans = ~.*3, name = "Temperature (degF)")) +
  theme_bw() +
  theme(axis.title.y.right = element_text(color = "firebrick"),
        axis.text.y.right = element_text(color = "firebrick")) +
  labs(title = "Agua Canyon Snotel 2020-2023")



#
#
# 2
#
#
df <- read.csv("../../Data_Course_CHRISTENSEN/GEOG_490R/task_6/UTSNTL_ALL_2020_2022_Daily_Wide.csv")

df$Date <- as.Date(df$Date, tryFormats = "%m/%d/%y")

df_long <- df %>% pivot_longer(2:length(df),
                    names_to = "Category",
                    values_to = "Value")


#Regular expression to extract the numbers inside of the categories
length(unique(gsub(".*\\.\\.([0-9]+)\\.\\..*", "\\1", df_long$Category)))

#There are 137 different sites with 6570 observations each
table(gsub(".*\\.\\.([0-9]+)\\.\\..*", "\\1", df_long$Category))

# Just checking!
900090/137


#
#
# 3
#
#

#creating new column that specifies each year winter season
df_long$season <- cut(df_long$Date, breaks =  c(as.Date(c("2020-10-01", "2021-10-01", "2022-10-01")), Inf), labels = paste0("season_", c(1, 2, 3)))

# code that outputs 15 lines, 5 lines for each season of the locations with the most snow depth
deepest_snow <- df_long %>%
  filter(grepl("Snow.Depth", Category)) %>%
  filter(grepl("season_1", season)) %>% 
  arrange(desc(Value)) %>%
  distinct(Category, .keep_all = TRUE) %>%
  slice_head(n = 5) %>% 
union(
  df_long %>%
    filter(grepl("Snow.Depth", Category)) %>%
    filter(grepl("season_2", season)) %>% 
    arrange(desc(Value)) %>%
    distinct(Category, .keep_all = TRUE) %>%
    slice_head(n = 5)
) %>% 
  union(
    df_long %>%
      filter(grepl("Snow.Depth", Category)) %>%
      filter(grepl("season_3", season)) %>% 
      arrange(desc(Value)) %>%
      distinct(Category, .keep_all = TRUE) %>%
      slice_head(n = 5)
  )

deepest_snow

#
#
# 4
#
#


deepest_snow %>%
  mutate(Category = str_extract(Category, "^[^0-9]+") %>% str_replace_all("\\.", "")) %>% 
  ggplot(aes(x = Category)) +
  geom_histogram(stat = "count", fill = "skyblue", color = "black") +
  labs(title = "Each time a location was in the top 5 for snow depth per season",
       x = "",
       y = "Count") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#
#
# 5
#
#

#making column that specifies the type of data collection
df_long$data <- str_extract(df_long$Category, "\\d+\\.\\.(.*)")

# Remove numbers and dots
df_long$data <- str_replace_all(df_long$data, "\\d+\\.", "")

# Remove all dot at the beginning
df_long$data <- str_replace(df_long$data, "^\\.*", "")

# Remove dot at the end
df_long$data <- str_replace(df_long$data, "\\.$", "")

#turning all . into _
df_long$data <- gsub("\\.","_",df_long$data)


#keeping everything before the numbers in the category column
df_long <- df_long %>% 
  mutate(location = str_extract(Category, "^[^0-9]+"))

#removing trailing dots in the Category column
df_long$location <- gsub("\\.+$", "", df_long$location)

#changing middle . to _
df_long$location <- gsub("\\.", "_", df_long$location)

#removing "category" column
df_long <- df_long %>% select(-Category)


#reading in elevation data
elev_df <- read.csv("../GEOG_490R/task_6/UTSNTL_ELEV.csv")

#turning all empty spaces to underscores
elev_df$names <- gsub(" ","_",elev_df$names)

unique(df_long$location)
unique(elev_df$names)

#trying out the anti_join function
different_rows <- anti_join(df_long, elev_df, by = c("location" = "names"))
unique(different_rows$location)
#This has 14 variable and the full join has 151. This makes sense as there was only 137 different locations in the df_long

#full joining the elevation to the long dataframe
full_df <- full_join(df_long, elev_df, by = c("location" = "names"))

unique(full_df$location)

#showing that most of the locations now have an elevation
full_df %>% 
  distinct(location, .keep_all = TRUE) %>% 
  select(location,elevation_ft) %>% 
  print(n=151)

unique(full_df$Date)

swe_2021_april <- full_df %>% 
  filter(Date >= "2021-04-01" & Date < "2021-05-01") %>% 
  filter(str_detect(data, "Snow_Water_Equi")) %>% 
  arrange(desc(Value)) %>%
  distinct(location, .keep_all = TRUE)

full_df %>% 
  filter(Date >= "2021-04-01" & Date < "2021-05-01") %>% 
  filter(str_detect(data, "Snow_Water_Equi")) %>% 
  ggplot(aes(x=elevation_ft,y=Value)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

full_df %>% 
  filter(Date >= "2022-04-01" & Date < "2022-05-01") %>% 
  filter(str_detect(data, "Snow_Water_Equi")) %>% 
  ggplot(aes(x=elevation_ft,y=Value)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

full_df %>% 
  filter(Date >= "2023-04-01" & Date < "2023-05-01") %>% 
  filter(str_detect(data, "Snow_Water_Equi")) %>% 
  ggplot(aes(x=elevation_ft,y=Value)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

#
#
# 6
#
#

swe_2021_april <- full_df %>% 
  filter(Date >= "2021-04-01" & Date < "2021-05-01") %>% 
  filter(str_detect(data, "Snow_Water_Equi"))

swe_2022_april <- full_df %>% 
  filter(Date >= "2022-04-01" & Date < "2022-05-01") %>% 
  filter(str_detect(data, "Snow_Water_Equi"))

swe_2023_april <- full_df %>% 
  filter(Date >= "2023-04-01" & Date < "2023-05-01") %>% 
  filter(str_detect(data, "Snow_Water_Equi"))

summary(lm(Value~elevation_ft,swe_2021_april))
summary(lm(Value~elevation_ft,swe_2022_april))
summary(lm(Value~elevation_ft,swe_2023_april))


