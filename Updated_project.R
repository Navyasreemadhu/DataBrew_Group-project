Abstract

Introduction

The chosen data-set for this project is “The Great American Coffee Taste Test” from TidyTuesday (Week 21, 2024). In October 2023, “world champion barista” James Hoffmann and coffee company Cometeer held the “Great American Coffee Taste Test” on YouTube, during which viewers were asked to fill out a survey about 4 coffees they ordered from Cometeer for the tasting. Based on the research conducted by the NIH: National Institutes of Health, which found that 75% of U.S. adults over the age of 20 drink coffee, with 49% drinking it daily, coffee consumption is clearly a significant part of American culture and daily life. This widespread prevalence of coffee consumption makes it an ideal subject for data analysis, as it offers insights into behavior patterns that affect a large portion of the population.

Understanding the coffee consumption habits of individuals offers valuable insights into consumer behavior, lifestyle preferences, and spending patterns. Coffee is an integral part of daily life for many people around the world, and studying why people choose to drink coffee, their preferred roast, where they typically consume it, and how much they spend can reveal a lot about personal tastes, habits, and economic factors. These factors could also provide valuable information for businesses in the coffee industry, helping them tailor their products and marketing strategies more effectively. People from different age groups or genders may gravitate toward specific coffee types (e.g., espresso, cappuccino, black coffee) and may prefer different levels of customization when it comes to additives. By exploring how age and gender impact these preferences, we can gain insight into how demographic characteristics shape coffee consumption patterns.  

Question 1: Why do people choose to drink coffee, what roast preference are common, where do they typically drink it, and how much do they spend on coffee overall? 
  
  Introduction 

We are interested in this question because analyzing coffee consumption habits provides valuable insights into consumer behavior, lifestyle choices, and spending trends. Coffee plays a significant role in the daily routines of people worldwide, and examining why individuals drink coffee, their preferred roast type, typical consumption locations, and spending amounts can shed light on personal preferences, habits, and economic influences. These insights could also be beneficial for businesses in the coffee industry, enabling them to better tailor their products and marketing strategies.

To explore this question, the essential attributes in the coffee dataset—total_spend, why_drink, where_drink, and roast_level—will offer a comprehensive view of coffee consumption patterns, revealing motivations, preferences, spending behavior, and lifestyle tendencies. We plan to use bar plots and box plots to visualize categorical data, such as reasons for drinking coffee (why_drink) and coffee roast preferences (roast_level), as well as to display the spending distribution by location (where_drink).

Approach

In order to answer our first question we choose a bar plot and heatmap.

The first part of question 1 focuses on understanding why people choose to drink coffee and identifying common roast preferences. In this study, we aim to illustrate relationships between multiple variables in a single plot. We selected a bar plot for this purpose due to its simplicity, flexibility, and ability to clearly highlight differences across categories. A bar plot offers a straightforward yet powerful way to present complex information in an organized and easily understandable format.

To address the second part of question 1, we used a heatmap. A heatmap is a valuable data visualization tool that employs color gradients to depict the intensity or magnitude of values within a dataset. This visual approach makes it easy to recognize complex data patterns and identify areas of high or low concentration. Here, we aim to discover the most popular locations for coffee consumption and how much people typically spend at each location.

Analysis

Question No.1- Part 1

Code 

{r}
# Remove rows with NA values in the important columns

coffee_sankey_clean <- na.omit(coffee_survey[, c("age", "gender", "favorite", "additions")])



coffee_Q1 <- coffee_survey[, c("why_drink", "roast_level", "total_spend", "where_drink")]


coffee_Q1 <- coffee_survey[, c("why_drink", "roast_level", "total_spend", "where_drink")]



na.omit(coffee_Q1)



coffee_Q1 <- na.omit(coffee_Q1[, c("why_drink", "roast_level", "total_spend", "where_drink")])



library(dplyr)

library(tidyr)

library(ggplot2)

# Calculate total percentages for each reason
reason_counts <- coffee_Q1 %>%
  
  mutate(
    tastesGood = grepl("It tastes good", why_drink),
    ritual = grepl("I need the ritual", why_drink),
    caffeine = grepl("I need the caffeine", why_drink),
    toPoop = grepl("It makes me go to the bathroom", why_drink)
  ) %>%
  pivot_longer(cols = tastesGood :toPoop, names_to = "reason", values_to = "selected") %>%
  filter(selected) %>%
  count(reason) %>%
  mutate(percentage = (n / nrow(coffee_Q1)) * 100)


# Calculate percentages for each roast level within each reason

roast_level_counts <- coffee_Q1 %>%
  
  mutate(
    tastesGood = grepl("It tastes good", why_drink),
    ritual = grepl("I need the ritual", why_drink),
    caffeine = grepl("I need the caffeine", why_drink),
    toPoop = grepl("It makes me go to the bathroom", why_drink)
  ) %>%
  pivot_longer(cols = tastesGood:toPoop, names_to = "reason", values_to = "selected") %>%
  filter(selected) %>%
  filter(roast_level %in% c("Light", "Medium", "Dark")) %>%
  count(reason, roast_level) %>%
  group_by(reason) %>%
  mutate(percentage = (n / sum(n)) * 100)

# Define roast colors

roast_colors <- c("Light" = "wheat", "Medium" = "#C4A484", "Dark" = "chocolate4")

# Reorder roast_level to ensure proper order in bars and legend
roast_level_counts$roast_level <- factor(roast_level_counts$roast_level, levels = c("Light", "Medium", "Dark"))

# Plot original bars for reasons

p1 <- ggplot(reason_counts, aes(x = reorder(reason, -percentage), y = percentage)) +
  geom_bar(stat = "identity", fill = "#FAF0E6", position = position_dodge(width = 0.8), width = 0.7) +
  labs(title = "Percentage of Reasons for Drinking Coffee (with Roast Level Breakdown)",
       x = "Reason",
       y = "Percentage (%)") +
  scale_y_continuous(breaks = seq(0, 100, 10), labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  coord_flip()

# Add the roast level bars just above the original bars

p2 <- ggplot(roast_level_counts, aes(x = reorder(reason, +percentage), y = percentage, fill = roast_colors)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 1) +
  scale_fill_manual(values = roast_colors, breaks = c("Light", "Medium", "Dark")) +
  theme_minimal() +
  coord_flip() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Combine the two plots by overlaying them

p1 + geom_bar(data = roast_level_counts, aes(y = percentage, fill = roast_level),
              stat = "identity", position = position_dodge(width = 0.4), width = 0.4) + 
  theme_minimal() +
  scale_fill_manual(values = roast_colors, breaks = c("Light", "Medium", "Dark")) +
  theme(legend.position = "bottom")

Part-2

Code


coffee_survey <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-14/coffee_survey.csv')


coffee_survey <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-14/coffee_survey.csv')

head(coffee_survey)


coffee_Q1 <- na.omit(coffee_survey[, c("why_drink", "roast_level", "total_spend", "where_drink")])



coffee_Q1$roast_level[coffee_Q1$roast_level == "Blonde"] <- NA

coffee_Q1$roast_level[coffee_Q1$roast_level == "Italian"] <- NA

coffee_Q1$roast_level[coffee_Q1$roast_level == "Nordic"] <- NA

coffee_Q1$roast_level[coffee_Q1$roast_level == "French"] <- NA

coffee_Q1 <- coffee_Q1[!is.na(coffee_Q1$roast_level), ]

coffee_Q1



library(tidyr)

# Separate each reason into individual rows

Q1_separated <- coffee_Q1 %>%
  
  separate_rows(why_drink, sep = ",\\s*") %>%
  
  separate_rows(where_drink, sep = ",\\s*")

Q1_separated


# Count occurrences of each total_spend range per location

data_Q1_part2 <- Q1_separated %>%
  
  count(where_drink, total_spend)


library(dplyr)

# Remove "None of these" from the where_drink column

data_Q1_part3 <- data_Q1_part2 %>%
  
  filter(where_drink != "None of these")


a <- ggplot(data_Q1_part3, aes(x = where_drink, y = total_spend, fill = n)) +
  
  geom_tile(color = "white") +
  
  geom_text(aes(label = n), color = "black") +
  
  scale_fill_gradient(low = "lightyellow", high = "darkred", name = "Count") +
  
  labs(title = "Distribution of Spending by Location",
       
       x = "Where Drink",
       
       y = "Total Spend Range") +
  
  theme_minimal() +
  
  theme(panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank())

a

#ggsave("heatmap.png",plot = a, width = 10, height = 6, dpi = 300)

Discussion

We can effectively address our first question—why people consume coffee and which roast preferences are the most popular—using a bar plot. The "why_drink" column consists of four categories: taste good, caffeine, ritual, and to poop. More than 35% of respondents enjoy light or medium coffee, while only about 15% prefer dark coffee. The data visualization analysis indicates that a significant 94% of people drink coffee primarily for its taste. Within this group, 50% prefer light roast, and 40% opt for medium roast, with just 10% choosing dark coffee. This trend suggests that light or medium roasts are favored over dark coffee. Additionally, around 53-56% of people consume coffee as a ritual or for the caffeine boost, with these preferences also leaning towards light or medium flavors. Interestingly, only 13% drink coffee for the purpose of aiding digestion.

In our next analysis, we focus on where people typically enjoy their coffee and how much they spend. To explore this, we created a heatmap comparing "where_drink" with total spending ranges. The heatmap reveals that individuals prefer to have their coffee at home, more so than at the office, cafes, or on the go. The results also show that those who drink coffee at home generally spend between $20 and $40. Conversely, the most popular choice for coffee consumption is while on the go, with people willing to spend between $20 and $80 for their coffee.





Question No.2: How do age and gender impact preferences for various types of coffee and the use of additives? 
  
Introduction

We are particularly interested in this question because age and gender are significant demographic factors that often affect consumer behavior, especially regarding preferences for various types of coffee and the use of additives such as sugar, milk, and cream. Individuals from different age groups or genders may favor specific coffee types (like espresso, cappuccino, or black coffee) and may have varying preferences for customization regarding additives. By investigating how age and gender influence these preferences, we can gain valuable insights into how demographic characteristics shape coffee consumption patterns.

To tackle this question, the key components of the dataset include four columns: ‘favorite,’ ‘age,’ ‘gender,’ and ‘additions.’ These elements will facilitate a comprehensive analysis of how coffee preferences (reflected in the “favorite” variable) differ across various age groups and between genders, helping to reveal meaningful patterns in consumer behavior.

Approach

We approach the second question with a circular bar plot and a bubble plot.

A circular bar plot is a variation of the traditional bar plot where bars are arranged in a circular layout, making it useful for representing cyclical or categorical data. This format allows for an aesthetically engaging display of information, especially when comparing values across multiple categories or groups. This plot is best suited for data that can be clearly understood in a circular format and may not be ideal for precise value comparisons.

bubbleplot

Analysis

Code


age_palette <- c("#3B2A1E", "#E3B89A", "#D9C9B0", "#BFA7A1", "#D2B48C", "#A57B52", "#8B4513", "#4B3D33")


Q2_separated <- coffee_sankey_clean %>%
  separate_rows(additions, sep = ",\\s*")

Q2_separated



gender_counts <- sort(table(Q2_separated$gender), decreasing = TRUE)
print(gender_counts)



Q2_separated$additions[Q2_separated$additions == "Sugar or sweetner"] <- NA
Q2_separated$additions[Q2_separated$additions == "Flavor syrup"] <- NA


Q2_separated <- Q2_separated[!is.na(Q2_separated$additions), ]
Q2_separated



Q2_separated$gender[Q2_separated$gender == "Prefer not to say"] <- NA
Q2_separated$gender[Q2_separated$gender == "Other (please specify)"] <- NA

Q2_separated <- Q2_separated[!is.na(Q2_separated$gender), ]
Q2_separated




Q2_aggregated <- Q2_separated %>%
  group_by(age, gender, favorite, additions) %>%
  summarize(count = n()) %>%  # Count the number of occurrences
  ungroup()



Q2_aggregated <- Q2_aggregated %>%
  group_by(favorite) %>%
  mutate(proportion = count / sum(count) * 100) %>%  # Convert to percentage if needed
  ungroup()



# Circular bar plot with fill by age

ggplot(Q2_aggregated, aes(x = favorite, y = proportion, fill = age)) +
  
  geom_bar(stat = "identity", position = "stack", width = 0.9) +
  
  coord_polar(start = 0) +
  
  scale_fill_manual(values = age_palette, name = "Age") +
  
  labs(title = "Preferences for Coffee type by age",
       
       x = "", y = "") +
  
  theme_minimal() +
  
  theme(
    
    axis.text.x = element_text(size = 8, color = "black", face = "bold"),
    
    axis.title = element_blank(),
    
    panel.grid = element_blank(),
    
    legend.position = "right"
    
  )





q <- ggplot(Q2_aggregated, aes(x = age, y = additions, size = count, color = gender)) +
  
  geom_point(alpha = 1.5) +
  
  scale_color_manual(values = c("Female" = "darkred", "Male" = "#D2B48C", "Non-binary" = "black"))+
  
  scale_size_continuous(range = c(5, 20)) +
  
  labs(title = "Bubble Plot of Age, Addition, and Gender",
       
       x = "Age Group",
       
       y = "Additives",
       
       size = "Count")+
  
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        
        axis.text.x = element_text(size = 5, face = "bold"),
        
        legend.text = element_text(size = 10),  # Font size for legend text
        
        legend.title = element_text(size = 12)) # Font size for legend title)

q

Discussion

The circular bar plot illustrates coffee preferences across various age groups, with each type of coffee positioned along the perimeter and bars extending outward to indicate levels of preference. Darker shades represent older age groups, while lighter shades represent younger ones. The plot reveals that regular drip coffee is the most favored across all ages, particularly among older individuals aged 55-64. On the other hand, blended drinks, like Frappuccino, appear to be more popular among younger groups, especially those under 18. Espresso, iced coffee, and latte show balanced appeal across a wide age range, while cold brew and Americano are less preferred, with modest popularity in the middle-aged demographic. This visualization offers a clear depiction of how coffee choices shift with age, identifying both widely popular and more niche preferences.

The bubble plot provides an analysis of coffee additive preferences across different age groups and genders. Larger bubbles indicate higher counts of individuals who prefer a particular additive. The most popular additive choices include milk and black coffee, especially among males (shown in light tan) and non-binary individuals (in blue) within the 25–34 and 35–44 age groups. Sugar or sweeteners and dairy alternatives are less commonly preferred, with a few notable cases among younger and older age groups. Females (represented by red) show more diverse preferences across additives but with generally lower counts. This visualization highlights age and gender trends in coffee customization preferences, particularly the tendency of younger adults to prefer black coffee and milk as additives.

The bubble plot provides an analysis of coffee additive preferences across different age groups and genders. Larger bubbles indicate higher counts of individuals who prefer a particular additive. The most popular additive choices include milk and black coffee, especially among males (shown in light tan) and non-binary individuals (in blue) within the 25–34 and 35–44 age groups. Sugar or sweeteners and dairy alternatives are less commonly preferred, with a few notable cases among younger and older age groups. Females (represented by red) show more diverse preferences across additives but with generally lower counts. This visualization highlights age and gender trends in coffee customization preferences, particularly the tendency of younger adults to prefer black coffee and milk as additives.Introduction

The chosen data-set for this project is “The Great American Coffee Taste Test” from TidyTuesday (Week 21, 2024). In October 2023, “world champion barista” James Hoffmann and coffee company Cometeer held the “Great American Coffee Taste Test” on YouTube, during which viewers were asked to fill out a survey about 4 coffees they ordered from Cometeer for the tasting. Based on the research conducted by the NIH: National Institutes of Health, which found that 75% of U.S. adults over the age of 20 drink coffee, with 49% drinking it daily, coffee consumption is clearly a significant part of American culture and daily life. This widespread prevalence of coffee consumption makes it an ideal subject for data analysis, as it offers insights into behavior patterns that affect a large portion of the population.

Understanding the coffee consumption habits of individuals offers valuable insights into consumer behavior, lifestyle preferences, and spending patterns. Coffee is an integral part of daily life for many people around the world, and studying why people choose to drink coffee, their preferred roast, where they typically consume it, and how much they spend can reveal a lot about personal tastes, habits, and economic factors. These factors could also provide valuable information for businesses in the coffee industry, helping them tailor their products and marketing strategies more effectively. People from different age groups or genders may gravitate toward specific coffee types (e.g., espresso, cappuccino, black coffee) and may prefer different levels of customization when it comes to additives. By exploring how age and gender impact these preferences, we can gain insight into how demographic characteristics shape coffee consumption patterns.  