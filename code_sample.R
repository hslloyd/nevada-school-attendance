
# Install the required packages if not already installed. Change FALSE to TRUE if you need to install these packages.
if (!require('tidyverse')) install.packages('tm', dependencies=FALSE)
if (!require('readr')) install.packages('tidytext', dependencies=FALSE)

library(tidyverse)
library(readr) 

#### GLobal Variables and Vectors

# These are the variables related to race in these datasets
race_variables <- c("American Indian/Alaskan Native", "Asian", "Black", "Hispanic", "Two or More races","Two or More Races", "Pacific Islander", "White") 

#define the color palette
color_palette <- c("#9e0142", "#f46d43", "#fdae61", "#fee08b",  "#66c2a5", "#3288bd", "#5e4fa2")


# Read data from GitHub URLs 
# This csv contains all of the reported demographics of all of the students at the school district level in Nevada
demo_all <- read_csv(url("https://raw.githubusercontent.com/hslloyd/nevada-school-attendance/main/csvs/demo_all_students.csv"))

# This csv contains all of the reported demographics of the chronically absent students at the school district level in Nevada
demo_chronic <- read_csv(url("https://raw.githubusercontent.com/hslloyd/nevada-school-attendance/main/csvs/demo_ca_students.csv" ))

# Clean variable names to be able to merge the dataframes
names(demo_chronic) <- sub("Chronicabsenteeism - |Chronic Absenteeism  - |Chronic Absenteeism - ","", names(demo_chronic))
names(demo_all) <- sub(" % ","", names(demo_chronic))

# Convert character values to numeric
demo_all <- demo_all %>%
  mutate_at(vars(starts_with(colnames(demo_all)[5:length(colnames(demo_all))])), as.numeric)

demo_chronic <- demo_chronic %>%
  mutate_at(vars(starts_with(colnames(demo_chronic)[5:length(colnames(demo_chronic))])), as.numeric)

# Pivot data to long format for both dataframes
demo_all <- demo_all %>%
  pivot_longer(cols = c(starts_with(colnames(demo_all)[6:length(colnames(demo_all))])), names_to = 'demographic', values_to = 'percentage') %>%
  mutate(chronic_absence = "n")

demo_chronic <- demo_chronic %>%
  pivot_longer(cols = c(starts_with(colnames(demo_chronic)[6:length(colnames(demo_chronic))])), names_to = 'demographic', values_to = 'percentage') %>%
  mutate(chronic_absence = "y")

# Combine dataframes to one long
demo_combined <- rbind(demo_chronic, demo_all)

# Replace NAs with 0s 
demo_combined <- demo_combined %>% 
  mutate(percentage = replace(percentage, is.na(percentage), 0)) %>% 
  filter(`Accountability Year`=='2022-2023')

# create new variable `percentage_norm` since the variable `percentage` since the sum of percentage is not always 100
demo_combined <- demo_combined %>%
  mutate(demographics_type = case_when(
    demographic %in% race_variables ~ "race",
    TRUE ~ "other"
  )) %>%
  group_by(Name, demographics_type, chronic_absence) %>%
  mutate(sum = sum(percentage),
         percentage_norm = percentage/sum * 100) %>%
  mutate(sum_2 = sum(percentage_norm), difference = abs(percentage_norm - percentage)) %>%
  ungroup()


### Data Exploration and Visualizations
# How does the chronic absence distribution and the entire distribution of students differ vary based on demographics?
demo_combined%>%filter(demographics_type == "race")%>%
  ggplot( aes(x=percentage_norm, fill=chronic_absence)) +
  geom_density(alpha=0.4) +
  facet_wrap(~demographics_type, scales='free') +  # Facet by demographic type
  ggtitle('Density of student percentage by demographic and student sample') +
  xlab('percentage of student population') +
  ylab('Density') +
  theme_minimal()+
  facet_wrap(~demographic)+
  labs(fill = "Student Population") +
  scale_fill_manual(
    values = c("y" = "#d8b365", "n" = "#5ab4ac"),  # Set custom colors
    breaks = c("y", "n"),  # Specify breaks
    labels = c( "Chronically Absent", "Entire")  # Specify labels
  )

# What is the normalized proportion distribution of demographic groups among students with chronic absence at the school district level, as depicted in the normalized stacked bar chart?
# Plot demographics of students with chronic absence
demo_combined %>%
  filter(chronic_absence == "y" & demographics_type == "race")%>%
  ggplot(aes(x = Name, y = percentage_norm, fill = demographic)) +
  geom_bar(stat = "identity", position = "fill") +
  labs( title = "",
        subtitle = "Normalized demographic distribution among chronically absent students in nevada",
        x = "School District",
        y = "Proportion") + 
  scale_fill_manual(values = color_palette) +  # Set color palette
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#  What is the normalized proportion distribution of demographic groups among all students in Nevada at the school district level?
# Plot demographics for all students in each county
demo_combined %>%
  filter(chronic_absence == "n")%>%
  ggplot(aes(x = Name, y = percentage_norm, fill = demographic)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "",
       subtitle = "Normalized demographic distribution among all students in Nevada",
       x = "School District",
       y = "Proportion") + 
  scale_fill_manual(values = color_palette) +  # Set color palette
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Statistical Tests 
# create contingency table for Caron City
carson_city_cont <- demo_combined %>%
  filter(Name == "Carson City" & demographics_type == "race") %>%
  select(-c(`Accountability Year`, `Organization Code`, `District/Authority Name`, `All Students`, demographics_type, Name, percentage, sum, sum_2, difference)) %>%
  spread(demographic, percentage_norm) %>%
  distinct()

# Perform chi-square test
chisq_result_carson <- chisq.test(carson_city_cont[-1])

# Print the chi-square test result
print(chisq_result_carson)

