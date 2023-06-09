'FR-07337-Final-Task-Financial-Times'

#Reset Environment
rm(list = ls())

# Load necessary libraries/packages
#For data manipulation
library(dplyr)
library(tidyverse)
library(date)
library(lubridate)

#For web scrapping
library(rvest)
library(purrr)

#For plotting
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(viridis) #Color map that has been designed to improve readability for readers with common forms of color blindness and/or color vision deficiency
library(ggtext)
library(ggfittext)


# Initialize empty data frame to store data from Box Office Mojo
mojo_df <- data.frame()

#Loop over the years of interest
for(year in 2018:2023){
  
  #Construct the link for the specific year
  link = paste0("https://www.boxofficemojo.com/weekly/by-year/", year,"/")
  
  #Scrape the web page
  pages = read_html(link)
  
  #Extract the table from the web page
  page_bo<- pages %>% 
    html_nodes("table") %>% 
    html_table()
  
  # Make column names unique for each data frame in the list
  page_bo <- map(page_bo, ~ setNames(., make.names(names(.), unique=TRUE)))
  
  # Add column to each data frame in the list with the specific year
  page_bo <- map(page_bo, ~ mutate(., Year = year))
  
  # Ensure 'Releases' column is of integer type
  page_bo <- map(page_bo, ~ mutate(., Releases = as.integer(Releases)))
  
  # Append the yearly data to the main dataframe
  mojo_df <- bind_rows(mojo_df, page_bo)
}

'Cleaning the data set obtained'

#Converting prices into integers and rounding the number to million $
mojo_df <- mojo_df %>%  
  mutate(Overall.Gross = parse_number(Overall.Gross)) %>%
  mutate(Top.10.Gross = parse_number(Top.10.Gross)) %>%
  mutate(Overall.Gross = Overall.Gross/10^6) %>% #Divide by a million
  mutate(Top.10.Gross = Top.10.Gross/10^6)


#Remove rows where 'Overall.Gross' NA - since we are only interested in the available data
mojo_df <- mojo_df %>%
  filter(!is.na(Overall.Gross))

#Create a Date column that is in Date Format as opposed from character format in the column 'Dates'
mojo_df <- mojo_df %>%
  mutate(
    # Extract the month and day from the 'Dates' column
    MonthDay = substr(Dates, 1, 6),  # Assuming the format in 'Dates' is always 'Month Day'
    # Create a full date by pasting the year, month, and day together and converting to Date
    Date = mdy(paste(MonthDay, Year))
  )

# Create a new variable to represent each 4-week period
mojo_df <- mojo_df %>%
  mutate(FourWeekPeriod = (as.numeric(Date - as.Date("2018-01-05"))) %/% 28 + 1)

# Create a new variable to calculate the average gross revenue in a 4-week period
average_gross <- mojo_df %>%
  group_by(FourWeekPeriod) %>%
  summarize(AverageGross = mean(Overall.Gross, na.rm = TRUE))

#Join the original data with the 4-week averages 
mojo_df <- inner_join(mojo_df, average_gross, by = "FourWeekPeriod")

'Chart 1 : Column-Line Timeline'

ggplot(mojo_df, aes(x = Date)) +
  geom_bar(aes(y = Overall.Gross, #Add bars to represent the 'Overall.Gross'
               fill = ifelse(Overall.Gross > 400, 'Above $400mn', 'Below $400mn')), #Color the bars based on the $400mn threshold 
           stat = "identity", #Height of the bar equal to the value
           alpha = 0.7) + #Transparency of the bars
  
  scale_fill_manual(values = c('Above $400mn'= '#88CCEE' , 'Below $400mn' = '#888888'),  # Manually define the color scale according to color-blind friendly guidelines
                    name = "Weekly Overall Gross\n (Labelled with #1 Release)") +
 
   geom_line(aes(y = AverageGross, #Add a dashed line to represent the 4-week average gross
                color = "4-Week Average"), 
            linetype = 'dashed') +
  
  scale_color_manual(values = "black",#Manually define colour of line
                     name = "Trend line") +

  geom_text_repel(data = subset(mojo_df, Overall.Gross > 400), # Add labels to the bars
                  aes(label = X.1.Release, y = Overall.Gross),
                  direction = 'y',
                  segment.color = 'grey50',
                  vjust=1,
                  size = 3,
                  fontface = "bold",
                  position=position_dodge(width=1)) +
  
  scale_x_date(date_breaks = "3 months", ## Define the x-axis scales with date breaks every 3 months
               date_labels = "%b\n%Y") +
  
  scale_y_continuous(limits = c(0, 500), # Define the y-axis scales with custom limits and breaks
                     seq(0, 500, by = 100),
                     name = NULL)+
  labs(x = NULL, #Set labels
       y = NULL,
       title = "Domestic Box Office Numbers Failing to Match Pre-Pandemic Levels",
       subtitle = "Weekly Gross US Box Office Revenue (in $ million)",
       caption = "Source : Box Office Mojo\nGross not adjusted for ticket price inflation")+
 
  # Add a rectangle and text annotation to highlight the cinema shutdown period during the pandemic
  annotate("rect", 
           xmin = as.Date("2020-03-20"), 
           xmax = as.Date("2020-08-07"), 
           ymin = 0, 
           ymax = 50, 
           alpha = 0.2, 
           fill = "lightblue") +
  annotate("text", 
           x = as.Date("2020-03-20") + (as.Date("2020-08-07") - as.Date("2020-03-20"))/2, 
           y = 25, label = "Cinema\nshutdown", 
           fontface = "italic", 
           hjust = 0.5, 
           size = 3) +
  theme_clean(base_size = 11)+
  theme(panel.grid.major.x = element_blank())

'Chart 2: Calendar Heatmap'

# Add a column representing the month name
mojo_df <- mojo_df %>% 
  mutate(Month = month.name[month(Date)], ordered = TRUE)

# Define the order of months
level <- c("January","February","March","April","May","June","July","August","September","October","November","December")

# Group by month and year, summarise the total release
mojo_df %>% 
group_by(Month, Year) %>% 
summarize(TotalReleases = sum(Releases), .groups = "drop") %>% 
ggplot(aes(Month, Year, fill = TotalReleases, label = TotalReleases)) + 
  
  geom_tile(color = "white", 
            size = 0.1) + 
  geom_fit_text(contrast = TRUE)+ #Add contrasting text labels
  
  scale_fill_viridis(name = NULL, # Define the color scale using the virdis color map
                     option="magma",
                     direction = -1,
                     limits = c(0,700),
                     guide = guide_colorbar(
                       draw.ulim = TRUE, 
                       draw.llim = TRUE,
                       barwidth  = 10,
                       nbin = 5)
                       ) +
  
  scale_x_discrete(limits = level)+ # Define the y-axis scale with custom month levels
  
  scale_y_reverse(seq(2018, 2023, by = 1), # Define the x-axis scale with custom breaks

                     name = NULL)+
  # Set the labels for the plot
  labs(x=NULL,
       y=NULL,
       title = "Decline in Domestic Releases: Fewer Available Titles than in Pre-Pandemic Times",
       subtitle="Number of Available Releases in the US",
       caption = "Source : Box Office Mojo")+
 
  theme_minimal() +
  
  theme(legend.position = "top",
        legend.justification='left',
        legend.direction='horizontal',
        plot.title = element_text(face = "bold",size =14),
        axis.text =  element_text(face = "bold",size=12)) 
  
