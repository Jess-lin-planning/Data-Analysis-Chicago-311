#step 0 install packages
install.packages("tidyverse")
install.packages("janitor")

#step 1 load packages
library(tidyverse)
library(janitor)

#Data sets sources: Chicago Open Data - https://data.cityofchicago.org/
# Historical 311 Data: https://data.cityofchicago.org/Service-Requests/311-Service-Requests-Vacant-and-Abandoned-Building/7nii-7srd/about_data
# Historcial Data has 311 vacant/abandoned Building reports from 2010 - 2018
# 311 Service requests: https://data.cityofchicago.org/Service-Requests/311-Service-Requests/v6vf-nfxy/about_data
# 311 Service request data set was exported twice
# once for data on 2019-2026 and a seond time for just 2018 because I forgot
# NOTE for 311 Service requests: before exporting, I filtered out some columns that weren't needed
# to decrease the file size and for easier navigation, especially because we're only looking at
# 1 service type

#step 2 
#read all csv containing 311 service requests for vacant/abandoned buildings to clean names
#new_requests = 311 service requests (SR) between 2019 - 2026
new_requests <- read.csv("311_Service_Requests_.csv") |> 
  clean_names()
#Old_requests = 311 SR from 2018
old_requests <- read.csv("311_request_2018.csv") |> 
  clean_names()
#his_requests = 311 SR from the old data set
his_requests <- read.csv("311_Vacant_and_Abandoned_Buildings_Reported_-_2008-2018.csv") |> 
  clean_names() |> 
  rename(created_date = date_service_request_was_received) #renaming column to match the column name on other datasets

#step 3: mutating to fix dates into right format for later calculations on length of days to close and create new columns with year request was created
new_clean <- new_requests |> 
  #creating new column with just year ticket was created and cleaning date column to be mutated
  mutate(
    created_date = as_date(mdy_hms(created_date)),
    last_modified_date = as_date(mdy_hms(last_modified_date)),
    closed_date = as_date(mdy_hms(closed_date)),
    year = lubridate::year(created_date)
  ) |> 
  select(year, created_date, closed_date) #selecting only the necessary columns to calculate how long the request was open for before it was closed later on

old_clean <- old_requests |>
  #creating new column with just year ticket was created
  mutate(
      created_date = as_date(mdy_hms(created_date)),
      last_modified_date = as_date(mdy_hms(last_modified_date)),
      closed_date = as_date(mdy_hms(closed_date)),
      year = lubridate::year(created_date) #tried other ways to just get year, this was the only way that seemed to work without errors
  )|>
  filter(
    #filtering to only have 2018 tickets
    year == 2018,
    #removing all SRs that are legacy SR (from the historical data set) to avoid the being counted twice 
    is.na(legacy_record) | (legacy_record != "true" & legacy_record != TRUE)
  ) |> 
  select(year, created_date, closed_date) #selecting columns for clean table so that I can calculate how long it took for a request to be closed later

his_clean <- his_requests |> 
  #creating new column with just year ticket was created so that I can get at count of how many SR were created every year
  mutate(created_date = as_date(mdy(created_date)),
         year = lubridate::year(created_date)
  ) |> 
  filter(year == 2018) |> #selecting just 2018 from the data set
  select(year, created_date) |> #selecting year and date requests received for later join
  arrange(created_date) #rearranging dates so that it's in ascending order like the other data sets
#not really important but good for organization and aesthetics
#NOTE: this is completely avoidable if you export the 311 Service Request Data Set WITH 2018 included.
#I did not and opted to download a second set for 2018

#joining historical data and old data for total count of tickets received in 2018
legacy_total <- bind_rows(his_clean, old_clean) |> 
  arrange(year)
#binding rows helps join the data sets without creating any N/A that would happen with a full join

#Step 4: joining 2018 data set with 2019-2026 to get the full table to requests created
full_ticket_created <- 
  bind_rows(legacy_total, new_clean) |> 
  select(year, created_date) |> 
  group_by(year) |> 
  summarise(created_date = n())
#table that counts the total number of 311 service requests received per year
#We'll use this table for the bar chart on how many tickets were created every year between 2018-2026

#Step 5: create bar chart of total 311 requests created per year
total_plot <- ggplot(
  full_ticket_created, 
  aes(
    x = factor(year), 
    y = created_date, 
    fill = (year == 2018)
    )
) +
  geom_col(width = 0.7, color = "black", linewidth = 0.3) +
  scale_fill_manual(
    values = c(
      "TRUE"  = "#E4002B",
      "FALSE" = "black"
    ),
    labels = c(
      "TRUE"  = "old 311 system", #Label for Red
      "FALSE" = "new 311 system"  #Label for Black
    )
  ) +
  geom_text(
    aes(label = scales::comma(created_date)), 
    vjust = -0.5, 
    size = 3.5, 
    fontface = "bold"
  ) +
  scale_y_continuous(
    labels = scales::comma, 
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "311 Vacant/Abandoned Buildings Complaints Increased Between \n 2018 and 2019 Before Stabilizing in 2019-2026",
    subtitle = "Annual Total 311 Vacant/Abandoned Building Requests Created",
    x = "Year Created",
    y = "Total Tickets Received",
    fill = "System Era",  #Legend title
    caption = str_wrap("Source: City of Chicago Open Data Portal, 311 Service Requests and 311 Service Requests - Vacant and Abandoned Buildings Reported - Historical")
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey30", size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, face = "bold"),
    panel.grid.major.x = element_blank(),
    legend.position = "top"
  )

print(total_plot)

#step 6: create data frame to show how long each resolved service request took to close
days_to_close <- bind_rows(legacy_total, new_clean) |> 
  mutate(days_to_close = as.numeric(closed_date - created_date)) |> 
  filter(!is.na(year), !is.na(days_to_close), days_to_close > 0) #filtering out 0 days as that would imply it was closed the day of and filtering out all N/A in data

#step 6: Generate Table of min, max, median, mean, and percent of requests closed in 30 days or less for more detailed analysis
resolution_table <- days_to_close |> 
  group_by(year) |> 
  summarise(
    total_resolved = n(),
    # Calculate percentage closed in less than or equal to 30 days
    pct_under_30days = round(mean(days_to_close <= 30, na.rm = TRUE) * 100, 1),
  )

#exporting resolution_table as csv to format and edit on excel
write.csv(resolution_table, file = "311 resolution table.csv")

# Step 7: creating new data frame with just year and average day it takes to close to feed into a ggplot
close_summary <- days_to_close |> 
  group_by(year) |> 
  summarise(
    mean_days = mean(days_to_close)
  )

write.csv(close_summary, file = "Average Resolution Time.csv") #exporting table to excel for cleanup

# Step 8: Creating the Line Plot detailing average resolution times of tickets
line_plot <- ggplot(close_summary, aes(x = factor(year), y = mean_days, group = 1)) +
  geom_line(color = "black", linewidth = 1.2) +
  geom_point(color = "#E4002B", size = 3.5) +
  scale_y_continuous(
    limits = c(0, 300),
    breaks = seq(0, 300, by = 50), # Tracks requests at 0, 50, 100, 150, 200, 250, 300
    expand = expansion(mult = c(0, 0.1))
    ) +
  labs(
    title = "Average Resolution Time for Vacant/Abandoned Building Service Requests \nDropped Sharply After 2018",
    subtitle = "Annual Average Days It Takes to Close Vacant/Abandoned Building Requests",
    x = "Year Sevice Request was Created",
    y = "Average Days to Close Request",
    caption = str_wrap("Source: City of Chicago Open Data Portal, 311 Service Requests and 311 Service Requests - Vacant and Abandoned Buildings Reported - Historical")
  ) +
  theme_minimal()
print(line_plot)
