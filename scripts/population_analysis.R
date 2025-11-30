  library(ggplot2)
  library(reshape2)
  
  # Load the dataset
  data <- read.csv("C:\\Users\\mohit\\Downloads\\population-by-age-group-with-projections.csv")
  
  # Filter for India
  data <- subset(data, Entity == "India")
  
  # Select relevant columns for age groups
  data <- data[, c("Year",
                   "Population...Sex..all...Age..all...Variant..estimates",
                   "Population...Sex..all...Age..all...Variant..medium",
                   "Population...Sex..all...Age..65....Variant..estimates",
                   "Population...Sex..all...Age..65....Variant..medium",
                   "Population...Sex..all...Age..25.64...Variant..estimates",
                   "Population...Sex..all...Age..25.64...Variant..medium",
                   "Population...Sex..all...Age..0.24...Variant..medium",
                   "Population...Sex..all...Age..0.24...Variant..estimates",
                   "Population...Sex..all...Age..0.14...Variant..estimates",
                   "Population...Sex..all...Age..0.14...Variant..medium",
                   "Population...Sex..all...Age..0.4...Variant..estimates",
                   "Population...Sex..all...Age..0.4...Variant..medium")]
  
  # Reshape data for ggplot
  data_melted <- melt(data, id.vars = "Year", variable.name = "Age_Group", value.name = "Population")
  
  # Define mapping of column names to labels
  age_group_mapping <- list(
    "Population...Sex..all...Age..all...Variant..estimates" = "Total (Estimates)",
    "Population...Sex..all...Age..all...Variant..medium" = "Total (Projections)",
    "Population...Sex..all...Age..65....Variant..estimates" = "Age 65+ (Estimates)",
    "Population...Sex..all...Age..65....Variant..medium" = "Age 65+ (Projections)",
    "Population...Sex..all...Age..25.64...Variant..estimates" = "Age 25-64 (Estimates)",
    "Population...Sex..all...Age..25.64...Variant..medium" = "Age 25-64 (Projections)",
    "Population...Sex..all...Age..0.24...Variant..medium" = "Age 0-24 (Projections)",
    "Population...Sex..all...Age..0.24...Variant..estimates" = "Age 0-24 (Estimates)",
    "Population...Sex..all...Age..0.14...Variant..estimates" = "Age 0-14 (Estimates)",
    "Population...Sex..all...Age..0.14...Variant..medium" = "Age 0-14 (Projections)",
    "Population...Sex..all...Age..0.4...Variant..estimates" = "Age 0-4 (Estimates)",
    "Population...Sex..all...Age..0.4...Variant..medium" = "Age 0-4 (Projections)"
  )
  
  data_melted$Age_Group <- factor(data_melted$Age_Group, levels = names(age_group_mapping), labels = age_group_mapping)
  
  # Convert population to billions for better readability
  data_melted$Population <- data_melted$Population / 1e9
  
  # Custom color palette
  custom_colors <- c("Total (Estimates)" = "#E3120B", "Total (Projections)" = "#E3120B",
                     "Age 25-64 (Estimates)" = "#1F2E7A", "Age 25-64 (Projections)" = "#1F2E7A",
                     "Age 65+ (Estimates)" = "#36E2BD", "Age 65+ (Projections)" = "#36E2BD",
                     "Age 0-24 (Estimates)" = "#FB9851", "Age 0-24 (Projections)" = "#FB9851",
                     "Age 0-14 (Estimates)" = "#B3B3B3", "Age 0-14 (Projections)" = "#B3B3B3",
                     "Age 0-4 (Estimates)" = "#9B59B6", "Age 0-4 (Projections)" = "#9B59B6")
  
  # Define line types (dotted for projections)
  custom_linetypes <- c("Total (Estimates)" = "solid", "Total (Projections)" = "dashed",
                        "Age 25-64 (Estimates)" = "solid", "Age 25-64 (Projections)" = "dashed",
                        "Age 65+ (Estimates)" = "solid", "Age 65+ (Projections)" = "dashed",
                        "Age 0-24 (Estimates)" = "solid", "Age 0-24 (Projections)" = "dashed",
                        "Age 0-14 (Estimates)" = "solid", "Age 0-14 (Projections)" = "dashed",
                        "Age 0-4 (Estimates)" = "solid", "Age 0-4 (Projections)" = "dashed")
  
  # Plot
  ggplot(data_melted, aes(x = Year, y = Population, color = Age_Group, linetype = Age_Group)) +
    geom_line(size = 1.2) +
    geom_vline(xintercept = 2023, linetype = "dotted", color = "black") +
    theme_minimal(base_size = 12) +  # Adjust base font size
    labs(title = "India's Population Growth",
         subtitle = "Historical Estimates (1950-2023) and Projections (2024-2100)",
         x = "Year",
         y = "Population (Billions)",
         caption = "Data Source: UN, World Population Prospects (2024)  |  Created by: Mohit Luthra") +
    scale_color_manual(values = custom_colors) +
    scale_linetype_manual(values = custom_linetypes) +
    scale_y_continuous(breaks = seq(0, 2, 0.2)) +  # Set Y-axis breaks at every 200 million
    scale_x_continuous(breaks = c(1950, 1980, 2000, 2020, 2040, 2060, 2080, 2100)) +  # Add year breaks
    theme(
      legend.position = "none",  # Remove legend
      plot.title = element_text(face = "bold", size = 16),  # Title font size
      plot.subtitle = element_text(size = 12, margin = margin(b = 40)),  # Subtitle font size
      panel.background = element_rect(fill = "#FFFFFF", color = NA),  # Background color
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey70", size = 0.1),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 50, 20),  # Increase bottom margin
      plot.caption = element_text(vjust =-3,hjust = 0, size = 10, color = "grey40")  # Increased right margin for text labels
    )
  ggsave("India_Population_Growth22.jpeg", width = 12, height = 8, dpi = 300)
