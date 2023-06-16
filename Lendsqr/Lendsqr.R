library(readxl)
library(tidyverse)
library(janitor)
library(gt)

A1Credit_App <- read_excel("~/A1Credit.xlsx")
IRORUN_WEB_APP <- read_excel("~/IRORUN WEB APP.xlsx")
IRORUN_App <- read_excel("~/IRORUN App.xlsx")

A1Credit_App.1 <- A1Credit_App %>% 
  separate(Amount, into = c("Min loan Amount", "Max loan Amount"), sep = " to ") %>% 
  separate(`Loan Duration`, into = c("Min loan_duration", "Max loan duration"), sep = " to ") %>% 
  mutate(`Min loan duration` = c("7 days", "1 months", "1 months")) %>%
  mutate(product_id = c(101, 102, 103)) %>% 
  select(Products, product_id, `Min loan Amount`, `Max loan Amount`, `Min loan duration`, `Max loan duration`, `Interest per month (%)`, `Processing Fee (%)`, `Repayment frequency`)

IRORUN_App.1 <- IRORUN_App %>% 
  separate(Amount, into = c("Min loan Amount", "Max loan Amount"), sep = " to ") %>% 
  separate(`Loan Duration`, into = c("Min loan duration (days)", "Max loan duration (days)"), sep = " to ") %>% 
  mutate(product_id = c(101, 102, 103, 104))

IRORUN_WEB_APP.1 <- IRORUN_WEB_APP %>%
  rename(Products = Product) %>% 
  separate(Amount, into = c("Min loan Amount", "Max loan Amount"), sep = " to ") %>% 
  separate(`Loan  duration (days)`, into = c("Min loan duration (days)", "Max loan duration (days)"), sep = " to ") %>% 
  mutate(product_id = c(101, 102, 103, 104))

# Creating tables 

IRORUN_App.1 %>%gt() %>% 
  tab_header(title = "Products of Irorun (mobile app)") %>% 
  tab_style(style = list(cell_borders(style = "solid")),
            locations = cells_body(columns = c(Products, `Min loan duration (days)`, `Max loan duration (days)`, `Repayment Frequency`, `No. of Prerequisite Loan`, `Customer's Class`))
            ) %>% 
  tab_style(style = list(cell_fill(color = "#a2d0ef"), 
                         cell_text(weight = "bold"), cell_borders(style = "solid")),
            locations = cells_body(columns = c(`Min loan Amount`, `Max loan Amount`))
            ) %>% 
  tab_style(style = list(cell_text(weight = "bolder")),
            locations = cells_body(columns = Products)) %>% 
  tab_style(style = list(cell_fill(color = "#afefb8"),
                         cell_text(weight = "bold"), cell_borders(style = "solid")),
            locations = cells_body(columns = c(`Interest (%)`, `Processing Fee`))
            )

IRORUN_WEB_APP.1 %>%gt() %>% 
  tab_header(title = "Products of Irorun (web app)") %>% 
  tab_style(style = list(cell_borders(style = "solid")),
            locations = cells_body(columns = c(Products, `Min loan duration (days)`, `Max loan duration (days)`, `Repayment frequency`, `Prerequisite loan`))
            ) %>% 
  tab_style(style = list(cell_fill(color = "#b2d0d5"), 
                         cell_text(weight = "bold"), cell_borders(style = "solid")),
            locations = cells_body(columns = c(`Min loan Amount`, `Max loan Amount`))
            ) %>% 
  tab_style(style = list(cell_text(weight = "bolder")),
            locations = cells_body(columns = Products)) %>% 
  tab_style(style = list(cell_fill(color = "#cfef7a"),
                         cell_text(weight = "bold"), cell_borders(style = "solid")),
            locations = cells_body(columns = c(`Interest per month (%)`, `Processing fee`))
            )

A1Credit_App.1 %>%gt() %>% 
  tab_header(title = "Products of A1Credit") %>% 
  tab_style(style = list(cell_borders(style = "solid")),
            locations = cells_body(columns = c(Products, `Min loan duration`, `Max loan duration`, `Repayment frequency`))
            ) %>% 
  tab_style(style = list(cell_fill(color = "#c2d05f"), 
                         cell_text(weight = "bold"), cell_borders(style = "solid")),
            locations = cells_body(columns = c(`Min loan Amount`, `Max loan Amount`))
            ) %>% 
  tab_style(style = list(cell_text(weight = "bolder")),
            locations = cells_body(columns = Products)) %>% 
  tab_style(style = list(cell_fill(color = "#e3a35c"),
                         cell_text(weight = "bold"), cell_borders(style = "solid")),
            locations = cells_body(columns = c(`Interest per month (%)`, `Processing Fee (%)`))
            )


comparison_df <- full_join(A1Credit_App.1, IRORUN_App.1, by = "product_id") %>%
  full_join(., IRORUN_WEB_APP.1, by = "product_id")

comparison_df <- comparison_df %>%
  rename(`A1Credit interest rate` = `Interest per month (%).x`,
         `Irorun mobile app interest rate` = `Interest (%)`,
         `Irorun web app interest rate` = `Interest per month (%).y`) %>% 
  rename(`Products of A1Credit` = Products.x,
         `Products of Irorun (mobile app)` = Products.y,
         `Products of Irorun (web app)` = Products) %>% 
  rename(`A1Credit app processing fee (%)` = `Processing Fee (%)`,
         `Irorun mobile app processing fee` = `Processing Fee`,
         `Irorun web app processing fee (₦)` = `Processing fee`)


comparison_df %>%
  select(`Products of A1Credit`,
         `Products of Irorun (mobile app)`,
         `Products of Irorun (web app)`,
         `A1Credit interest rate`,
         `Irorun mobile app interest rate`,
         `Irorun web app interest rate`,
         `A1Credit app processing fee (%)`,
         `Irorun mobile app processing fee`,
         `Irorun web app processing fee (₦)`
  ) %>%
  gt() %>%
  tab_header(title = "Comparison of Loan Products") %>%
  tab_spanner(label = "Products", columns = c(`Products of A1Credit`, `Products of Irorun (mobile app)`, `Products of Irorun (web app)`)) %>%
  tab_spanner(label = "Interest Rate", columns = c(`A1Credit interest rate`, `Irorun mobile app interest rate`, `Irorun web app interest rate`)) %>%
  tab_spanner(label = "Processing Fee", columns = c(`A1Credit app processing fee (%)`, `Irorun mobile app processing fee`, `Irorun web app processing fee (₦)`)) %>%
  tab_style(
    style = list(cell_fill(color = "#FFA500"),
                 cell_text(weight = "bolder")),
    locations = cells_column_spanners()
  ) %>%
  tab_style(style = list(cell_borders(style = "solid")), locations = cells_body(columns = everything())
            ) %>% 
  tab_style(style = list(cell_fill(color = "#e3a35c"),
                         cell_text(weight = "bold"), cell_borders(style = "solid")),
            locations = cells_body(columns = c(`A1Credit interest rate`, `Irorun mobile app interest rate`, `Irorun web app interest rate`))
            )




# Creating visualization 

library(ggplot2)

# Bar plot for interest rates of A1Credit products
ggplot(comparison_df, aes(x = `Products of A1Credit`, y = `A1Credit interest rate`, fill = `Products of A1Credit`)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(`A1Credit interest rate`, 2)), vjust = -0.5, color = "black") +
  labs(x = "Products", y = "Interest Rate (%)", title = "Comparison of Interest Rates") +
  theme_minimal() +
  theme(legend.position = "none")

# Bar plot for processing fees of A1Credit products
ggplot(comparison_df, aes(x = `Products of A1Credit`, y = `A1Credit app processing fee (%)`, fill = `Products of A1Credit`)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = `A1Credit app processing fee (%)`), vjust = -0.5, color = "black") +
  labs(x = "Products", y = "Processing Fee (%)", title = "Comparison of Processing Fees") +
  theme_minimal() +
  theme(legend.position = "none")



# Bar plot and line plot for interest rates of Irorun Mobile App
ggplot(comparison_df, aes(x = `Products of Irorun (mobile app)`)) +
  geom_bar(aes(y = `Irorun mobile app interest rate`, fill = `Products of Irorun (mobile app)`), stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(y = `Irorun mobile app interest rate`, label = round(`Irorun mobile app interest rate`, 2)), vjust = -0.5, color = "black", size = 3) +
  labs(x = "Products", y = "Interest Rate (%)", title = "Comparison of Irorun Mobile App Interest Rates") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


# Bar plot and line plot for interest rates of Irorun Mobile App
ggplot(comparison_df, aes(x = `Products of Irorun (web app)`)) +
  geom_bar(aes(y = `Irorun mobile app interest rate`, fill = `Products of Irorun (mobile app)`), stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(y = `Irorun mobile app interest rate`, label = round(`Irorun mobile app interest rate`, 2)), vjust = -0.5, color = "black", size = 3) +
  labs(x = "Products", y = "Interest Rate (%)", title = "Comparison of Irorun Mobile App Interest Rates") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


install.packages("LaTeX")
