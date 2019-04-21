library(tidyverse)
library(pdftools)

# Download report from https://www.documentcloud.org/documents/5955210-Redacted-Mueller-Report.html
mueller_report_txt <- pdf_text("Redacted-Mueller-Report.pdf")

mueller_report <- tibble(
  page = 1:length(mueller_report_txt),
  text = mueller_report_txt
) %>% 
  separate_rows(text, sep = "\n") %>% 
  group_by(page) %>% 
  mutate(line = row_number()) %>% 
  ungroup() %>% 
  select(page, line, text)

write_csv(mueller_report, "mueller_report_2.csv")

librarian::shelf(ggpage)

mueller_report_csv <- "mueller_report_2.csv"
mueller_report <- read_csv(mueller_report_csv)
mueller_report

mueller_pages <- function(highlight){
  mueller_report %>% 
  # pad pages with fewer lines than expected
  complete(
    page, 
    line = 1:max(mueller_report$line),
    fill = list(text = "")
  ) %>% 
  # Pre-process for {ggpage}
  ggpage_build(
    ncol = 30, 
    bycol = FALSE, 
    page.col = "page", 
    wtl = FALSE, 
    x_space_pages = 10,
    y_space_pages = 100
  ) %>% 
  mutate(
    color = case_when(
      str_detect(word, highlight) ~ highlight,
      TRUE ~ "normal"
    )
  ) 
}
mueller_pages0 <- mueller_pages(hl0)

plot_pages <- function(data){ 
  
  setdiff(unique(data$color), 'normal') -> my_title 
  
  ggpage_plot(data) +
  aes(fill = color) +
  scale_fill_manual(
    values = c("#d0d0d0", "red"), 
    breaks = "#d0d0d0"
  ) +
  labs(fill = NULL, caption = "@grrrck", title = my_title) +
  theme(legend.position = "bottom")
}
plot_pages(mueller_pages0)

gen_nestdf <- function(kwordv){
 mueller_pages(kwordv) %>% 
    mutate(
      kword = kwordv
    ) %>% 
  nest(-kword) %>% 
  mutate(
    plots = map(.$data, plot_pages)
  )
}

gen_nestdf('russia') -> vdf 
c('trump|president', 'russia') %>% map_df(gen_nestdf) -> vdf