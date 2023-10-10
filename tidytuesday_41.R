
install.packages("pacman")
pacman::p_load(tidyverse, tm, wordcloud, htmlwidgets, tidyr, RColorBrewer,plotly, tidytext, tidytuesdayR, png, grid)

tuesdata <- tidytuesdayR::tt_load(2023, week = 41)

h_data <- tuesdata$haunted_places 

state_counts <- h_data %>%
  group_by(state) %>%
  summarise(count = n())
state_counts$state <- tolower(state_counts$state)


word_frequencies <- tokenized_data %>%
  group_by(state, word) %>%
  tally() %>%
  arrange(state, -n)

word_frequencies$state <- tolower(word_frequencies$state)

top_10_words_by_state <- word_frequencies %>%
  group_by(state) %>%
  slice_max(n, n = 10) %>%
  summarise(tooltip_text = str_c(word, collapse=", "))

us_map <- map_data("state")


# Then, join the two dataframes on the state/region column
map_data_joined <- left_join(us_map, top_10_words_by_state, by = c("region" = "state"))
final_map_data <- left_join(map_data_joined, state_counts, by = c("region" = "state"))



state_abbreviations <- c(
  alabama = "AL", arizona = "AZ", arkansas = "AR", california = "CA", colorado = "CO", 
  connecticut = "CT", delaware = "DE", `district of columbia` = "DC", florida = "FL", georgia = "GA", 
  idaho = "ID", illinois = "IL", indiana = "IN", iowa = "IA", kansas = "KS", 
  kentucky = "KY", louisiana = "LA", maine = "ME", maryland = "MD", massachusetts = "MA", 
  michigan = "MI", minnesota = "MN", mississippi = "MS", missouri = "MO", montana = "MT", 
  nebraska = "NE", nevada = "NV", `new hampshire` = "NH", `new jersey` = "NJ", `new mexico` = "NM", 
  `new york` = "NY", `north carolina` = "NC", `north dakota` = "ND", ohio = "OH", oklahoma = "OK", 
  oregon = "OR", pennsylvania = "PA", `rhode island` = "RI", `south carolina` = "SC", 
  `south dakota` = "SD", tennessee = "TN", texas = "TX", utah = "UT", vermont = "VT", 
  virginia = "VA", washington = "WA", `west virginia` = "WV", wisconsin = "WI", wyoming = "WY"
)

final_map_data$region <- state_abbreviations[final_map_data$region]

map <- plot_ly(data = final_map_data, 
               locations = ~region, 
               locationmode = 'USA-states',
               z = ~count, 
               text = ~tooltip_text,
               hoverinfo = "text+z",
               hovertemplate = '<b>%{location}</b><br>No: %{z}<br>Haunted Words: %{text}',
               type = 'choropleth', 
               colorscale = list(c(0, "#facd63"), list(0.25, "#a78eb8"), list(0.5, "#644b77"), list(0.75, "#3e2445"), list(1, "#2e2e2e")),
               colorbar = list(title = "No. of Haunted Places")) %>%
  layout(title = "Haunted Places by State", font = list(
    family = "Arial",
    size = 24),
    geo = list(scope = 'usa'))

htmlwidgets::saveWidget(map, "haunted_41.html")
