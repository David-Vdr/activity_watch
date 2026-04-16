### Processing and cleaning of raw data exports from activity watch ###
#Make sure to have a json file exported from activity watch in your working directory. (i believe activity watch automatically names these "aw-buckets-export.json")

#install.packages("jsonlite")  # run once if not installed
library(jsonlite)

raw_json <- readLines("aw-buckets-export.json", warn = FALSE) #this stores each line as a separate element in a character vector 
full_text <- paste(raw_json, collapse = "\n") #this glues together all of the strinsg so that we now have one massive string

# Parse it into an R object so you can also explore it programmatically
parsed <- fromJSON("aw-buckets-export.json", simplifyVector = FALSE) # JSON objects {} become R named lists, and JSON arrays [] become R lists (or vectors) (basically translating Json in native R form)

# getting to the data structure of these activity watch json files
print_tree <- function(x, indent = 0, max_depth = 7, max_items = 8) {
  if (indent >= max_depth || !is.list(x)) return()
  nms <- names(x)
  n <- min(length(x), max_items)
  for (i in seq_len(n)) {
    label <- if (!is.null(nms) && nms[i] != "") nms[i] else paste0("[[", i, "]]")
    cat(strrep("  ", indent), "- ", label, "\n", sep = "")
    print_tree(x[[i]], indent + 1, max_depth, max_items)
  }
  if (length(x) > max_items) {
    cat(strrep("  ", indent), "  ... (", length(x) - max_items, " more items)\n", sep = "")
  }
} ## a function to visualize the treelike structure of the lists within lists
print_tree(parsed)

parsed$buckets$`aw-stopwatch` <- NULL #removing the aw-stopwatch because we do not care about that
names(parsed$buckets) #checking it got removed

all_bucket_names <- names(parsed$buckets) #extracting the "aw-watcher-afk" and "aw-watcher-window" names (different per user)

window_bucket_names <- all_bucket_names[grepl("aw-watcher-window", all_bucket_names)] #identifies any bucket names with window in it
afk_bucket_names    <- all_bucket_names[grepl("aw-watcher-afk",    all_bucket_names)] #identifies any bucket names with afk in it
web_bucket_names <- all_bucket_names[grepl("aw-watcher-web", all_bucket_names)]

# taking out only the individual events so that i end up with lists of all events (across multiple possible sources/hosts of data, in my case macbook and radboud network for some reason)
window_events_raw <- unlist(
  lapply(window_bucket_names, function(b) parsed$buckets[[b]]$events),
  recursive = FALSE
)

afk_events_raw <- unlist(
  lapply(afk_bucket_names, function(b) parsed$buckets[[b]]$events),
  recursive = FALSE
)
# how many events of each do we have?
length(window_events_raw)  
length(afk_events_raw)     

flatten_event <- function(event) {
  top_level <- event[c("timestamp", "duration")]
  nested    <- event$data
  c(top_level, nested)
} #function that ensures that the sublist of "data" get's put at the same level as the "timestamp" and "duration" elements

#let's run this function on every single event within the two lists that we made:
window_events_flat <- lapply(window_events_raw, flatten_event)
afk_events_flat    <- lapply(afk_events_raw,    flatten_event)

#Now let's turn these lists into data frames
library(dplyr)
window_df <- bind_rows(lapply(window_events_flat, as.data.frame))
afk_df    <- bind_rows(lapply(afk_events_flat,    as.data.frame))

# running everything on the web bucket in one go within an if statement to avoid errors, for participant data without extension:
if (length(web_bucket_names) == 0) {
  message("No web watcher buckets found — skipping web_df creation.")
  web_df <- NULL
} else {
  web_events_raw <- unlist(
    lapply(web_bucket_names, function(b) parsed$buckets[[b]]$events),
    recursive = FALSE
  )
  web_events_flat <- lapply(web_events_raw, flatten_event)
  web_df <- bind_rows(lapply(web_events_flat, as.data.frame))
}

# Data is now ready to be cleaned and inspected









##### random stuff to be deleted

library(ggplot2)
library(dplyr)

#First lemme remove the loginwindow data which is rubbish
window_df <- window_df %>%
  filter(app != "loginwindow")

# Summarise duration per app
app_summary <- window_df %>%
  group_by(app) %>%
  summarise(total_duration = sum(as.numeric(duration), na.rm = TRUE)) %>%
  arrange(desc(total_duration)) %>%
  mutate(proportion = total_duration / sum(total_duration))

# Split into "keep" and "other" based on cumulative proportion
app_summary <- app_summary %>%
  mutate(cumulative = cumsum(proportion),
         app_label = ifelse(cumulative > 0.95 & lag(cumulative, default = 0) < 0.95 | 
                              cumulative > 0.95, "Other", app))

# Collapse "Other" into one row
app_plot <- app_summary %>%
  group_by(app_label) %>%
  summarise(total_duration = sum(total_duration)) %>%
  arrange(desc(total_duration)) %>%
  mutate(proportion = total_duration / sum(total_duration))

# Move "Other" to the end
app_plot <- bind_rows(
  app_plot %>% filter(app_label != "Other"),
  app_plot %>% filter(app_label == "Other")
)

# Plot
ggplot(app_plot, aes(x = "", y = total_duration, fill = app_label)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.position = "right") +
  labs(fill = "App", title = "Time spent per app")
``

