# This app wraps everything together from the walkthrough and extends it to harvest data from a list of feeds
# on a rolling basis and displaying this in an app for search

# Install required libraries if needed
packages <- c("httr", "jsonlite", "tidyr", "leaflet", "ggplot2", "dplyr", "shiny")
for (package in packages) {
  if (!is.element(package, installed.packages())) {
    install.packages(package)
  }
}

# Load required libraries
library(httr)
library(jsonlite)
library(tidyr)
library(leaflet)
library(ggplot2)
library(dplyr)
library(shiny)

# Setting up
# Set a local subfolder for storing data between sessions
datastore <- "OpenActive"
# Check if you're working in the subfolder already and if not, create the subfolder if it doesn't exist
if (strsplit(getwd(), "/")[[1]][length(strsplit(getwd(), "/")[[1]])] == datastore) {
  print(paste("AlREADY IN FOLDER:", datastore))
} else if (!file.exists(datastore)) {
  print(paste("FOLDER DOES NOT EXIT SO CREATING:", datastore))
  dir.create(datastore)
  setwd(datastore)
} else {
  print(paste("FOLDER ALREADY EXISTS:", datastore))
  setwd(datastore)
}

## Create a control table (list of feeds) if one not found
if (!file.exists("control.rds")) {
  url <- "http://dataset-directory.herokuapp.com/datasets"
  res <- try(GET(url), silent = TRUE)
  if (!inherits(res, "try-error")) {
    if (res$status_code >= 200 && res$status_code < 400) {
      data_feeds <- NULL
      # If status ok, read content
      data_feeds <- fromJSON(rawToChar(res$content))
    }
  } else {
    print("Unable to read data feeds URL")
  }
  rm(res)
  # Create a control table
  control <- data.frame(feed_no = 1:nrow(data_feeds), stem = data_feeds$dataurl, NEXT = data_feeds$dataurl, skip = "Y")
  # Save to file in the folder
  saveRDS(control, file = "control.rds")
  print("Control table created")
  rm(data_feeds)
  rm(control)
} else {
  print("Control table already exists")
}

# Steps to harvest data from a list of feeds in a control table
# 1) Get stems from control table
# 2) Check control table to see which page to read
# 3) Read a page
# 4) Check if the page has new data
# 5) Read in any existing data
# 6) Append rows
# 7) Sort by id and modified and keep last modified
# 8) Storing the updated data
# 9) Update the control table.
# 10) If end of feed: go on to next feed
# 11) If not: Go back to start

# First, here's a slightly more robust version of the earlier callURL function
# It also includes a check to see if 5 seconds has passed since last call, to reduce burden on data providers
callURL <- function(url, lastStartTime) {
  # Check if at least 5 seconds have passed before calling URL
  elapsedTime <- difftime(Sys.time(), lastStartTime, units = "secs")
  if (elapsedTime < 5) {
    Sys.sleep(5 - as.numeric(elapsedTime))
  }
  res <- NULL
  res <- try(GET(url), silent = T)
  if (!inherits(res, "try-error")) {
    if (res$status_code >= 200 && res$status_code < 400) {
    } else {
      print("Reading URL status error")
      res <- NULL
    }
  } else {
    print("Reading URL failed")
    res <- NULL
  }
  res
}

# A quick function to tidy up after reading each page
cleanUp <- function() {
  objects <- c("control", "d", "data", "dataToStore", "endOfFeed", "files", "nextUrl", "previous")
  for (object in objects) {
    if (exists(object)) {
      rm(object)
    }
  }
}

# This next lines reduces the number of feeds being read by setting a 'skip' variable to Y.
# If skip is Y, then that row/feed is skipped.
# Running this repeatedly will randomly set skip to N - meaning that you read in more feeds.
# I used this to gradually increase the number of feeds being read, to debug and check performance at scale.
n_feeds <- 3
control <- readRDS(file = "control.rds")
control$skip <- ifelse(control$stem == control$NEXT, "Y", control$skip)
sample <- sample(nrow(control), n_feeds)
for (s in sample) {
  control$skip[s] <- "N"
}
# You can also explicitly specify to include a feed as follows:
# Set skip to N where control$stem includes the text 'runtogether'...
# control$skip[grepl("runtogether",control$stem)] <- "N"
# control$skip[grepl("englandnetball",control$stem)] <- "N"
# For live operation, you can ignore the sample and just use the line below
# control$skip <- "N"
saveRDS(control, file = "control.rds")
table(control$skip)
rm(control)

# These lines were used to answer Jules' query.
# feed <- control$feed_no[grepl("LED-live",control$stem)]
# feed
# previous = readRDS(paste0("feed_no_",feed,".rds")) %>% type.convert()
# glimpse(previous)
# Filter OAData to show any records containing a text string: stadium
# d <- previous[grep("Stadium",previous$data.organizer.name),]
# glimpse(d)
# names(d)
# Split the data.organizer.name column into a list of strings
# organizer_names <- strsplit(previous$data.organizer.name, " ")
# paste(d$id, "found at", d$sourceUrl)
# unique(d$sourceUrl)


# Define UI for a simple application
ui <- fluidPage(
  # css for visual styling
  tags$style("#logo {margin: 0px 10px}"),
  # header row
  titlePanel(fluidRow(
    tags$img(
      src = "https://www.openactive.io/wp-content/themes/open-active-1_4/images/open-active-logo.svg",
      width = "100px", id = "logo"
    ), "Exploring OpenActive Opportunity Data in R"
  )),

  # Sidebar with filters
  sidebarLayout(
    sidebarPanel(
      span("API status:", textOutput("status", inline = TRUE)),
      hr(), # Add a horizontal divider
      checkboxGroupInput(
        "activity", "Filter by Activity / Facility",
        choices = "",
        selected = "",
      ),
      hr(), # Add a horizontal divider
      strong("Opportunities by feed type:"),
      br(), br(),
      plotOutput("myplot", width = "100%")
    ),
    # Main panel
    mainPanel(
      leafletOutput("mymap"),
      br(),
      p("Click on an Opportunity marker on the map to see the data."),
      hr(),
      tableOutput("record")
    )
  )
)

# Define the server code
server <- function(input, output, session) {

  # Define the reactive dataset
  display <- reactiveVal(data.frame())

  # Define the reactive variables
  read_data_status <- reactiveVal("stopped")
  output$status <- renderText(read_data_status())
  
  # Display map
  output$mymap <- renderLeaflet({
    leaflet() %>% addTiles()
  })

  # Define the triggers
  collate_data_trigger <- reactiveTimer(6 * 60 * 60 * 1000, session) # Run every 6 hours
  read_data_trigger <- reactiveTimer(1 * 60 * 60 * 1000, session) # Run every 1 hours

  # Define the functions
  collate_data <- function() {
    output$status <- renderText(read_data_status())
    print("COLLATING")
    # Collate the data from Cloud Storage and present in a Leaflet map
    # Combine the OA data from the collected feeds
    control <- readRDS(file = "control.rds")
    control <- filter(control, skip == "N")
    feeds <- unique(control$feed_no)
    OAData <- data.frame()
    for (feed in feeds) {
      files <- list.files()
      previous <- paste0("feed_no_", feed, ".rds")
      if (previous %in% files) {
        dataToStore <- readRDS(previous) %>% ungroup()
        # Pull out activity if available
        if ("data.facilityType" %in% names(dataToStore)) {
          dataToStore <- hoist(dataToStore, data.facilityType, "prefLabel")
        }
        if ("data.activity" %in% names(dataToStore)) {
          dataToStore <- hoist(dataToStore, data.activity, "prefLabel")
        }
        # Handle variable format issues
        print(paste("ADDING", nrow(dataToStore), "ITEMS"))
        OAData <- bind_rows(OAData, dataToStore %>% mutate_all(as.character))
        rm(dataToStore)
      } else {
        print(paste("NO DATA FOR FEED", feed))
      }
    }
    OAData <- OAData %>% type.convert(as.is = TRUE)

    # Update choices
    choices <- filter(OAData, !is.na(data.location.geo.latitude)) %>%
      select(prefLabel) %>%
      arrange(prefLabel)
    choices <- unique(choices$prefLabel)
    updateCheckboxGroupInput(session, "activity",
      choices = choices,
      selected = choices[[1]]
    )

    # Display chart
    output$myplot <- renderPlot({
      ggplot(OAData, aes(x = kind, fill = kind)) +
        geom_bar() +
        scale_fill_brewer() +
        theme_void() +
        coord_flip() +
        theme(
          legend.position = "bottom", legend.title = element_blank(),
          axis.text.y = element_text(),
          panel.grid.major.y = element_line(color = "gray", linetype = "dashed")
        )
    })
    # Update the display dataset
    display(OAData)
  }

  read_data <- function() {
    # 1) Get no of feeds from control table
    control <- arrange(readRDS(file = "control.rds"), feed_no)
    control <- filter(control, skip == "N")
    n_stems <- nrow(control)
    # update this next line to account for the skip variable
    stems <- unique(control$feed_no)
    rm(control)
    # For each stem
    feed <- 1
    page <- 1
    startTime <- Sys.time() - 5
    while (feed <= n_stems && read_data_status() == "running") {
      print(read_data_status())
      stem_no <- stems[feed]
      # 2) Identify next page for the feed
      control <- readRDS(file = "control.rds")
      nextUrl <- filter(control, feed_no == stem_no)$NEXT
      print(paste("Reading feed:", feed, "/", n_stems, " page:", page, "-", nextUrl))
      # 3) Read next page
      lastStartTime <- startTime
      startTime <- Sys.time()
      d <- callURL(nextUrl, lastStartTime)
      # If read page ok, read content
      if (!is.null(d)) {
        data <- rawToChar(d$content)
        if (jsonlite::validate(data) == T) {
          # If JSON is valid, unpack it
          data <- fromJSON(rawToChar(d$content), flatten = T)
          # 4) Check if the new page has items
          if (is.data.frame(data$items)) {
            if (is.data.frame(data$items) & nrow(data$items) > 0) {
              # Added to handling a row binding issue
              if ("data.@context" %in% names(data$items)) {
                data$items$`data.@context` <- as.character(data$items$`data.@context`)
              }
              # 5) Appending new data to any existing data
              files <- list.files()
              previous <- paste0("feed_no_", stem_no, ".rds")
              if (previous %in% files) {
                dataToStore <- readRDS(previous)
                print(paste("READING", nrow(dataToStore), "ITEMS FROM PREVIOUS DATA AND ADDING", nrow(data$items), "ITEMS"))
                # 6) Append the new rows
                data$items$sourceUrl <- nextUrl
                dataToStore <- bind_rows(dataToStore, data$items)
                # 7) Sort by id and modified and keep last modified
                dataToStore <- arrange(dataToStore, id, modified) %>%
                  group_by(id) %>%
                  slice_tail()
              } else {
                print(paste("NO PREVIOUS DATA - CREATING FILE AND ADDING", nrow(data$items), "ITEMS"))
                dataToStore <- data$items
              }
              # 8) Storing the updated data
              # For displaying live opportunities, remove any 'deleted' items
              dataToStore <- filter(dataToStore, state == "updated")
              saveRDS(dataToStore, file = paste0("feed_no_", stem_no, ".rds"))
            } else {
              print("NO NEW ITEMS")
            }
          } else {
            print("NO NEW ITEMS")
          }
          # 9) Update the control table:
          control$NEXT[control$feed_no == stem_no] <- data$`next`
          saveRDS(control, file = "control.rds")

          # 10) If end of feed: stop
          # Simple check for end of feed - next URL is same as the URL you just called
          endOfFeed <- ifelse(nextUrl == data$`next`, TRUE, FALSE)
          if (endOfFeed) {
            print("END OF FEED")
            # clean up
            cleanUp()
            # Go to next line in control table
            feed <- feed + 1
            page <- 1
          } else {
            # 11) If not: Go back to start
            # print("CHECK NEXT PAGE")
            page <- page + 1
          }
        } else {
          print("Invalid JSON")
        }
      } else {
        print("Unable to read next page")
        # clean up
        cleanUp()
        # Go to next line in control table
        feed <- feed + 1
        page <- 1
      }
    }
    # Set the read_data_status
    read_data_status("stopped")
    output$status <- renderText(read_data_status())
  }

  # Observe the collate_data_trigger
  observeEvent(collate_data_trigger(), {
    if (read_data_status() == "running") {
      # Set the read_data_status to "paused"
      read_data_status("paused")
      output$status <- renderText(read_data_status())
      # Wait a few moments to allow read_data to pause
      Sys.sleep(30)
    } else {
      # Set the read_data_status to "paused"
      read_data_status("paused")
      output$status <- renderText(read_data_status())
    }
    collate_data()
    print("COLLATED")
    # Set the read_data_status

    read_data_status("stopped")
    output$status <- renderText(read_data_status())
  })

# Update markers on map
  observeEvent(input$activity, {
    subsetted <- filter(display(), prefLabel %in% input$activity) %>%
      filter(!is.na(data.location.geo.latitude))
    if (nrow(subsetted) > 0) {
      proxy <- leafletProxy("mymap") %>%
        clearMarkers() %>%
        clearMarkerClusters() %>%
        addMarkers(
          lng = subsetted$data.location.geo.longitude,
          lat = subsetted$data.location.geo.latitude,
          popup = subsetted$prefLabel, layerId = subsetted$id,
          clusterOptions = markerClusterOptions()
        ) %>%
        flyToBounds(
          min(subsetted$data.location.geo.longitude),
          min(subsetted$data.location.geo.latitude),
          max(subsetted$data.location.geo.longitude),
          max(subsetted$data.location.geo.latitude)
        )
    }
  })

  # When map marker clicked, show raw data
  observeEvent(input$mymap_marker_click, {
    record <- t(filter(display(), id == input$mymap_marker_click$id))
    output$record <- renderTable(record, colnames = F, rownames = T)
  })

  # Observe the read_data_trigger
  observeEvent(collate_data_trigger(), {
    if (read_data_status() == "stopped") {
      # Set the read_data_status to "running"
      read_data_status("running")
      output$status <- renderText(read_data_status())
      read_data()
    }
  }, ignoreInit = TRUE)
}

# Run the application
shinyApp(ui = ui, server = server)
