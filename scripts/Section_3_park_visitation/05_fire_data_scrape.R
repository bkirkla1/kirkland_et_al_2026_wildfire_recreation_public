#-------------------------------------------------------------------------------
# "05_fire_data_scrape.R": This script scrapes HMS fire data (series of shapefiles) and copies them
# to main data folder
#
# Important:
# Times given in UTC/GMT times
#
# Source: https://www.ospo.noaa.gov/Products/land/hms.html#about
#-------------------------------------------------------------------------------

# Load settings
source("./scripts/setup.R")

# Setup Selenium server
sel_server <- selenium(retcommand = TRUE) # setup server
rD <- rsDriver(browser = "firefox", chromever = NULL, check = FALSE,
               extraCapabilities = list(
                 "moz:firefoxOptions" = list(
                   args = list("--start-maximized")
                 )))
remDr <- rD[["client"]]
remDr$maxWindowSize()
remDr$navigate("https://www.ospo.noaa.gov/Products/land/hms.html#data")
Sys.sleep(4)

# Make full screen
remDr$maxWindowSize()

# Click on 1st dropdown and select smoke
remDr$findElement(using = 'xpath', value = '//*[@id="sats"]')$clickElement()
Sys.sleep(2)
remDr$findElement(using = 'xpath', "//*/option[@value = 'fire']")$clickElement()
Sys.sleep(2)

# Click on 2nd dropdown and select 
remDr$findElement(using = 'xpath', value = '//*[@id="d2"]')$clickElement()
Sys.sleep(2)
remDr$findElement(using = 'xpath', value = "//*/option[@value = 'Shapefile']")$clickElement()

# Insert date range
start_date = "01/01/2018"
end_date = "01/03/2024"

# Click and input date in from box
start_box <- remDr$findElement(using = 'xpath', value = '//*[@id="from"]')
start_box$clickElement()
input_start <- start_box$findChildElement(using = 'xpath', value = '//*[@id="from"]')
input_start$sendKeysToElement(list(start_date))
remDr$findElement(using = 'css', value = '.ui-state-active')$clickElement()

# Click and input date in to box
end_box <- remDr$findElement(using = 'xpath', value = '//*[@id="to"]')
end_box$clickElement()
input_end <- end_box$findChildElement(using = 'xpath', value = '//*[@id="to"]')
input_end$sendKeysToElement(list(end_date))
remDr$findElement(using = 'css', value = '.ui-state-active')$clickElement()

# Click in blank space to let dates "set"
remDr$findElement(using = 'xpath', value = "//html")$clickElement()
Sys.sleep(3)

# Calculate number of days of data
start_date_d <- as.Date(start_date, "%m/%d/%Y")
end_date_d <- as.Date(end_date, "%m/%d/%Y")
days <- as.numeric(end_date_d - start_date_d) + 1

# Create stop interval
stop_time <- seq(11, days, by = 10)

# Setup download directory
down_dir <- "C:/Users/bkirk/Downloads"

#create function to scroll and click
scroll_and_click <- function(remDr, element) {
  repeat {
    # Try to move to the element
    tmp <- try(remDr$mouseMoveToLocation(webElement = element))
    
    # If the element is clickable, break the loop
    if (!inherits(tmp, "try-error")) {
      break
    } else {
      # Scroll down using JavaScript
      remDr$executeScript("window.scrollBy(0, window.innerHeight);")
    }
  }
  # Click the element after it is in view
  element$clickElement()
}

# Loop over zip hrefs
for (i in 1:days) {
  # Downloads time out, and then get backed up. Force pause until download occurs
  len_downl_pre <- length(list.files(down_dir))
  
  # Track time it takes for server to respond to modify wait times on requests
  t0 <- Sys.time()
  
  # Identify link to zip file for a given day/month/year
  element <- remDr$findElement(using = "xpath", value = paste0('/html/body/main/div[1]/div/div[2]/div/section[4]/div/div[4]/p/a[', as.character(i), ']'))
  
  # Try to move cursor to the link and scroll if necessary
  scroll_and_click(remDr, element)
  
  # Pause loop adjusted by server response time
  t1 <- Sys.time()
  Sys.sleep(as.numeric(t1 - t0) * 4)
  
  # Print progress of main for loop
  print(paste0((i / days) * 100, '%'))
  print(start_date_d + i)
  
  # Calculate new download file list length
  len_down_post <- length(list.files(down_dir))
  
  # Pause loop at longer intervals on occasion to keep from freezing up or overloading server
  if (i %in% stop_time) { Sys.sleep(5) }
  if (len_down_post == len_downl_pre) { Sys.sleep(60) }
}


#quit session
rD[["server"]]$stop()

#it won't let me establish a direct download folder for the firefox remote session....
#so transfer files from downloads to objective folder

#match downloaded files in download folder
dl_folder_list <- list.files(down_dir, pattern="hms_fire")
dl_fold_list_tot <- paste0(down_dir,'/',dl_folder_list)

#copy over to main folder
file.copy(from=dl_fold_list_tot, to = dd_name('/fire_data'), 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

#remove files from downloads --> ONLY DO AFTER DOUBLE CHECKING 'fire_data' folder!!!
file.remove(dl_fold_list_tot)
