#-------------------------------------------------------------------------------
#
# '01_safegraph_api_call.R': this script downloads all Safegraph Spend data from 
# Dewey
#
#-------------------------------------------------------------------------------

#load relevant packages
library(deweydatar)

#api urls and key
apikey_ <- readLines("secrets/dewey_api_key.txt")
pp_safegraph_wp <- "https://api.deweydata.io/api/v1/external/data/prj_ody4bti6__fldr_fhx7b4q9kudjwmk4r"

#modify dewey's broken get_file_list function
get_file_list_fixed <- function(apikey, product_path, start_page = 1, end_page = Inf, 
                                start_date = NULL, end_date = NULL, meta = NULL, print_info = T) 
{
  product_path = make_api_endpoint(product_path)
  if (is.null(meta)) {
    meta = get_meta(apikey, product_path, print_meta = F)
  }
  selection_meta = NULL
  pages_meta = NULL
  files_df = NULL
  if (is.null(start_date)) {
    start_date = "1000-01-01"
  }
  if (is.null(end_date)) {
    end_date = "9999-12-31"
  }
  start_date = as.character(as.Date(start_date))
  end_date = as.character(as.Date(end_date))
  page = start_page
  while (T) {
    req = request(product_path) %>% req_headers(`X-API-KEY` = apikey) %>% 
      req_headers(accept = "application/json") %>% req_url_query(page = page)
    if (!is.null(meta$partition_column)) {
      req = req %>% req_url_query(partition_key_after = start_date) %>% 
        req_url_query(partition_key_before = end_date)
    }
    response = tryCatch({
      req_perform(req)
    }, warning = function(cond) {
      message("Warning during httr2::req_perform.")
      message(cond)
      message("")
    }, error = function(cond) {
      message("Error during httr2::req_perform.")
      message(cond)
      message("")
    })
    if (is.null(response)) {
      return(NULL)
    }
    else if (response$status_code == 401) {
      message(response)
      return(NULL)
    }
    else if (response$status_code == 422) {
      message(response)
      return(NULL)
    }
    res_json = resp_body_json(response)
    if (is.null(res_json$page)) {
      message("Error in resp_body_json")
      message(res_json)
      message(" ")
      return(NULL)
    }
    if (res_json$page == start_page) {
      selection_meta = data.frame(total_files = res_json$total_files, 
                                  total_pages = res_json$total_pages, total_size_MB = res_json$total_size/1000000, 
                                  expires_at = res_json$expires_at)
    }
    if (print_info == T) {
      message(paste0("Collecting files information for page ", 
                     res_json$page, "/", res_json$total_pages, "..."))
    }
    if (!is.null(res_json$partition_column)) {
      partition_column = res_json$partition_column
    }
    else {
      partition_column = NA
    }
    pages_meta = rbind(pages_meta, data.frame(page = res_json$page, 
                                              number_of_files_for_page = res_json$number_of_files_for_page, 
                                              avg_file_size_for_page = res_json$avg_file_size_for_page/1000000, 
                                              partition_column = partition_column))
    col_names_str = names(res_json$download_links[[1]])
    dn_link_unlist = unlist(res_json$download_links)
    if (!is.null(res_json$partition_column)) {
      dn_link_df = data.frame(matrix(dn_link_unlist, ncol = length(col_names_str), 
                                     byrow = T))
    }
    else {
      dn_link_df = data.frame(matrix(dn_link_unlist, ncol = length(col_names_str) - 
                                       1, byrow = T))
      dn_link_df = data.frame(dn_link_df[, 1], NA, dn_link_df[2:ncol(dn_link_df)])
    }
    colnames(dn_link_df) = col_names_str
    page_files_df = data.frame(page = res_json$page, dn_link_df)
    files_df = rbind(files_df, page_files_df)
    page = res_json$page + 1
    if ((page > res_json$total_pages) | (page > end_page)) {
      if (print_info == T) {
        message("Files information collection completed.")
      }
      break
    }
  }
  
  # PATCHED LINE: handle "YYYY-MM" format by appending "-01"
  files_df$partition_key = as.Date(paste0(files_df$partition_key, "-01"), 
                                   format = "%Y-%m-%d")
  
  files_df = data.frame(index = 1:nrow(files_df), files_df)
  files_df$download_link = files_df$link
  if (print_info == T) {
    message(" ")
    print_selection_meta(selection_meta, pages_meta)
    message(" ")
  }
  attr(files_df, "selection_meta") = selection_meta
  attr(files_df, "pages_meta") = pages_meta
  return(files_df)
}

#check out file contents
files_df <- get_file_list_fixed(apikey = apikey_, pp_safegraph_wp,
                                      start_date = "2019-01-01", end_date = "2022-12-31",
                                      print_info = T)

#view metadata
meta_data <- deweydatar::get_meta(apikey_, pp_safegraph_wp)

#create alternative download function that gives updates and includes api call pauses
download_files_mod <- function(files_df, dest_folder, filename_prefix = NULL, skip_exists = FALSE) {
  dest_folder = gsub("\\", "/", dest_folder, fixed = TRUE)
  if (!endsWith(dest_folder, "/")) {
    dest_folder = paste0(dest_folder, "/")
  }
  total_pages = attr(files_df, "selection_meta")$total_pages[1]
  num_files = nrow(files_df)
  
  for (i in 1:num_files) {
    message(paste0("Downloading file ", i, "/", num_files, 
                   " (file index = ", files_df$index[i], ")", 
                   ", page ", files_df$page[i], "/", total_pages))
    file_name = paste0(filename_prefix, files_df$file_name[i])
    dest_path = paste0(dest_folder, file_name)
    
    if (file.exists(dest_path) && skip_exists) {
      message(paste0("File already exists: ", dest_path))
      message("Skipping...")
      next
    }
    
    message(paste0("Writing ", dest_path))

    req = request(files_df$link[i]) %>% req_retry(max_tries=10, backoff = ~10)
    response = req_perform(req)
    
    file_con = file(dest_path, "wb")
    writeBin(response$body, file_con)
    close(file_con)
    
    message("   ")
    
    # Introduce a delay to stay under 2 API hits per second
    Sys.sleep(0.2)  # 0.6 seconds delay
  }
}

download_files_mod(files_df, "./data/safegraph_data/", skip_exists = T)






