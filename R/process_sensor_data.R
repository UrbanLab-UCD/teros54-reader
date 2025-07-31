#' Process TEROS54 Sensor Data from Excel Files
#'
#' Reads and processes TEROS54 soil moisture and temperature data from Excel files
#' with multiple sensors and depths. Converts wide format data to long format for
#' easier analysis and visualization. Automatically handles logger data (battery,
#' barometer) that is always present regardless of number of connected sensors.
#'
#' @param file_path Character string. Path to the Excel file containing TEROS54 data.
#' @param sheet Character string. Name of the Excel sheet to read. Default is "Processed Data Config 1".
#' @param timezone Character string. Timezone for timestamp conversion. Default is system timezone.
#' @param verbose Logical. If TRUE, prints processing information. Default is FALSE.
#' @param include_logger Logical. If TRUE, includes logger data (battery, barometer) in output. Default is FALSE.
#'
#' @return A data frame with columns:
#'   \item{time}{POSIXct timestamp}
#'   \item{wc}{Volumetric water content (m³/m³) - NA for logger data}
#'   \item{temp}{Soil temperature (°C) or logger temperature}
#'   \item{depth}{Measurement depth (cm) - "logger" for system data}
#'   \item{sensor_id}{Sensor/port identifier (Port 1-6) or "Logger"}
#'   \item{battery_pct}{Battery percentage (only if include_logger = TRUE)}
#'   \item{battery_mv}{Battery voltage in mV (only if include_logger = TRUE)}
#'   \item{pressure_kpa}{Barometric pressure in kPa (only if include_logger = TRUE)}
#'
#' @examples
#' \dontrun{
#' # Process TEROS54 data file
#' soil_data <- process_sensor_data("path/to/teros54_data.xlsx")
#' 
#' # View structure
#' str(soil_data)
#' 
#' # Quick summary
#' summary(soil_data)
#' }
#'
#' @importFrom readxl read_excel
#' @export
process_sensor_data <- function(file_path, 
                                sheet = "Processed Data Config 1", 
                                timezone = Sys.timezone(),
                                verbose = FALSE,
                                include_logger = FALSE) {
  
  # Input validation
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  
  if (!grepl("\\.xlsx?$", file_path, ignore.case = TRUE)) {
    warning("File does not appear to be an Excel file (.xlsx or .xls)")
  }
  
  if (verbose) cat("Reading Excel file:", file_path, "\n")
  
  # Read the Excel file with error handling
  tryCatch({
    data <- readxl::read_excel(file_path, sheet = sheet, col_names = FALSE)
    data <- as.data.frame(data)
  }, error = function(e) {
    stop("Error reading Excel file: ", e$message)
  })
  
  if (verbose) cat("File read successfully. Dimensions:", dim(data), "\n")
  
  # Extract the variable names from the first three rows
  headers_row1 <- as.character(data[1, ])
  headers_row2 <- as.character(data[2, ])
  headers_row3 <- as.character(data[3, ])
  
  # Clean up headers (remove NA values)
  headers_row1[is.na(headers_row1)] <- ""
  headers_row2[is.na(headers_row2)] <- ""
  headers_row3[is.na(headers_row3)] <- ""
  
  # Analyze how many TEROS54 sensors were captured from header row 1
  # Filter out empty strings, system columns, and logger data (Port 7 & 8)
  valid_ports <- headers_row1[headers_row1 != "" & 
                                !grepl("^(z6-|Records:|Timestamp|Port 7|Port 8|Battery|Barometer)", headers_row1)]
  port_names <- unique(valid_ports)
  n_sensors <- length(port_names)
  
  if (verbose) cat("Detected", n_sensors, "sensors:", paste(port_names, collapse = ", "), "\n")
  
  if (n_sensors == 0) {
    stop("No valid sensor data detected in the file")
  }
  
  all_data <- list()  # Create empty list to collect data
  
  for (i in 1:n_sensors) {
    if (verbose) cat("Processing sensor", i, "of", n_sensors, "\n")
    
    # Calculate column indices (assuming 8 columns per sensor: 4 depths × 2 measurements)
    base_col <- i * 8 - 6
    
    # Check if we have enough columns
    if ((base_col + 7) > ncol(data)) {
      warning("Not enough columns for sensor ", i, ". Skipping.")
      next
    }
    
    # Extract data for different depths with error handling
    tryCatch({
      depths_data <- list()
      depth_labels <- c("15", "30", "45", "60")
      
      for (d in 1:4) {
        wc_col <- base_col + (d-1)*2
        temp_col <- base_col + (d-1)*2 + 1
        
        # Extract depth from header (more robust)
        depth_header <- headers_row3[wc_col]
        depth_value <- ifelse(is.na(depth_header) || depth_header == "", 
                              depth_labels[d], 
                              substr(depth_header, 1, 2))
        
        depth_data <- data.frame(
          time = as.POSIXct(as.numeric(data[4:nrow(data), 1]) * 86400, 
                            origin = "1899-12-30", tz = timezone),
          wc = as.numeric(data[4:nrow(data), wc_col]), 
          temp = as.numeric(data[4:nrow(data), temp_col]),
          depth = rep(depth_value, nrow(data) - 3), 
          sensor_id = rep(port_names[i], nrow(data) - 3),
          stringsAsFactors = FALSE
        )
        
        depths_data[[d]] <- depth_data
      }
      
      # Combine depths for this sensor
      sensor_data <- do.call(rbind, depths_data)
      
      # Remove rows with all NA values for measurements
      sensor_data <- sensor_data[!(is.na(sensor_data$wc) & is.na(sensor_data$temp)), ]
      
      # Append to master list
      all_data[[i]] <- sensor_data
      
    }, error = function(e) {
      warning("Error processing sensor ", i, ": ", e$message)
    })
  }
  
  # Remove any NULL elements from failed sensor processing
  all_data <- all_data[!sapply(all_data, is.null)]
  
  if (length(all_data) == 0) {
    stop("No sensor data could be processed successfully")
  }
  
  # Combine all sensor data into one long dataframe
  combined_data <- do.call(rbind, all_data)
  
  # Clean up row names
  rownames(combined_data) <- NULL
  
  # Convert depth to factor with proper levels
  combined_data$depth <- factor(combined_data$depth, levels = c("15", "30", "45", "60"))
  
  # Add data quality flags
  combined_data$wc_valid <- !is.na(combined_data$wc) & combined_data$wc >= 0 & combined_data$wc <= 1
  combined_data$temp_valid <- !is.na(combined_data$temp) & combined_data$temp > -50 & combined_data$temp < 70
  
  # Add logger data if requested
  if (include_logger) {
    if (verbose) cat("Processing logger data (battery, barometer)...\n")
    
    # Find logger data columns (typically the last 4 columns)
    # Battery %	Battery mV	Pressure kPa	Logger Temp °C
    total_cols <- ncol(data)
    
    tryCatch({
      logger_data <- data.frame(
        time = as.POSIXct(as.numeric(data[4:nrow(data), 1]) * 86400, 
                          origin = "1899-12-30", tz = timezone),
        wc = rep(NA, nrow(data) - 3),  # No water content for logger
        temp = as.numeric(data[4:nrow(data), total_cols]),  # Logger temperature
        depth = rep("logger", nrow(data) - 3),
        sensor_id = rep("Logger", nrow(data) - 3),
        battery_pct = as.numeric(data[4:nrow(data), total_cols - 3]),
        battery_mv = as.numeric(data[4:nrow(data), total_cols - 2]), 
        pressure_kpa = as.numeric(data[4:nrow(data), total_cols - 1]),
        wc_valid = rep(FALSE, nrow(data) - 3),  # No water content
        temp_valid = !is.na(as.numeric(data[4:nrow(data), total_cols])),
        stringsAsFactors = FALSE
      )
      
      # Add logger columns to sensor data (fill with NA for sensor rows)
      if (!"battery_pct" %in% names(combined_data)) {
        combined_data$battery_pct <- NA
        combined_data$battery_mv <- NA
        combined_data$pressure_kpa <- NA
      }
      
      # Combine with sensor data
      combined_data <- rbind(combined_data, logger_data)
      
    }, error = function(e) {
      warning("Could not process logger data: ", e$message)
    })
  }
  
  if (verbose) {
    cat("Processing complete. Final dataset:\n")
    cat("- Rows:", nrow(combined_data), "\n")
    cat("- Time range:", as.character(range(combined_data$time, na.rm = TRUE)), "\n")
    cat("- Sensors:", paste(unique(combined_data$sensor_id), collapse = ", "), "\n")
    cat("- Depths:", paste(levels(combined_data$depth), collapse = ", "), "cm\n")
  }
  
  return(combined_data)
}