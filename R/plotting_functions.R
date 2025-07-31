#' Plot TEROS54 Water Content Time Series
#'
#' Creates a time series plot of soil water content measurements from TEROS54 sensors.
#' Each sensor is displayed in a separate facet with different line types for each depth.
#'
#' @param data Data frame from process_sensor_data() function
#' @param title Character string. Plot title. Default is "TEROS54 Sensor Data"
#' @param sensors Character vector. Specific sensors to plot. Default is all sensors
#' @param date_range Date vector of length 2. Date range to plot. Default is full range
#' @param ncol Integer. Number of columns for facet arrangement. Default is 1
#' @param use_okabeito Logical. Use colorblind-friendly Okabe-Ito colors. Default is TRUE
#' @param line_size Numeric. Line thickness. Default is 1
#'
#' @return A ggplot2 object
#'
#' @examples
#' \dontrun{
#' # Basic plot
#' data <- process_sensor_data("teros54_data.xlsx")
#' plot_water_content(data)
#' 
#' # Customize plot
#' plot_water_content(data, 
#'                   title = "My Site Water Content",
#'                   sensors = c("Port 1", "Port 2"),
#'                   ncol = 2)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_line labs theme_minimal theme element_blank facet_wrap
#' @importFrom see scale_color_okabeito
#' @export
plot_water_content <- function(data, 
                               title = "TEROS54 Sensor Data",
                               sensors = NULL,
                               date_range = NULL,
                               ncol = 1,
                               use_okabeito = TRUE,
                               line_size = 1) {
  
  # Input validation
  required_cols <- c("time", "wc", "sensor_id", "depth")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Filter data if requested
  plot_data <- data
  
  # Filter by sensors
  if (!is.null(sensors)) {
    plot_data <- plot_data[plot_data$sensor_id %in% sensors, ]
    if (nrow(plot_data) == 0) {
      stop("No data found for specified sensors: ", paste(sensors, collapse = ", "))
    }
  }
  
  # Filter by date range
  if (!is.null(date_range)) {
    if (length(date_range) != 2) {
      stop("date_range must be a vector of length 2")
    }
    plot_data <- plot_data[plot_data$time >= date_range[1] & plot_data$time <= date_range[2], ]
  }
  
  # Remove rows with missing water content
  plot_data <- plot_data[!is.na(plot_data$wc), ]
  
  if (nrow(plot_data) == 0) {
    stop("No valid water content data to plot")
  }
  
  # Create base plot
  p <- ggplot2::ggplot(data = plot_data, 
                       ggplot2::aes(x = time, y = wc, color = sensor_id)) + 
    ggplot2::geom_line(ggplot2::aes(linetype = depth), size = line_size) +
    ggplot2::labs(
      title = title,
      x = "Time",
      y = expression("Water content"~(cm^3~cm^{-3}))
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_blank()
    ) +
    ggplot2::facet_wrap(~ sensor_id, ncol = ncol)
  
  # Add color scale
  if (use_okabeito && requireNamespace("see", quietly = TRUE)) {
    p <- p + see::scale_color_okabeito(
      palette = "full",
      reverse = FALSE,
      order = 1:9,
      aesthetics = "color"
    )
  }
  
  return(p)
}

#' Find Times Near Specific Hours
#'
#' Helper function to find measurement times close to specific hours of the day.
#' Useful for selecting consistent time points across multiple days.
#'
#' @param data Data frame with time column
#' @param hours Numeric vector of hours (0-23). Default is c(6, 12, 18) for morning, noon, evening
#' @param max_days Integer. Maximum number of days to search. Default is 7
#' @param tolerance_minutes Numeric. How close to target hour (in minutes). Default is 30
#'
#' @return POSIXct vector of matching times
#'
#' @examples
#' \dontrun{
#' data <- process_sensor_data("teros54_data.xlsx")
#' 
#' # Find noon measurements
#' noon_times <- find_times_near_hours(data, hours = 12)
#' plot_soil_profile(data, time_points = noon_times)
#' 
#' # Find morning, noon, evening measurements
#' daily_times <- find_times_near_hours(data, hours = c(6, 12, 18))
#' plot_soil_profile(data, time_points = daily_times)
#' }
#'
#' @export
find_times_near_hours <- function(data, 
                                  hours = c(6, 12, 18), 
                                  max_days = 7,
                                  tolerance_minutes = 30) {
  
  if (!"time" %in% names(data)) {
    stop("Data must contain a 'time' column")
  }
  
  # Get unique times and sort
  unique_times <- sort(unique(data$time), decreasing = TRUE)
  
  # Get the date range to search
  latest_date <- as.Date(max(unique_times))
  earliest_date <- latest_date - max_days + 1
  
  matched_times <- c()
  
  # For each target hour
  for (target_hour in hours) {
    # For each day in range
    for (day_offset in 0:(max_days-1)) {
      search_date <- latest_date - day_offset
      
      # Create target datetime
      target_time <- as.POSIXct(paste(search_date, sprintf("%02d:00:00", target_hour)))
      
      # Find closest measurement time
      time_diffs <- abs(as.numeric(unique_times - target_time))
      closest_idx <- which.min(time_diffs)
      
      if (length(closest_idx) > 0) {
        closest_time <- unique_times[closest_idx]
        # Check if within tolerance
        if (time_diffs[closest_idx] <= tolerance_minutes * 60) {
          matched_times <- c(matched_times, closest_time)
        }
      }
    }
  }
  
  # Remove duplicates and sort
  matched_times <- sort(unique(matched_times), decreasing = TRUE)
  
  return(as.POSIXct(matched_times, origin = "1970-01-01"))
}

#' Plot TEROS54 Temperature Time Series
#'
#' Creates a time series plot of soil temperature measurements from TEROS54 sensors.
#' Each sensor is displayed in a separate facet with different line types for each depth.
#'
#' @param data Data frame from process_sensor_data() function
#' @param title Character string. Plot title. Default is "TEROS54 Temperature Data"
#' @param sensors Character vector. Specific sensors to plot. Default is all sensors
#' @param date_range Date vector of length 2. Date range to plot. Default is full range
#' @param ncol Integer. Number of columns for facet arrangement. Default is 1
#' @param use_okabeito Logical. Use colorblind-friendly Okabe-Ito colors. Default is TRUE
#' @param line_size Numeric. Line thickness. Default is 1
#'
#' @return A ggplot2 object
#'
#' @examples
#' \dontrun{
#' # Basic temperature plot
#' data <- process_sensor_data("teros54_data.xlsx")
#' plot_temperature(data)
#' 
#' # Compare specific sensors
#' plot_temperature(data, 
#'                 sensors = c("Port 1", "Port 3"),
#'                 ncol = 2)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_line labs theme_minimal theme element_blank facet_wrap
#' @importFrom see scale_color_okabeito
#' @export
plot_temperature <- function(data, 
                             title = "TEROS54 Temperature Data",
                             sensors = NULL,
                             date_range = NULL,
                             ncol = 1,
                             use_okabeito = TRUE,
                             line_size = 1) {
  
  # Input validation
  required_cols <- c("time", "temp", "sensor_id", "depth")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Filter data if requested
  plot_data <- data
  
  # Filter by sensors
  if (!is.null(sensors)) {
    plot_data <- plot_data[plot_data$sensor_id %in% sensors, ]
    if (nrow(plot_data) == 0) {
      stop("No data found for specified sensors: ", paste(sensors, collapse = ", "))
    }
  }
  
  # Filter by date range
  if (!is.null(date_range)) {
    if (length(date_range) != 2) {
      stop("date_range must be a vector of length 2")
    }
    plot_data <- plot_data[plot_data$time >= date_range[1] & plot_data$time <= date_range[2], ]
  }
  
  # Remove rows with missing temperature
  plot_data <- plot_data[!is.na(plot_data$temp), ]
  
  if (nrow(plot_data) == 0) {
    stop("No valid temperature data to plot")
  }
  
  # Create base plot
  p <- ggplot2::ggplot(data = plot_data, 
                       ggplot2::aes(x = time, y = temp, color = sensor_id)) + 
    ggplot2::geom_line(ggplot2::aes(linetype = depth), size = line_size) +
    ggplot2::labs(
      title = title,
      x = "Time",
      y = "Temperature (°C)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_blank()
    ) +
    ggplot2::facet_wrap(~ sensor_id, ncol = ncol)
  
  # Add color scale
  if (use_okabeito && requireNamespace("see", quietly = TRUE)) {
    p <- p + see::scale_color_okabeito(
      palette = "full",
      reverse = FALSE,
      order = 1:9,
      aesthetics = "color"
    )
  }
  
  return(p)
}

#' Quick Plot of TEROS54 Data
#'
#' Convenience function to quickly plot both water content and temperature
#' from a TEROS54 data file in one command.
#'
#' @param file_path Character string. Path to Excel file
#' @param plot_type Character string. Either "water", "temperature", or "both". Default is "both"
#' @param title_prefix Character string. Prefix for plot titles. Default uses filename
#' @param ... Additional arguments passed to plotting functions
#'
#' @return A ggplot2 object or list of ggplot2 objects if plot_type = "both"
#'
#' @examples
#' \dontrun{
#' # Quick plot from file
#' quick_plot_teros54("my_data.xlsx")
#' 
#' # Just water content
#' quick_plot_teros54("my_data.xlsx", plot_type = "water")
#' 
#' # Custom title and sensors
#' quick_plot_teros54("my_data.xlsx", 
#'                   title_prefix = "Site A",
#'                   sensors = c("Port 1", "Port 2"))
#' }
#'
#' @export
quick_plot_teros54 <- function(file_path, 
                               plot_type = "both",
                               title_prefix = NULL,
                               ...) {
  
  # Process the data
  data <- process_sensor_data(file_path)
  
  # Generate title prefix from filename if not provided
  if (is.null(title_prefix)) {
    title_prefix <- tools::file_path_sans_ext(basename(file_path))
  }
  
  # Create plots based on type
  if (plot_type == "water") {
    return(plot_water_content(data, 
                              title = paste(title_prefix, "- Water Content"),
                              ...))
  } else if (plot_type == "temperature") {
    return(plot_temperature(data, 
                            title = paste(title_prefix, "- Temperature"),
                            ...))
  } else if (plot_type == "both") {
    water_plot <- plot_water_content(data, 
                                     title = paste(title_prefix, "- Water Content"),
                                     ...)
    temp_plot <- plot_temperature(data, 
                                  title = paste(title_prefix, "- Temperature"),
                                  ...)
    return(list(water_content = water_plot, temperature = temp_plot))
  } else {
    stop("plot_type must be 'water', 'temperature', or 'both'")
  }
}

#' Plot TEROS54 Soil Profile
#'
#' Creates soil profile plots showing water content or temperature by depth
#' at specific time points. Useful for visualizing vertical soil gradients.
#'
#' @param data Data frame from process_sensor_data() function
#' @param plot_var Character string. Variable to plot: "wc" for water content or "temp" for temperature
#' @param time_points POSIXct vector. Specific times to plot. If NULL, uses recent data points
#' @param sensors Character vector. Specific sensors to plot. Default is all sensors
#' @param n_times Integer. Number of recent time points to plot if time_points is NULL. Default is 4
#' @param point_size Numeric. Size of points. Default is 3
#' @param line_size Numeric. Line thickness connecting depths. Default is 1
#' @param use_okabeito Logical. Use colorblind-friendly colors. Default is TRUE
#'
#' @return A ggplot2 object
#'
#' @examples
#' \dontrun{
#' # Water content profile
#' data <- process_sensor_data("teros54_data.xlsx")
#' plot_soil_profile(data, plot_var = "wc")
#' 
#' # Temperature profile for specific times
#' specific_times <- as.POSIXct(c("2025-06-01 12:00:00", "2025-06-01 18:00:00"))
#' plot_soil_profile(data, plot_var = "temp", time_points = specific_times)
#' 
#' # Single sensor profile
#' plot_soil_profile(data, plot_var = "wc", sensors = "Port 1")
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_line labs theme_minimal theme element_blank facet_wrap scale_y_reverse
#' @importFrom see scale_color_okabeito
#' @export
plot_soil_profile <- function(data, 
                              plot_var = "wc",
                              time_points = NULL,
                              sensors = NULL,
                              n_times = 4,
                              point_size = 3,
                              line_size = 1,
                              use_okabeito = TRUE) {
  
  # Input validation
  if (!plot_var %in% c("wc", "temp")) {
    stop("plot_var must be 'wc' or 'temp'")
  }
  
  required_cols <- c("time", plot_var, "sensor_id", "depth")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Filter data
  plot_data <- data
  
  # Filter by sensors
  if (!is.null(sensors)) {
    plot_data <- plot_data[plot_data$sensor_id %in% sensors, ]
    if (nrow(plot_data) == 0) {
      stop("No data found for specified sensors: ", paste(sensors, collapse = ", "))
    }
  }
  
  # Convert depth to numeric for plotting
  plot_data$depth_numeric <- as.numeric(as.character(plot_data$depth))
  
  # Remove rows with missing values or invalid depths
  plot_data <- plot_data[!is.na(plot_data[[plot_var]]) & !is.na(plot_data$depth_numeric), ]
  
  if (nrow(plot_data) == 0) {
    stop("No valid data to plot")
  }
  
  # Select time points
  if (is.null(time_points)) {
    # Get the most recent n_times time points
    unique_times <- sort(unique(plot_data$time), decreasing = TRUE)
    time_points <- head(unique_times, n_times)
    # Filter to selected time points
    plot_data <- plot_data[plot_data$time %in% time_points, ]
  } else {
    # For user-specified time points, find closest matches
    matched_data <- data.frame()
    for (target_time in time_points) {
      # Find closest time within 30 minutes
      time_diffs <- abs(as.numeric(plot_data$time - target_time))
      closest_idx <- which.min(time_diffs)
      
      if (length(closest_idx) > 0 && time_diffs[closest_idx] <= 1800) {  # 30 minutes = 1800 seconds
        closest_time <- plot_data$time[closest_idx]
        matched_data <- rbind(matched_data, plot_data[plot_data$time == closest_time, ])
      }
    }
    plot_data <- matched_data
  }
  
  if (nrow(plot_data) == 0) {
    stop("No data found for specified time points")
  }
  
  # Create time labels for legend
  plot_data$time_label <- format(plot_data$time, "%m/%d %H:%M")
  
  # Set up labels and titles
  if (plot_var == "wc") {
    y_label <- expression("Water content"~(cm^3~cm^{-3}))
    title <- "Soil Water Content Profile"
  } else {
    y_label <- "Temperature (°C)"
    title <- "Soil Temperature Profile"
  }
  
  # Create the plot
  p <- ggplot2::ggplot(data = plot_data, 
                       ggplot2::aes(x = .data[[plot_var]], y = depth_numeric, 
                                    color = time_label, group = interaction(sensor_id, time_label))) +
    ggplot2::geom_point(size = point_size) +
    ggplot2::geom_line(size = line_size) +
    ggplot2::scale_y_reverse(breaks = c(15, 30, 45, 60)) +
    ggplot2::labs(
      title = title,
      x = y_label,
      y = "Depth (cm)",
      color = "Time"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom"
    )
  
  # Add faceting if multiple sensors
  if (length(unique(plot_data$sensor_id)) > 1) {
    p <- p + ggplot2::facet_wrap(~ sensor_id)
  }
  
  # Add color scale
  if (use_okabeito && requireNamespace("see", quietly = TRUE)) {
    p <- p + see::scale_color_okabeito(
      palette = "full",
      reverse = FALSE,
      aesthetics = "color"
    )
  }
  
  return(p)
}

#' Compare Sensors Side by Side
#'
#' Creates comparison plots showing the same measurement across different sensors
#' in separate panels for easy comparison.
#'
#' @param data Data frame from process_sensor_data() function
#' @param plot_var Character string. Variable to plot: "wc" for water content or "temp" for temperature
#' @param depths Character vector. Specific depths to plot. Default is all depths
#' @param date_range Date vector of length 2. Date range to plot. Default is full range
#' @param ncol Integer. Number of columns for facet arrangement. Default is 2
#' @param line_size Numeric. Line thickness. Default is 0.8
#' @param use_okabeito Logical. Use colorblind-friendly colors. Default is TRUE
#'
#' @return A ggplot2 object
#'
#' @examples
#' \dontrun{
#' # Compare water content across sensors
#' data <- process_sensor_data("teros54_data.xlsx")
#' compare_sensors(data, plot_var = "wc")
#' 
#' # Compare only surface measurements
#' compare_sensors(data, plot_var = "temp", depths = c("15", "30"))
#' 
#' # Compare recent data
#' recent_range <- c(Sys.Date() - 7, Sys.Date())
#' compare_sensors(data, plot_var = "wc", date_range = recent_range)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_line labs theme_minimal theme facet_wrap
#' @importFrom see scale_color_okabeito
#' @export
compare_sensors <- function(data,
                            plot_var = "wc",
                            depths = NULL,
                            date_range = NULL,
                            ncol = 2,
                            line_size = 0.8,
                            use_okabeito = TRUE) {
  
  # Input validation
  if (!plot_var %in% c("wc", "temp")) {
    stop("plot_var must be 'wc' or 'temp'")
  }
  
  required_cols <- c("time", plot_var, "sensor_id", "depth")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Filter data
  plot_data <- data
  
  # Filter by depths
  if (!is.null(depths)) {
    plot_data <- plot_data[plot_data$depth %in% depths, ]
    if (nrow(plot_data) == 0) {
      stop("No data found for specified depths: ", paste(depths, collapse = ", "))
    }
  }
  
  # Filter by date range
  if (!is.null(date_range)) {
    if (length(date_range) != 2) {
      stop("date_range must be a vector of length 2")
    }
    plot_data <- plot_data[plot_data$time >= date_range[1] & plot_data$time <= date_range[2], ]
  }
  
  # Remove rows with missing values
  plot_data <- plot_data[!is.na(plot_data[[plot_var]]), ]
  
  if (nrow(plot_data) == 0) {
    stop("No valid data to plot")
  }
  
  # Set up labels and titles
  if (plot_var == "wc") {
    y_label <- expression("Water content"~(cm^3~cm^{-3}))
    title <- "Water Content Comparison Across Sensors"
  } else {
    y_label <- "Temperature (°C)"
    title <- "Temperature Comparison Across Sensors"
  }
  
  # Create the plot
  p <- ggplot2::ggplot(data = plot_data, 
                       ggplot2::aes(x = time, y = .data[[plot_var]], color = depth)) +
    ggplot2::geom_line(size = line_size) +
    ggplot2::labs(
      title = title,
      x = "Time",
      y = y_label,
      color = "Depth (cm)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom"
    ) +
    ggplot2::facet_wrap(~ sensor_id, ncol = ncol, scales = "free_y")
  
  # Add color scale
  if (use_okabeito && requireNamespace("see", quietly = TRUE)) {
    p <- p + see::scale_color_okabeito(
      palette = "full",
      reverse = FALSE,
      aesthetics = "color"
    )
  }
  
  return(p)
}