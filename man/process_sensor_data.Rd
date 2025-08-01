% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_sensor_data.R
\name{process_sensor_data}
\alias{process_sensor_data}
\title{Process TEROS54 Sensor Data from Excel Files}
\usage{
process_sensor_data(
  file_path,
  sheet = "Processed Data Config 1",
  timezone = Sys.timezone(),
  verbose = FALSE,
  include_logger = FALSE
)
}
\arguments{
\item{file_path}{Character string. Path to the Excel file containing TEROS54 data.}

\item{sheet}{Character string. Name of the Excel sheet to read. Default is "Processed Data Config 1".}

\item{timezone}{Character string. Timezone for timestamp conversion. Default is system timezone.}

\item{verbose}{Logical. If TRUE, prints processing information. Default is FALSE.}

\item{include_logger}{Logical. If TRUE, includes logger data (battery, barometer) in output. Default is FALSE.}
}
\value{
A data frame with columns:
  \item{time}{POSIXct timestamp}
  \item{wc}{Volumetric water content (m³/m³) - NA for logger data}
  \item{temp}{Soil temperature (°C) or logger temperature}
  \item{depth}{Measurement depth (cm) - "logger" for system data}
  \item{sensor_id}{Sensor/port identifier (Port 1-6) or "Logger"}
  \item{battery_pct}{Battery percentage (only if include_logger = TRUE)}
  \item{battery_mv}{Battery voltage in mV (only if include_logger = TRUE)}
  \item{pressure_kpa}{Barometric pressure in kPa (only if include_logger = TRUE)}
}
\description{
Reads and processes TEROS54 soil moisture and temperature data from Excel files
with multiple sensors and depths. Converts wide format data to long format for
easier analysis and visualization. Automatically handles logger data (battery,
barometer) that is always present regardless of number of connected sensors.
}
\examples{
\dontrun{
# Process TEROS54 data file
soil_data <- process_sensor_data("path/to/teros54_data.xlsx")

# View structure
str(soil_data)

# Quick summary
summary(soil_data)
}

}
