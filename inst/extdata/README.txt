TEROS54 Sample Data Description
===============================

File: sample_teros54_data.xlsx

This sample dataset contains soil moisture and temperature measurements from TEROS54 sensors deployed at multiple depths and locations.
The dataformat is exactly the way it is downloaded from the z6 datalogger.

Data Structure:
- Sheet 1: "Processed Data Config 1" - Main sensor readings
- Sheet 2: "Metadata" - Sensor configuration and site information

Measurement Setup:
- 6 monitoring ports occupied by TEROS54 sensors (Port 1, Port 2, Port 3,Port 4,Port 5,Port 6)
- 4 measurement depths per port: 15cm, 30cm, 45cm, 60cm
- 2 parameters per sensor:
  * Volumetric Water Content (m³/m³)
  * Soil Temperature (°C)

Temporal Coverage:
- Start: June 1, 2025, 9:45 AM
- Measurement interval: 15 minutes
- Total records: 4,321 measurements per sensor

Data Format:
- Wide format with each sensor-depth combination as separate columns
- Timestamp in M/D/YYYY H:MM format
- Headers include port number, depth, and measurement type
- First few rows contain metadata (logger ID, record count)

Usage:
This sample data can be used to test the teros54reader functions and 
understand the expected input format for your own TEROS54 data files.

Note: This is example data for demonstration purposes.