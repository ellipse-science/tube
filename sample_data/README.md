# Sample Data for Testing ellipse_push()

This folder contains sample CSV files for testing the `ellipse_push()` function with different scenarios.

## Individual Files

### Single CSV Files
- **survey_responses.csv** - Customer satisfaction survey data (10 respondents)
- **weather_data.csv** - Weather station readings (mixed stations, hourly data)
- **sales_data.csv** - Product sales data (10 transactions)
- **employee_data.csv** - Employee information (10 employees)
- **financial_transactions.csv** - Banking transactions (10 transactions)

## Multi-File Folders (Same Schema)

### multi_weather_stations/
Contains 5 CSV files with the same weather data schema:
- **station_001_data.csv** - Station 001 hourly readings
- **station_002_data.csv** - Station 002 hourly readings  
- **station_003_data.csv** - Station 003 hourly readings
- **station_004_data.csv** - Station 004 hourly readings
- **station_005_data.csv** - Station 005 hourly readings

**Schema:** `station_id,temperature_c,humidity_percent,pressure_hpa,wind_speed_kmh,wind_direction,precipitation_mm,timestamp`

### quarterly_sales/
Contains 3 CSV files with the same sales data schema:
- **q1_2025_sales.csv** - Q1 2025 sales data
- **q2_2025_sales.csv** - Q2 2025 sales data
- **q3_2025_sales.csv** - Q3 2025 sales data

**Schema:** `product_id,product_name,category,price_cad,units_sold,revenue_cad,sale_date,region`

## Usage Examples

### Test Single File Upload
```r
# Connect to public datalake
con <- ellipse_connect(env = "DEV", database = "datalake")

# Upload single file with metadata
ellipse_push(con, 
            file_or_folder = "sample_data/survey_responses.csv",
            dataset_name = "customer-satisfaction",
            tag = "survey-2025-q3",
            metadata = list(
              title = "Customer Satisfaction Survey Q3 2025",
              author = "Marketing Team",
              survey_period = "Q3 2025"
            ),
            interactive = FALSE)
```

### Test Folder Upload (Multiple Files, Same Schema)
```r
# Upload entire folder of weather station data
ellipse_push(con,
            file_or_folder = "sample_data/multi_weather_stations",
            dataset_name = "weather-monitoring",
            tag = "stations-aug-2025",
            metadata = list(
              title = "Multi-Station Weather Data August 2025",
              data_source = "Environment Canada",
              measurement_frequency = "hourly"
            ),
            interactive = FALSE)
```

### Test Interactive Mode
```r
# Let ellipse_push prompt for missing information
ellipse_push(con, file_or_folder = "sample_data/sales_data.csv")
```

## Data Characteristics

- **Realistic data**: All sample data uses realistic Canadian contexts (CAD currency, Canadian cities, etc.)
- **Varied data types**: Includes strings, numbers, dates, timestamps, categorical data
- **Different file sizes**: From small (10 rows) to medium datasets
- **Schema consistency**: Multi-file folders maintain identical column structures for testing aggregation
- **Metadata variety**: Different types of custom metadata examples for each dataset type

This sample data is designed to thoroughly test all aspects of the `ellipse_push()` functionality including file validation, metadata handling, progress reporting, and both single-file and multi-file upload scenarios.
