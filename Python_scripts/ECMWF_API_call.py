import cdsapi

dataset = "derived-era5-single-levels-daily-statistics"


request = {
    "product_type": "reanalysis",
    "variable": [
        "10m_u_component_of_wind",
        "2m_dewpoint_temperature",
        "2m_temperature",
        "total_precipitation",
        "100m_u_component_of_wind",
        "100m_v_component_of_wind",
        "total_column_rain_water",
        "boundary_layer_height"
    ],
    "year": "2022",
    "month": [
        "01", "02", "03",
        "04", "05", "06",
        "07", "08", "09",
        "10", "11", "12"
    ],
    "day": [
        "01", "02", "03",
        "04", "05", "06",
        "07", "08", "09",
        "10", "11", "12",
        "13", "14", "15",
        "16", "17", "18",
        "19", "20", "21",
        "22", "23", "24",
        "25", "26", "27",
        "28", "29", "30",
        "31"
    ],
    "daily_statistic": "daily_mean",
    "time_zone": "utc-06:00",
    "frequency": "1_hourly",
    "area": [20.08, -99.67, 18.9, -98.5]
}

client = cdsapi.Client()
key = '1f2d7087-7924-4ec6-8204-693ba1eeb2c0'

client.retrieve(dataset, request).download()
