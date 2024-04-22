"""Code for reading and writing standardized measures."""
from pathlib import Path
import geopandas as gpd
import pandas as pd

MEASURE_DATA_DIR = Path("/Users/laurenwilner/Desktop/Desktop/epidemiology_PhD/data/clean/")


def create_measure(measure_data: pd.DataFrame | gpd.GeoDataFrame, measure_name: str) -> None:
    """Function to write out clean data

    Parameters
    ----------
    measure_data
        The data we want to write.
    measure_name
        The name of the measure represented by the data.
    """
    if isinstance(measure_data,gpd.GeoDataFrame):
        extension = "geojson"
        writer = _write_shapefile
    else:
        extension = "parquet"
        writer = _write_parquet
            
    out_path = f"{MEASURE_DATA_DIR}/{measure_name}.{extension}"
    print("Writing data to ", out_path)
    writer(measure_data, out_path)

def load_measure(measure_name: str) -> pd.DataFrame | gpd.GeoDataFrame:
    """Function to load data""" 
    shp_in_path = Path(f"{MEASURE_DATA_DIR}/{measure_name}.geojson")
    parquet_in_path = Path(f"{MEASURE_DATA_DIR}/{measure_name}.parquet")
    if shp_in_path.exists():
        data = gpd.read_file(shp_in_path)
    elif parquet_in_path.exists(): 
        data = pd.read_parquet(parquet_in_path)
    else:
        raise FileNotFoundError
    return data

    
# Helper functions to write out particular data formats.

def _write_shapefile(data: gpd.GeoDataFrame, path: str):
    data.to_file(path, driver="GeoJSON")


def _write_parquet(data: pd.DataFrame, path: str):
    data.to_parquet(path, index=False)
