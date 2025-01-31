# Data from: Approximately 15% of miscanthus yield is lost at current commercial cutting heights in Iowa
[https://doi.org/10.1002/agg2.70039](https://doi.org/10.1002/agg2.70039)

## How to perform the analysis and generate the figures
Before you can run the analysis, you will need to make sure git and conda is installed on your computer. You can download git from [here](https://git-scm.com/downloads) and conda from [here](https://docs.conda.io/en/latest/miniconda.html). Once you have installed git and conda, you can follow the steps below to run the analysis.


1. Fork the repository to your GitHub account.
2. Clone the repository to your local machine.
   1. Open a terminal.
   2. Change the current working directory to the location where you want the cloned directory.
      ```bash
      cd /path/to/directory
      ```
   3. Run the following command:
      ```bash
      git clone https://github.com/bpetersen18/cutting_height_paper.git
      ```
3. Create a conda environment using the `environment.yml` file. The will download and install the necessary packages to run the analysis.
   1. Change the current working directory to the cloned repository.
   2. Run the following command:
      ```bash
      conda env create -f environment.yml
      ``` 
4. Activate the conda environment.
   1. Run the following command:
      ```bash
      conda activate cutting_height
      ```
5. Run the Snakemake workflow to process the data and generate the figures.
   1. Run the following command:
      ```bash
      snakemake -c 1
      ```

The figures will be saved in the `visuals/` directory and the processed data will be saved in the `data/internal/` directory.

## Description of the data and file structure

This repository contains data and code related to the study on cutting height and its effects on *Miscanthus x giganteus* (*mxg*) stem mass and density. The data is organized into several directories, each containing specific types of data and scripts.

### Directory Structure

- `code/`: Contains R scripts used for data analysis and visualization.
  - `calc_stem_mass_stats.R`
  - `model_mxg_stem_linear_density.R`
  - `plot_mxg_cutting_height.R`
  - `plot_weather_data.R`
  - `read_mxg_stem.R`

- `data/`: Contains the data used in the study.
  - `external/`: Data collected from external sources.
- `environment.yml`: Conda environment file.
- `LICENSE`: License information.
- `README.md`: This file.
- `Snakefile`: Snakemake workflow file.

### Data Description
Data used in the study is stored in the `data/external` directory. Once you run the Snakemake workflow, the data will be processed and stored in the `data/internal` directory. The external data files are described below:

#### `weather/columbus_junct_data.txt`
This data was used to calculate the climatology for the Southeast Research Farm (SERF), where the Mxg field trial was conducted. Weather data collected from the "Columbus Junct 2 SSW" COOP station, which is 11.3 km NW of the SERF. The data includes daily low temperature, high temperature, and precipitation. The data was obtained from the [Iowa Environmental Mesonet](https://mesonet.agron.iastate.edu/request/coop/fe.phtml).  The data is stored in a comma-delimited text file with the following columns:

| Column Name | Units | Description |
|-------------|-------|-------------|
| station | - | Station ID |
| station_name | - | Station Name |
| lat | degrees N | Latitude |
| lon | degrees E | Longitude |
| day | YYYY/MM/DD | Date |
| doy | - | Day of year |
| high | °F | High temperature |
| highc | °C | High temperature (converted) |
| low | °F | Low temperature |
| lowc | °C | Low temperature (converted) |
| temp_estimated | - | Temperature estimated (TRUE = yes, FALSE = no) |
| precip | inches | Precipitation |
| precipmm | mm | Precipitation (converted) |
| precip_estimated | - | Precipitation estimated (TRUE = yes, FALSE = no) |

#### `weather/serf_sm_mesonet.txt`
This data was used to observe the weather conditions at the SERF during the 2020 growing season. The data was collected from the Crawfordsville - Southeast ISU-RDF ISU soil moisture network station. The data includes daily low temperature, high temperature, solar insolation, relative humidity, precipitation and wind speed. The data was obtained from the [Iowa Environmental Mesonet](https://mesonet.agron.iastate.edu/agclimate/hist/daily.php). The data is stored in a comma-delimited text file with the following columns:

| Column Name | Units | Description |
|-------------|-------|-------------|
| station | - | Station ID |
| valid | YYYY-MM-DD | Date |
| high | °F | High temperature |
| low | °F | Low temperature |
| rh_min | % | Minimum relative humidity |
| rh | % | Average relative humidity |
| rh_max | % | Maximum relative humidity |
| gdd50 | °F day^-1^ | Growing degree days (base 50°F) |
| solar_mj | MJ m^-2^ | Solar insolation |
| precip | inches | Precipitation |
| speed | mph | Average wind speed |

#### `cutting_height_experiment_obs.xlsx`
This data was used to measure the linear stem density of *mxg* in the lower 44 cm of the stem. The data was collected from a nitrogen fertilization study conducted at the SERF. Please refer to the methods section in the manuscript for more details on how the data was collected. The data is stored in an Excel file with the following columns:

| Column Name | Units | Description |
|-------------|-------|-------------|
| Farm | - | Name of the farm |
| Planting year | - | Year the *mxg* was planted |
| Block | - | Block number |
| Plot | - | Plot number |
| Nrate | kg ha^-1^ | Nitrogen rate |
| Harvest Date | DD/MM/YYYY | Date of harvest |
| Stem Count | stems m^-2^ | Number of stems in a sq. meter |
| 4 stem total dry biomass | g | Total dry biomass of 4 stems |
| 4 stem leaf dry biomass | g | Leaf dry biomass of 4 stems |
| 4 stem stem dry biomass | g | Stem dry biomass of 4 stems |
| 4cm_stem_segment | g | Dry biomass of the 0 - 4 cm 4 stem segments |
| 8cm_stem_segment | g | Dry biomass of the 4 - 8 cm 4 stem segments |
| 12cm_stem_segment | g | Dry biomass of the 8 - 12 cm 4 stem segments |
| 16cm_stem_segment | g | Dry biomass of the 12 - 16 cm 4 stem segments |
| 20cm_stem_segment | g | Dry biomass of the 16 - 20 cm 4 stem segments |
| 24cm_stem_segment | g | Dry biomass of the 20 - 24 cm 4 stem segments |
| 28cm_stem_segment | g | Dry biomass of the 24 - 28 cm 4 stem segments |
| 32cm_stem_segment | g | Dry biomass of the 28 - 32 cm 4 stem segments |
| 36cm_stem_segment | g | Dry biomass of the 32 - 36 cm 4 stem segments |
| 40cm_stem_segment | g | Dry biomass of the 36 - 40 cm 4 stem segments |
| 44cm_stem_segment | g | Dry biomass of the 40 - 44 cm 4 stem segments |

#### `cutting_height_obs.csv`
This data was used to determine the expectation and variance of cut height in commercial *mxg* fields. The data was collected from 4 commercial fields in eastern Iowa. Please refer to the methods section in the manuscript for more details on how the data was collected. The data is stored in a comma-delimited text file with the following columns:

| Column Name | Units | Description |
|-------------|-------|-------------|
| date | DD/MM/YYYY | Date of measurement |
| field_id | - | Field ID |
| sample_id | - | Sample ID |
| stem_number | - | Stem ID |
| stem_height | cm | Stem height |
| baled | - | Baled (TRUE = yes, FALSE = no) |
