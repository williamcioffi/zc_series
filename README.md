# zc_series

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.8010306.svg)](https://doi.org/10.5281/zenodo.8010306) 

This repository contains the data and code used to produce analysis for a manuscript:

> Cioffi WR, Quick NJ, Swaim ZT, Foley HJ, Waples DM, Webster DL, Baird RW, Southall BL, Nowacek DP, Read AJ. 2023. Trade-offs in telemetry tag programming for deep-diving cetaceans: data longevity, resolution, and continuity. _Animal Biotelemetry_ 11, 23; doi: [10.1186/s40317-023-00334-1](https://doi.org/10.1186/s40317-023-00334-1)

If you have questions about anything in this repository or are interested in using the included datasets please contact wrc14 [at] duke [dot] edu. You can also post to [issues](https://github.com/williamcioffi/zc_series/issues) with questions or bugs.

# details

directories:
- `00_data` contains input data as well as several intermediate data products
- `01_helper_functions` contains helper functions used in the analysis and loaded by the included scripts.
- `02_figures` contains code to produce all figures in the manuscript.
- `03_tables` contains csv versions of the tables included in the manuscript and 1 supplementary table.

input data files:
- `00_data/tags/baseline_tags.rds` dive data from a baseline tag configuration (SPLASH10) with some concurrent time-series and dive summary records.
- `00_data/tags/series_tags_w_meta.rdata` dive data from a set of experimental deployments of SPLASH10 tags which only recorded time-series dive records.
- `00_data/tags/Tag17.csv` depth data from a SPLASH10 tag deployed in the same group as the dtag Tag18.
- `00_data/tags/Tag18_depth.mat` depth data from a DTAG deployment in the same group as the sattag Tag17.
- `00_data/gonio` includes several files detailing the reception of messages from SPLASH10 tags on various platforms using the Argos Goniometer.

intermediate data products can be found in `00_data/working` see `R01_convert_compare_divesummary_series.r`

More information on SPLASH10 Wildlife Computers satellite-linked tag can be found at the Wildlife Computers [downloads page](https://wildlifecomputers.com/support/downloads/). The SPLASH10 [manual](https://static.wildlifecomputers.com/SPLASH10-TDR10-User-Guide-3.pdf) and [file descriptions](https://static.wildlifecomputers.com/Spreadsheet-File-Descriptions-3.pdf) are good places to start.

main analysis code files (figure specific code can be found in `02_figures`):
- `R01_convert_compare_divesummary_series.r` generates the bulk of the data used for analysis and to generate figures in the manuscript. A large portion of this script is interactive to allow manual inspection and qa/qc of the conversion from time-series to dive summary records (identifying discrete dives from time-series data). Intermediate outputs found in `00_data/working` show the exact data generated and used in the present manuscript, but the code is fully operational and can be re-run by the user to generate similar output.
- `R02_comparison_summary_stats.r` calculates some basic summaries from the data.

# software versions
This code was written and initially run on `R 3.6.2`, but has been tested on `R 4.0.5` as well.

github libraries:
- [sattagutils](https://github.com/williamcioffi/sattagutils) version 0.2.1

cran libraries:
```
colorspace_1.4-1
ggplot2_3.2.1
reshape2_1.4.3
R.matlab_3.6.2
```

# citation
Please cite this repository as:

> Cioffi WR, Quick NJ, Swaim ZT, Foley HJ, Waples DM, Webster DL, Baird RW, Southall BL, Nowacek DP, Read AJ. 2022 zc_series: Dataset and code for Trade-offs in telemetry tag programming for deep-diving cetaceans: data longevity, resolution, and continuity. [https://github.com/williamcioffi/zc_series](https://github.com/williamcioffi/zc_series). doi: [10.5281/zenodo.6589596](https://doi.org/10.5281/zenodo.6589596)

Please cite the manuscript as:

> Cioffi WR, Quick NJ, Swaim ZT, Foley HJ, Waples DM, Webster DL, Baird RW, Southall BL, Nowacek DP, Read AJ. 2023. Trade-offs in telemetry tag programming for deep-diving cetaceans: data longevity, resolution, and continuity. _Animal Biotelemetry_ 11, 23; doi: [10.1186/s40317-023-00334-1](https://doi.org/10.1186/s40317-023-00334-1)

# contact information

If you have questions about anything in this repository or are interested in using the included datasets please contact wrc14 [at] duke [dot] edu.

