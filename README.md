# P. rapae garden experiment

Repeating a study of field selection on the thermal sensitivity of P. rapae feeding rate

# GENERAL INFORMATION

This README.txt file was updated on September 25, 2025 by Lauren Buckley

## A. Paper associated with this archive

Citation: Taylor M. Hatcher, J. Gwen Shlichta, Joel G. Kingsolver, and Lauren B. Buckley. 2025.
Climate change induced selection on and evolution of insect thermal sensitivity

Synopsis: How have recent changes in temperature means and variability altered selection on and evolution of the thermal sensitivity of insect growth? We repeated a 1999 study of cabbage white butterflies, Pieris rapae, that linked lab measurements of the temperature sensitivity of short-term growth to fitness components (survival, development time, final body size, and fecundity) in the field. In 1999, selection favored increased growth at low temperatures at the expense of decreased growth at high temperatures. We document evolution consistent with this past selection: caterpillars now grow faster at low temperatures but slower at warm temperatures in 2024 compared to 1999. However, temperature means and extremes have shifted such that selection in 2024 has reversed to favor increased growth at the highest temperature assayed. 25 years of evolution has increased phenotypic variance at warm temperatures and strengthened a tradeoff between growth at intermediate and warm temperatures, consistent with evolutionary constraints that may restrict evolution to grow faster at rare, high temperatures. We document rapid evolution of an agricultural pest’s thermal sensitivity through climate change. However, selection to initially capitalize on warming temperatures can ultimately decrease fitness as warming temperatures move into a stressful range, driving reversals in the direction of selection.

## B. Originators

Lauren B. Buckley and Taylor Hatcher, Department of Biology, University of Washington, Seattle, WA 98195-1800, USA

## C. Contact information

Lauren Buckley.
Department of Biology, University of Washington, Seattle, WA 98195-1800, USA.
[lbuckley\@uw.edu](mailto:lbuckley@uw.edu){.email}

## D. Dates of data collection

June and July 2024.

## E. Geographic Location(s) of data collection

Seattle, WA

## F. Funding Sources

This work was supported by the National Science Foundation (IOS-2222089 to L.B.B., IOS-2222090 to J.G.K.).

# ACCESS INFORMATION

## 1. Licenses/restrictions placed on the data or code

CC0 1.0 Universal (CC0 1.0).
Public Domain Dedication

## 2. Data derived from other sources
Data associated with these publications were provided by Joel Kingsolver.

Kingsolver, J. G., Gomulkiewicz, R. & Carter, P. A. Variation, selection and evolution of function-valued traits. Genetica 112, 87–104 (2001).

Kingsolver, J. G. & Gomulkiewicz, R. Environmental variation and selection on performance curves. Integr. Comp. Biol. 43, 470–477 (2003).

Approximate dataset also published here: <https://datadryad.org/dataset/doi:10.5061/dryad.nzs7h44zz>.

## 3. Recommended citation for this data/code archive

Data: source publications above

Code: Hatcher TM et al. 2025.
Climate change induced selection on and evolution of insect thermal sensitivity. <https://github.com/HuckleyLab/PrapaeGardenExpt>.

# DATA & CODE FILE OVERVIEW

This data repository consist of 5 data files, 3 code scripts, this README document, and an archive code folder. The repository includes the following data and code filenames and variables.

Raw data is in the data folder and figures are written to the figures folder.

## Data files and variables
data/: 

PrapaeGardenExpt_WARP_TPC2023.csv: Results from 2023 experiments

PrapaeGardenExpt_WARP.csv: Experimental results

PrapaeGardenTemps_WARP.csv: Caterpillar model temperatures

TPCconstant_past.csv: Short term growth rate data from past TPC constant experiment

GHCNdata/USW00094290_2025.csv: GHCN weather station data

## Code scripts and workflow

1. TempData.R: Analyzes temperature data and creates figure 1.

2. GardenExpt2024_Fig.R: Analyzes experimental data and produces figures 2-4 and supplementary figues.

3. GardenExpt2024_TPCparental.R: Analyzes 2023 and 2024 data to assess potential parental effects and seasonal acclimation.

# SOFTWARE VERSIONS

R version 4.3.1 (2023-06-16)

Packages:
library(ggplot2) #ggplot2_3.5.0  

library(data.table) #data.table_1.14.8  

library(patchwork) #patchwork_1.2.0.9000

library(reshape2) #reshape2_1.4.4 

library(viridis) #viridis_0.6.4

library(nlme) #nlme_3.1-162

library(lme4) #lme4_1.1-34

library(ggridges) #ggridges_0.5.4

library(lubridate) #lubridate_1.9.2

library(readxl) #readxl_1.4.3

library(dplyr) #dplyr_1.1.2

library(rTPC) #rTPC_1.0.4

library(car) #car_3.1-2

library(scales) #scales_1.3.0

# REFERENCES
Kingsolver, J. G., Gomulkiewicz, R. & Carter, P. A. Variation, selection and evolution of function-valued traits. Genetica 112, 87–104 (2001).

Kingsolver, J. G. & Gomulkiewicz, R. Environmental variation and selection on performance curves. Integr. Comp. Biol. 43, 470–477 (2003).
