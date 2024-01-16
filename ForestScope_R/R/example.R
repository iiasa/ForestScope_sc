# Introduction to the IIASAICP Package
#
# The IIASAICP package provides functions for processing and analyzing soil, climate, and stand management data related to the International Co-operative Programme (ICP) on the Assessment and Monitoring of Air Pollution Effects on Forests.
#
# Main Functions:
# 1. isric_soil_data: This function processes ISRIC soil data and extracts information specific to ICP sites. It allows users to retrieve soil characteristics and properties for the selected sites.
#
# 2. ICP_climate_soil: This function processes ICP climate, soil, stand, and management parameters. It provides a comprehensive overview of the various environmental factors affecting ICP sites and helps users understand the relationships between these factors.
#
# 3. IIASA_ICP_process: This function acts as a wrapper, calling the aforementioned functions (isric_soil_data and ICP_climate_soil) and inquiring about the site to be processed. It streamlines the data processing and analysis workflow for users working with ICP data.
#
# To get started with the IIASAICP package, load the package, and follow the examples provided in the documentation for each function. Explore the available data and customize your analysis based on your research interests and objectives.
#
# Example of Data Extraction:
#
# data_output <- IIASA_ICP_process(isric_soil_d, icp_data, site_number)
# This will execute all the required functions and ask for the site selection, returning the processed data for the selected site.
