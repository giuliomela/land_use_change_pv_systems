# land_use_change_pv_systems
R scripts used to prepare the manuscript "Regionalized Life Cycle Analysis of Ecosystem External Cost Associated with Land Use Change in Photovoltaic Systems" by Molocchi, Brivio, Mela and Girardi

## What the folder contains

- A "data" folder with LCA inventories, the ESVD ecosystem services values database, and two *.xlsx files containing correspondence tables to be used in the analysis
- Two *.R files to perform the analysis:
  - *script_paper_es_impact_of_pv_plant_july25.R* to perform the computations
  - *script_plots.R* to make the plots used in the manuscript
- A *.qmd file to be rendered to produce plots with the same size and resolution used in the manuscript

Instructions:

- Open the two *.R files and make sure paths to the files contained in the 'data' folder actually point to the .xlsx files
to be loaded.
- Run the script if you want to reproduce the numerical results (you can then save them in the forma you like the most)
- Open the .qmd file and render it if you want to produce a document (.docx) containing all the plots used in manuscript