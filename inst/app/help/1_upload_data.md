### Uploading Health Facility Data (HFD)

This application enables users to upload health facility data (HFD) for analysis. 
Supported file formats include:

- **Health Facility Data**: Excel files (`.xls` and `.xlsx`)
- **Master Dataset**: Stata files (`.dta`) and R data files (`.rds`)

Simply drag and drop your file into the uploader for easy upload. The application 
is designed to facilitate quick and efficient data processing.

---

## Save Progress

You can save your work at any time by clicking the **Save Progress** button in 
the sidebar. This will create a zip file containing all relevant session data,
including input data, processed data, cached results, and a session report:

- **Input Data**
  - `input_data.rds`: The original uploaded data.
  
- **Processed Data**
  - `combined_data_table.csv`: The processed data within the app, in CSV format.
  - `data.rds`: A comprehensive data file used in the app, containing:
    - `data_list`: A list of modified data, reflecting any transformations (e.g.,
      filtering, subsetting).
    - `summary`: A summary table of the processed data.
    - `all_removed_outliers`: Rows flagged as outliers and removed during processing.

- **Cached Calculations** (`cache.zip`): A collection of cached results from 
  previous calculations, enabling faster operations when repeating tasks.

- **Session Report**
  - `report.html`: A record of all actions taken during the session, including 
    data transformations and analyses.
  - `plot.zip`: All plots generated in the session, saved as PNG or PDF files,
    with maps saved as HTML files.

- **Error Log**
  - `error_log.txt`: A log of any errors encountered during the session, for 
    debugging and troubleshooting.

### DPI Control for Plots

You can adjust the DPI (dots per inch) of saved plot images to control their
resolution. Resize the application window to adjust the plot dimensions.

---

## Restore Progress

If you previously saved your session as a zip file, you can restore it by using
the **Restore Progress** feature on the import page. This will reload the input 
data, processed data, and cached results, allowing you to resume your analysis
from where you left off.

- **Limitations**: The restore feature will reload input data and cached results
  but cannot fully replicate every step of the workflow (e.g., table selections or
  interactive plot choices). Workflow stages that require user interactions cannot
  be reproduced automatically.
  
- **App Version Compatibility**: Cached results are only compatible with the exact 
  app version they were created in. Data saved with older versions of the app may
  not work with future versions.
