# **Internal Consistency Module**

The Internal Consistency Checks Module is designed to assess the coherence of 
health facility data by comparing related indicators. It provides visual tools to 
identify discrepancies and ensure data reliability.

---

# Purpose

This module assists users in:

- Visualizing relationships between key health indicators to detect inconsistencies.
- Customizing comparisons between any two selected indicators.
- Downloading plots for reporting and further analysis.

# Features and Usage

## Predefined Indicator Comparisons

The module offers predefined plots for common indicator comparisons:

- **ANC1 and Penta1**: Compares the number of first antenatal care visits to the 
  first dose of the pentavalent vaccine.
- **Penta1 and Penta3**: Evaluates the drop-off between the first and third doses 
  of the pentavalent vaccine.
- **OPV1 and OPV3**: Assesses the consistency between the first and third doses
  of the oral polio vaccine.

Each comparison includes:

- A scatter plot visualizing the relationship between the two indicators.
- A download button to save the plot as a PNG file.

## Custom Indicator Comparison

Users can create custom comparisons:

- **X-axis Selection**: Choose an indicator for the x-axis from a dropdown menu.
- **Y-axis Selection**: Select an indicator for the y-axis from a dropdown menu.
- **Custom Plot**: Generates a scatter plot based on the selected indicators.
- **Download Option**: Save the custom plot as a PNG file.

# Troubleshooting

| **Issue**                             | **Cause**                                         | **Solution**                                                                 |
|---------------------------------------|---------------------------------------------------|-------------------------------------------------------------------------------|
| Plot not displaying                   | Data not loaded or selected indicators have no data | Ensure data is properly loaded and selected indicators contain data.         |
| Download button not functioning       | Missing or invalid input selections               | Verify that all required inputs are selected before attempting to download.  |
| Custom indicators not appearing       | Data does not include expected indicator names    | Check that the dataset includes the indicators intended for comparison.      |

# Key Outputs

1. **Predefined Comparison Plots**:
   - Visual representations of the relationships between predefined pairs of indicators.

2. **Custom Comparison Plot**:
   - A user-defined scatter plot comparing any two selected indicators.

3. **Downloadable Plots**:
   - PNG files of both predefined and custom plots for reporting purposes.

# Tips for Effective Use

- Ensure that the dataset includes all relevant indicators before using the module.
- Use the predefined comparisons for quick assessments of common indicator relationships.
- Utilize the custom comparison feature to explore specific relationships pertinent to your analysis.
- Regularly download and review plots to document findings and monitor data consistency over time.
