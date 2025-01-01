# **Outlier Detection Module**

The Outlier Detection Module enables users to identify, visualize, and analyze 
extreme outliers in health facility data. The module provides tools to detect 
outliers at both the district and indicator levels, with options for trend 
analysis and data download.

---

## **Purpose**

This module helps users:
- Detect outliers in health facility indicators across districts and years.
- Analyze trends in indicators to identify patterns or anomalies.
- Export summaries and detailed information for further analysis.

---

## **Features and Usage**

### **1. Outlier Detection**
- Detect extreme outliers using a 5-standard deviation rule for selected health indicators.
- Provides a summary of districts and indicators with outliers.

### **2. Year, Indicator, and District Selection**
- Choose the year to filter data.
- Select a specific indicator (e.g., vaccination coverage).
- Narrow down analysis by selecting a district for trend visualization.

### **3. Visualizations**
- **Outlier Heatmap**: Displays the number of outliers by indicator and district.
- **District Trends**: Visualize trends for a selected indicator within a specific district.

### **4. Data Downloads**
- **Outlier Summary**: Export data summarizing outliers by year and district.
- **District Outliers**: Download a detailed table of districts with extreme outliers.
- **District Trends**: Save district-level trend data for further exploration.

---

## **Outlier Detection Calculations**

Outlier detection is performed using a **5-standard deviation rule**. The process involves:

1. **Indicator Selection**:
   - Select relevant indicators for analysis (e.g., vaccination rates).

2. **Calculate Median and MAD**:
   - **Median**: The middle value of the dataset for an indicator.
   - **MAD (Median Absolute Deviation)**: A robust measure of variability.

3. **Define Outlier Thresholds**:
   - **Upper Bound**: `Median + 5 × MAD`
   - **Lower Bound**: `Median - 5 × MAD`

4. **Flag Outliers**:
   - Values outside the upper and lower bounds are flagged as outliers.

---

## **Outputs**

### **1. Heatmap**
- A heatmap visualization of outlier counts by indicator and district.
- Highlights areas with high occurrences of extreme values.

### **2. Outlier Summary Table**
- Lists districts with extreme outliers for the selected year and indicator.
- Interactive table with filtering and sorting options.

### **3. District Trend Plot**
- Displays monthly trends for selected indicators in a chosen district.
- Includes median and deviation boundaries to help visualize anomalies.

---

## **Troubleshooting**

| **Issue**                              | **Cause**                                         | **Solution**                                                                 |
|----------------------------------------|--------------------------------------------------|-------------------------------------------------------------------------------|
| "No data available for the selected year" | The selected year is not present in the dataset | Ensure the year is available in the uploaded dataset.                        |
| Heatmap not rendering                   | Missing year or dataset is incomplete            | Select a valid year or verify the dataset contains relevant data.            |
| District Trends not displaying          | Selected district has no data for the indicator | Check that the district and indicator are valid selections.                  |
| Download errors                         | Required inputs are missing                      | Ensure all inputs (year, indicator, district) are selected before downloading.|

---

## **Common Use Cases**

1. **Outlier Analysis**:
   - Identify districts and indicators with extreme values that may indicate data quality issues.

2. **Trend Monitoring**:
   - Visualize trends in indicators to detect sudden spikes or drops in reported values.

3. **Data Export for Reporting**:
   - Export summaries and detailed district-level outlier data for external analysis.

---

## **Tips for Effective Use**

- Ensure the dataset is complete and includes all necessary indicators and years.
- Use the heatmap to quickly identify areas with frequent outliers.
- Focus on trends in districts with consistent reporting to ensure accurate analysis.
