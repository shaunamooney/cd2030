# **Reporting Rate Module**

The Reporting Rate Module allows users to assess the completeness and quality of 
health facility reporting. It provides insights into district-level performance 
and highlights areas needing improvement.

---

## **Purpose**

This module helps users:
- Visualize district reporting rates.
- Identify districts with reporting rates below a user-defined threshold.
- Download detailed data for further analysis.

---

## **Features and Usage**

### **1. Performance Threshold**
- Enter the desired threshold value (default: `90%`) to identify districts with 
  low reporting rates.

### **2. Indicator and Year Selection**
- Choose one of the key indicators:
  - Antenatal Care (ANC)
  - Institutional Delivery
  - Vaccination.
- Select the year for which you want to analyze reporting rates.

### **3. Visualizations and Tables**
- **Reporting Rate Plot**: Displays district-level reporting rates, highlighting 
  those below the threshold.
- **Low Reporting Districts Table**: Lists districts with low reporting rates for 
  the selected indicator and year.

### **4. Data Downloads**
- **Plot**: Save the reporting rate visualization as a PNG file.
- **Summary Data**: Download an Excel file summarizing reporting rates by year
  and districts above the threshold.
- **Low Reporting Districts**: Export a detailed table of districts falling below 
  the threshold.

---

## **Troubleshooting**

| **Issue**                              | **Cause**                                         | **Solution**                                                                 |
|----------------------------------------|--------------------------------------------------|-------------------------------------------------------------------------------|
| Plot not rendering                      | Required inputs are missing                      | Ensure all inputs (indicator, year, threshold) are selected.                 |
| Empty Low Reporting Districts table    | No districts meet the low reporting criteria     | Lower the performance threshold or check data validity.                      |
| Download errors                         | Required inputs are missing                      | Ensure all inputs (year, indicator, district) are selected before downloading.|

---

## **Key Outputs**

1. **Reporting Rate Plot**:
   - A district-level visualization of reporting rates.

2. **Summary Data**:
   - Average reporting rates by year.
   - Percentage of districts achieving the threshold.

3. **Low Reporting Districts**:
   - A detailed table highlighting districts with reporting rates below the 
     threshold for the selected indicator and year.

---

## **Tips for Effective Use**

- Always ensure the selected year and indicator are valid for the dataset.
- Use the performance threshold to customize the analysis based on program requirements.
