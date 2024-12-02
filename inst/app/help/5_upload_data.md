
# Guide for National Rates Setup Module

This guide provides step-by-step instructions for setting up national rates, uploading required datasets, and troubleshooting common issues. It is intended for users working with health data, focusing on data quality and coverage analysis for decision-making.

---

## **Module Overview**

The `Setup National Rates` module allows users to:
1. Configure critical national health metrics such as mortality rates and survey proportions.
2. Upload and validate data for analysis, including UN estimates, WUENIC data, and national/regional survey datasets.

---

## **Data Requirements**

Different datasets are required for various components of the analysis:

1. **UN Estimates**:
   - Used across **Denominator Assessment**, **National Coverage**, and **Subnational Coverage and Inequality**.
   
2. **WUENIC Data**:
   - Required for **National Coverage** and **Subnational Coverage**.

3. **Survey Data**:
   - **National Survey Data**: Required for **National Coverage**.
   - **Regional Survey Data**: Required for **Subnational Coverage** and **Subnational Inequality**.

---

## **Setup Steps**

### **1. Configuring National Rates**
1. **Fields Available:**
   - **Neonatal Mortality Rate:** Default value is 0.025 (range: 0 to 0.05).
   - **Post Neonatal Mortality Rate:** Default value is 0.024 (range: 0 to 0.05).
   - **Twin Rate:** Default value is 0.015 (range: 0 to 0.05).
   - **Pregnancy Loss Rate:** Default value is 0.03 (range: 0 to 0.05).
   - **Stillbirth Rate:** Default value is 0.02 (range: 0 to 0.05).
   - **ANC1 to Penta1 Mortality Rate:** Default value is 0.025 (range: 0 to 0.05).

2. **Survey Proportions:**
   - **ANC1 Survey Proportion:** Enter as a percentage (e.g., 80 for 80%).
   - **Penta1 Survey Proportion:** Enter as a percentage (e.g., 75 for 75%).

3. Adjust the values using the provided numeric inputs, ensuring consistency with the data format requirements.

---

### **2. Uploading Data**
The module supports uploading data files in `.dta` format for:
- UN Estimates
- WUENIC Data
- National Survey Data
- Regional Survey Data

#### **Instructions for Uploading:**
1. **Navigate to the Upload Survey Data Section.**
2. Select the appropriate file upload field (e.g., "Upload UN Estimates data").
3. Drag and drop your `.dta` file or click **Browse** to select a file.
4. Wait for the upload confirmation message.

#### **Expected Feedback:**
- **Success Message:** *Upload successful: File `<filename>` is ready.*
- **Error Message:** *Upload failed: Check the file format and try again.*

#### **Troubleshooting File Uploads:**
- Ensure the file is in `.dta` format. Other formats are not supported.
- Verify that the file contains the correct columns, including year, region, and indicator values.
- For UN Estimates and WUENIC data, ensure the data spans the correct time range (e.g., 2015–2023).

---

## **Data Format and Organization**

1. **Proportions vs. Percentages:**
   - Use **percentages** for survey inputs (e.g., ANC1, Penta1). Ensure values range between 0 and 100.
   - Use **proportions** for rates (e.g., mortality rates, pregnancy loss). Values should range between 0 and 0.05.

2. **National vs. Subnational Data:**
   - **National Data:** Aggregated metrics for the entire country.
   - **Subnational Data:** Regional or provincial metrics aligned with administrative boundaries.

3. **Consistency Checks:**
   - Ensure the total population figures match across national and subnational datasets.
   - Check for logical consistency, such as ratios between related indicators (e.g., ANC1 to Penta1).

---

## **Common Issues and Fixes**

### **1. Incorrect Upload File Format**
- **Problem:** Error message during file upload.
- **Solution:** Verify that the file is saved in `.dta` format. Use software like Stata or R to convert the file if needed.

### **2. Misaligned Survey Proportions**
- **Problem:** Values entered for ANC1 or Penta1 do not match survey data.
- **Solution:** Cross-check proportions with the most recent national and regional surveys.

### **3. Missing Metadata**
- **Problem:** Country-specific metadata (e.g., ISO codes) not found.
- **Solution:** Ensure that the input dataset is formatted with metadata attributes (`country` and `iso3`).

---

## **Outputs**

Upon successful setup, the module generates:
1. A reactive list containing:
   - Configured rates and proportions.
   - Uploaded and validated datasets.
2. A ready-to-use data package for further analyses, including coverage and equity assessments.

---

This guide ensures that you can seamlessly set up national rates, upload data, and address any challenges during the process.
