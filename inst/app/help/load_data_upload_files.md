# Upload Data Help Guide

This guide explains how to structure and upload data into the app, ensuring 
compatibility with the Countdown Health Facility Data Format.

---

## **Purpose of the Upload Section**

The `Upload Data` module enables users to:
- Upload health facility data structured according to the **Countdown Health Facility Data Format**.
- Validate the uploaded file for compatibility with the required template format.
- Prepare data for subsequent analyses within the app.

---

## **Supported File Formats**

The app supports uploading the following file types:
- **Health Facility Data**: Excel files (`.xls`, `.xlsx`)
- **Master Dataset**: Stata files (``.dta`)
- **Cached Data**: R Data Files(`.rds`)

---

## **How to Upload Data**

### **Step 1: Prepare Your Data File**

Ensure your data is cleaned and structured according to the **Countdown Health Facility Data Format** by:
1. Following the **HFD Guidelines**.
2. Using the provided **HFD Standardized Template** to format your data correctly.
3. Saving the file in a supported format: `.xls`, `.xlsx`, `.dta`, or `.rds`.

---

### **Step 2: Upload the File**

1. Navigate to the **Upload Data** section of the app.
2. Drag and drop your file into the upload box, or click **Browse** to select it manually.
3. The app will validate your file against the Countdown Health Facility Data Format.

   - If successful, a confirmation message will appear: *"Upload successful: Your file is ready for analysis."*
   - If errors are detected, an error message will indicate the issue.

---

## **Common Errors and How to Fix Them**

| **Error Message**                                                  | **Cause**                                                                 | **Solution**                                                                 |
|--------------------------------------------------------------------|---------------------------------------------------------------------------|-------------------------------------------------------------------------------|
| "Unsupported file format"                                          | File type not supported                                                   | Save your file as `.xls`, `.xlsx`, `.dta`, or `.rds`.                         |
| "The following required columns are missing from the data: opv1"   | Missing essential columns in the data                                    | Add the missing column(s) to your dataset and ensure their values are valid. |
| "The following sheets are missing: `Service_data_1`, `Service_data_2`, `Service_data_3`, `Reporting_completeness`, `Population_data`, `Admin_data`" | Missing one or more required sheets in the file                          | Add the missing sheets to your file and ensure they conform to the template. |
| "Sheet `Service_data_3` is empty"                                  | The sheet exists but contains no data                                    | Populate the sheet with valid data or remove the empty sheet.                |
| "Key Columns \"month\" missing in Service_data_3"                  | A key column `district`, `year`, or `month`,  is missing from the specified sheet                    | Add the missing column(s) to the sheet and ensure the data is structured correctly. |
| "Column name `month` must not be duplicated. Use `.name_repair` to specify repair." | Duplicate column names in the dataset                                   | Ensure all column names are unique. Rename or remove duplicate columns.      |

> **Note**: If a key column (`district`, `year`, or `month`) is missing data in a row, that row will be excluded from the resulting dataset.

---
   
## **Validation Process**

The app performs the following validation steps on your uploaded file:

### **1. File Format Check**
- Confirms the file is in one of the supported formats (`.xls`, `.xlsx`, `.dta`, `.rds`).

### **2. Sheet Presence Check**
- Verifies that all required sheets (e.g., `Service_data_1`, `Population_data`) are present.

### **3. Column Presence and Uniqueness Check**
- Ensures that:
  - All required columns (`district`, `year`, `month`, and relevant indicators) are present.
  - Column names within each sheet are unique.

### **4. Data Availability Check**
- Checks that:
  - Required sheets are not empty.
  - Key columns (`district`, `year`, `month`) are fully populated.
- Rows with missing data in key columns are excluded automatically.

### **5. Data Type Validation**
- Ensures numeric columns (e.g., indicators) contain valid numeric values.
- Values that are not numeric are converted to be empty

---

### **How Errors Are Handled**
- If validation fails, the upload process stops, and an error message is displayed.
- Refer to the **Common Errors and How to Fix Them** table for details on resolving specific issues.

---

## **Tips for a Successful Upload**

1. Always use the latest Countdown Health Facility Data Format template to structure your data.
2. Double-check column names, formats, and content before uploading.
3. Save your file in a supported format and ensure it is UTF-8 encoded.
