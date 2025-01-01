# DHIS2 Download Guide

This guide explains how to securely connect to DHIS2 using the app and outlines the steps to download data.

---

## **Steps for Connecting to DHIS2**

1. **Enter Your Credentials**:
   - Provide your DHIS2 **username** and **password** in the respective fields.

2. **Select a Country**:
   - Choose your country from the dropdown menu.

3. **Set the Period**:
   - Define the date range for the data you want to fetch using the `Period` field.

4. **Click Login**:
   - Press the `Login` button to start the connection process.

5. **Download Data**:
   - After the data is successfully fetched:
     - Use the **Download Master File** button for a `.dta` file.
     - Use the **Download Excel File** button for a `.xlsx` file.

---

## **Validation Process**

To ensure successful connection and data download, the app performs the following validations:

1. **Credential Check**:
   - The app verifies that both username and password fields are not empty.
   - **Error Example**: *"Error: Username must be provided."*

2. **Country Selection**:
   - A valid country must be selected from the dropdown list.
   - **Error Example**: *"Error: Country must be selected."*

3. **Date Range Check**:
   - Ensures that the selected date range is valid and not empty.

4. **DHIS2 Server Connection**:
   - The app establishes a secure connection to the DHIS2 server using the selected country's configuration.

---

## **Security and Privacy**

- **Temporary Credentials**:
  - Your DHIS2 credentials are only used temporarily during the session and are not stored by the app.

- **Session-Based Authentication**:
  - Once the session ends, all credential information is discarded.

- **Open Source Transparency**:
  - The app's open-source nature ensures transparency in how your credentials are handled.

---

## **Common Errors and How to Resolve Them**

| **Error Message**                      | **Cause**                               | **Solution**                          |
|----------------------------------------|-----------------------------------------|---------------------------------------|
| "Error: Country must be selected"      | No country selected from the dropdown   | Select a valid country from the list. |
| "Error: Username must be provided"     | Username field is empty                 | Enter your DHIS2 username.            |
| "Error: Password must be provided"     | Password field is empty                 | Enter your DHIS2 password.            |
| "Download Error: Failed to retrieve user profile."   | Issue with the DHIS2 server or timeout  | Ensure credentials and country are correct; check server status. |

---

## **Troubleshooting Tips**

1. **Connection Issues**:
   - Verify your internet connection.
   - Ensure that your DHIS2 server is operational.

2. **Credential Problems**:
   - Double-check your username and password for typos.
   - Ensure you have access to the selected country's DHIS2 server.

3. **Timeout Errors**:
   - If fetching data takes too long, try narrowing the date range.

4. **Missing Data**:
   - Ensure the selected date range contains data on the DHIS2 server.
