### Plotting Graphs for Internal Consistency Checks

#### Purpose
The dashboard enables users to visualize and analyze key consistency ratios, providing insights into health facility data. The following pairs of indicators are used for comparison:

1. **ANC1 vs Penta1**: Compare antenatal care visits (ANC1) with the first dose of pentavalent vaccine (Penta1).
2. **Penta1 vs Penta3**: Examine dropout rates between the first and third doses of pentavalent vaccine.
3. **OPV1 vs OPV3**: Highlight inconsistencies in oral polio vaccine (OPV) service delivery.
4. **Custom Graph**: Allow users to select and compare any two indicators interactively.

#### Graph Generation Process

1. **Prepare Data**:
   - The system imports and cleans health facility data automatically.
   - It ensures relevant indicators, such as `ANC1`, `Penta1`, `Penta3`, `OPV1`, and `OPV3`, are included.

2. **Generate Plots**:
   - The dashboard provides pre-built visualizations for each indicator pair, ensuring users can explore trends and relationships effortlessly.

3. **Customizable Graphs**:
   - Users can interactively select any two indicators to generate custom plots, offering flexibility for deeper analysis.

#### Recommendations for Interpretation

- **Visual Patterns**: Examine graphs for linear relationships or significant deviations. Outliers or patterns far from the line of equality may indicate data quality issues.
- **Custom Comparisons**: Use the custom graph feature to explore additional indicators and uncover trends or inconsistencies beyond the predefined comparisons.

This intuitive dashboard streamlines the process of evaluating internal consistency, making it easier to identify and address data issues.
