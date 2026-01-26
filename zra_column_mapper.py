"""
ZRA Column Mapper Script
========================
This script maps ZRA columns from product_import_template.xlsx (df_clean) 
to ProductTemplate.xlsx (df_raw) using Odoo's x_* field naming convention.

The resulting file can be used to update products in Odoo 18.
"""

import pandas as pd
import numpy as np

# Load the data files
df_clean = pd.read_excel('/Users/malvernbright/Desktop/ML/product_import_template.xlsx')
df_raw = pd.read_excel('/Users/malvernbright/Desktop/ML/ProductTemplate.xlsx')

print(f"Loaded df_clean: {len(df_clean)} products, {len(df_clean.columns)} columns")
print(f"Loaded df_raw: {len(df_raw)} products, {len(df_raw.columns)} columns")

# Column mapping: df_clean (human-readable) -> df_raw (Odoo x_* fields)
column_mapping = {
    'Item Classification': 'x_itemCls',
    'Item Classification Code(zra)': 'x_itemClsCd',
    'Origin Place Code (Nation)': 'x_orgnNatCd',
    'Origin Place (Nation)': 'x_orgnNat',
    'Export Nation Code': 'x_exptNatCd',
    'Export Nation': 'x_exptNat',
    'Packaging Unit Code (zra)': 'x_pkgUnitCd',
    'Packaging Unit': 'x_pkgUnit',
    'VAT Category Code': 'x_vatCatCd',
    'ZRA Product Type': 'x_itemTyCd',
    'Quantity Unit Code(zra)': 'x_qtyUnitCd',
    'ZRA Unit of Measure': 'x_UnitCd',
    'ZRA Purchase Unit of Measure': 'x_PacCd',
    'Insurance Applicable (Y/N)': 'x_isrcAplcbYn',
    'Indication of whether an item has rental or not': 'x_rentalYn',
    'Indication of whether an item has service charge': 'x_svcChargeYn',
    'Used/UnUsed': 'x_useYn',
    'ZRA Modify Y/N': 'x_ZRAModYn',
    'tlCatCd': 'x_tlCatCd',
    'ExciseCatCd': 'x_exciseTxCatCd',
    'Destination Country Code': 'x_export_country_id',
    'Is Import Item?': 'x_is_import_item',
    'Declaration Date': 'x_dclDe',
    'Import Item Status Code': 'x_imptItemsttsCd',
    'Manufacturer item code for MTV product': 'x_manufacturerItemCd',
    'Manufacturer TPIN for MTV product': 'x_manufactuterTpin',
    'RRP': 'x_rrp',
    'Product name': 'name',
    'Barcode': 'barcode',
    'Internal Reference': 'default_code',
    'Sales Price': 'list_price',
    'Cost': 'standard_price'
}

print(f"\nColumn mapping defined: {len(column_mapping)} mappings")

# Rename columns in df_clean to match Odoo x_* field names
df_clean_renamed = df_clean.rename(columns=column_mapping)

# Identify ZRA columns (x_*) from df_clean_renamed
zra_columns_in_clean = [col for col in df_clean_renamed.columns if col.startswith('x_')]
zra_columns_in_raw = [col for col in df_raw.columns if col.startswith('x_')]

print(f"\nZRA columns in df_clean (renamed): {len(zra_columns_in_clean)}")
print(f"ZRA columns in df_raw: {len(zra_columns_in_raw)}")

# Check product matching
print(f"\nProducts in df_raw: {len(df_raw)}")
print(f"Products in df_clean: {len(df_clean_renamed)}")

# Check for matching product names
common_names = set(df_raw['name'].dropna()) & set(df_clean_renamed['name'].dropna())
print(f"Products matching by name: {len(common_names)}")

# Select columns to merge (only the ZRA x_* columns we need)
cols_to_merge = ['name'] + zra_columns_in_clean
df_clean_subset = df_clean_renamed[cols_to_merge].copy()

# Merge the ZRA columns from df_clean into df_raw based on product name
# Using left join to keep all products in df_raw
df_merged = df_raw.merge(
    df_clean_subset, 
    on='name', 
    how='left', 
    suffixes=('', '_new')
)

print(f"\nMerged dataframe shape: {df_merged.shape}")

# Update existing ZRA columns with new values from df_clean where available
for col in zra_columns_in_clean:
    new_col = f'{col}_new'
    if new_col in df_merged.columns:
        # Update original column with new values where they exist
        df_merged[col] = df_merged[new_col].combine_first(df_merged[col])
        # Drop the _new suffix column
        df_merged = df_merged.drop(columns=[new_col])

print(f"Final merged dataframe shape: {df_merged.shape}")

# Save the updated dataframe to a new Excel file for Odoo import
output_path = '/Users/malvernbright/Desktop/ML/ProductTemplate_Updated.xlsx'
df_merged.to_excel(output_path, index=False)

print(f"\n{'='*60}")
print(f"Updated product template saved to: {output_path}")
print(f"Total products: {len(df_merged)}")
print(f"Total columns: {len(df_merged.columns)}")
print(f"{'='*60}")

# Summary of ZRA fields now in the updated template
zra_cols_final = [col for col in df_merged.columns if col.startswith('x_')]
print(f"\nZRA fields (x_*) now present in the updated template ({len(zra_cols_final)} fields):")
for i, col in enumerate(zra_cols_final, 1):
    print(f"  {i}. {col}")
