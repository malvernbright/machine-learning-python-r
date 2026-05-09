import json

# Load the notebook
with open('house-prices-test.ipynb', 'r', encoding='utf-8') as f:
    nb = json.load(f)

# Find the grid search cell
for i, cell in enumerate(nb['cells']):
    if cell.get('cell_type') == 'code' and 'GridSearchCV' in ''.join(cell.get('source', [])):
        # Insert new cell after this one
        new_cell = {
            'cell_type': 'code',
            'execution_count': None,
            'metadata': {},
            'outputs': [],
            'source': [
                'import joblib\n',
                '\n',
                '# Save the tuned model and scaler for later reuse\n',
                'joblib.dump(best_model, "best_house_price_model.joblib")\n',
                'joblib.dump(scaler, "house_price_scaler.joblib")\n',
                'print("Saved best model to best_house_price_model.joblib")\n',
                'print("Saved scaler to house_price_scaler.joblib")\n'
            ]
        }
        nb['cells'].insert(i + 1, new_cell)
        print(f"Inserted save cell after GridSearchCV cell at index {i}")
        break

# Save the notebook
with open('house-prices-test.ipynb', 'w', encoding='utf-8') as f:
    json.dump(nb, f, indent=1, ensure_ascii=False)

print('Successfully added save cell to notebook')