import json
from pathlib import Path

import joblib
import numpy as np
import pandas as pd

MODEL_PATH = Path("best_house_price_model.pkl")
SCALER_PATH = Path("house_price_scaler.pkl")


def load_agent(model_path: Path = MODEL_PATH, scaler_path: Path = SCALER_PATH):
    """Load the saved regression model and preprocessing scaler."""
    model = joblib.load(model_path)
    scaler = joblib.load(scaler_path)
    return model, scaler


def preprocess_features(raw_features: dict, scaler):
    """Prepare a feature row for prediction using the same scaling step."""
    data = raw_features.copy()
    data["resid_area_air_qual"] = data["air_qual"] + data["resid_area"]
    df = pd.DataFrame([data])
    df[["resid_area_air_qual", "room_num"]]
    df[["resid_area_air_qual", "room_num"]] = scaler.transform(
        df[["resid_area_air_qual", "room_num"]]
    )
    return df


def predict_price(raw_features: dict, model=None, scaler=None):
    """Predict house price from a dict of raw input features."""
    if model is None or scaler is None:
        model, scaler = load_agent()

    X = preprocess_features(raw_features, scaler)
    return float(model.predict(X)[0])


def get_expected_features(model):
    return getattr(model, "feature_names_in_", None)


def validate_input(raw_features: dict, model):
    expected = get_expected_features(model)
    if expected is None:
        return
    missing = [name for name in expected if name not in raw_features]
    if missing:
        raise ValueError(f"Missing features for prediction: {missing}")


def run_cli():
    model, scaler = load_agent()
    print("House Price Prediction Agent")
    print("Provide input features as JSON in a file, or call predict_price() from Python.")
    print("Example: python house_price_agent.py sample_input.json")


if __name__ == "__main__":
    import sys

    if len(sys.argv) != 2:
        print("Usage: python house_price_agent.py <input_json>")
        sys.exit(1)

    input_path = Path(sys.argv[1])
    if not input_path.exists():
        raise FileNotFoundError(f"Input JSON file not found: {input_path}")

    raw = json.loads(input_path.read_text())
    model, scaler = load_agent()
    validate_input(raw, model)
    predicted = predict_price(raw, model, scaler)
    print(f"Predicted house price: {predicted}")
