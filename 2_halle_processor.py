import os
import pandas as pd
from transformers import pipeline

# 1. Establish automated directory paths to ensure absolute anonymity
BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DATA_DIR = os.path.join(BASE_DIR, "data_pipeline")

input_path = os.path.join(DATA_DIR, "rapunzel_woc_buzz.csv")
output_path = os.path.join(DATA_DIR, "woc_model_sentiment.csv")

# Load the direct WOC Rapunzel buzz dataset
print(f"Loading direct WOC Rapunzel tracking file from: {input_path}")
try:
    df_woc_master = pd.read_csv(input_path)
except FileNotFoundError:
    raise FileNotFoundError(f"Error: Ensure 'rapunzel_woc_buzz.csv' is placed inside the data_pipeline/ folder.")

# 2. Fire up the deep learning sequence modeling framework
print("Initializing RoBERTa Transformer for direct WOC Rapunzel trend data...")
sentiment_analyzer = pipeline("sentiment-analysis", model="cardiffnlp/twitter-roberta-base-sentiment-latest")

results = []
print("Analyzing sentence data context...")
for text in df_woc_master['text'].astype(str):
    if not text.strip() or text == 'nan':
        results.append({'label': 'neutral', 'score': 0.0})
        continue
        
    clean_text = text[:512] # Protect model token limits
    
    try:
        # strips the outer list bracket immediately to return a clean dictionary
        prediction = sentiment_analyzer(clean_text)[0]
        results.append(prediction)
    except Exception as e:
        results.append({'label': 'neutral', 'score': 0.0})

# 3. Map keys and export standalone scored file
df_woc_master['label'] = [r['label'] for r in results]
df_woc_master['confidence'] = [r['score'] for r in results]

df_woc_master.to_csv(output_path, index=False)
print(f"WOC Rapunzel buzz processing complete! Saved to: {output_path}")
