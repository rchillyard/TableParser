import pyarrow.parquet as pq
import os

# Create the output directory if it doesn't exist
os.makedirs('taxi_sample_dataset', exist_ok=True)

table = pq.read_table('taxi_sample.parquet')

pq.write_table(table.slice(0, 500),    'taxi_sample_dataset/part-0000.parquet')
pq.write_table(table.slice(500, 1000), 'taxi_sample_dataset/part-0001.parquet')

print("Done. Two part files written to taxi_sample_dataset/")

