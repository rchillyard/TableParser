import pyarrow.parquet as pq

table = pq.read_table('yellow_tripdata_2024-01.parquet')

print(table.schema)

pq.write_table(
    table.slice(0, 1000),
    'taxi_sample.parquet'
)

print(f"Done. Original rows: {table.num_rows}, Sample rows: 1000")

