import json
import random
import time

def generate_sensor_data(sensor_id):
    timestamp = int(time.time()) * 1000  # Current time in milliseconds
    data = {
        "temperature": round(random.uniform(15.0, 35.0), 2),  # Random temperature between 15.0 and 35.0
        "humidity": round(random.uniform(40.0, 70.0), 2),     # Random humidity between 40.0 and 70.0
        "status": random.choice(["normal", "warning", "error"])  # Random status
    }
    return {
        "sensor_id": sensor_id,
        "timestamp": timestamp,
        "data": data
    }

sensor_data_list = []
for i in range(10000):  # Generate 10000 records
    sensor_id = f"sensor_{i + 1}"
    sensor_data = generate_sensor_data(sensor_id)
    sensor_data_list.append(sensor_data)

output_file = 'sensor_data.json'

with open(output_file, 'w') as file:
    json.dump(sensor_data_list, file, indent=2)

print(f"Generated JSON data saved to {output_file}")
