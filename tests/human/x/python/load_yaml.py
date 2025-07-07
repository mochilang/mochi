import yaml
from pathlib import Path

# Load people from YAML file
path = Path(__file__).resolve().parents[3] / 'tests' / 'interpreter' / 'valid' / 'people.yaml'
with path.open() as f:
    people = yaml.safe_load(f)

adults = [{"name": p["name"], "email": p["email"]} for p in people if p["age"] >= 18]
for a in adults:
    print(a["name"], a["email"])
