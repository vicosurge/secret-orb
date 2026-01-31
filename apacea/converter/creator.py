import json
import random
from datetime import datetime, timedelta

def generate_sample_data(num_records=200):
    """Generate sample data with 15-20 fields per object"""
    
    # Sample data pools
    first_names = ["John", "Jane", "Bob", "Alice", "Charlie", "Diana", "Eve", "Frank"]
    last_names = ["Smith", "Johnson", "Williams", "Brown", "Jones", "Garcia", "Miller"]
    cities = ["New York", "Los Angeles", "Chicago", "Houston", "Phoenix", "Philadelphia"]
    departments = ["Sales", "Engineering", "Marketing", "HR", "Finance", "Operations"]
    skills = ["Python", "Java", "C++", "JavaScript", "SQL", "Excel", "Leadership"]
    
    records = []
    
    for i in range(num_records):
        # Generate a base date for consistency
        base_date = datetime(2020, 1, 1) + timedelta(days=random.randint(0, 1460))
        
        record = {
            # Personal Information (5 fields)
            "id": i + 1,
            "first_name": random.choice(first_names),
            "last_name": random.choice(last_names),
            "age": random.randint(22, 65),
            "email": f"user{i+1}@company.com",
            
            # Employment Information (6 fields)
            "department": random.choice(departments),
            "position": f"Senior {random.choice(['Analyst', 'Developer', 'Manager', 'Specialist'])}",
            "salary": round(random.uniform(45000, 120000), 2),
            "hire_date": base_date.strftime("%Y-%m-%d"),
            "active": random.choice([True, True, True, False]),  # 75% active
            "employee_code": f"EMP{i+1:04d}",
            
            # Location & Contact (4 fields)
            "city": random.choice(cities),
            "zip_code": f"{random.randint(10000, 99999)}",
            "phone": f"+1-555-{random.randint(100, 999)}-{random.randint(1000, 9999)}",
            "emergency_contact": f"+1-555-{random.randint(100, 999)}-{random.randint(1000, 9999)}",
            
            # Performance & Skills (5+ fields)
            "performance_score": round(random.uniform(1.0, 5.0), 1),
            "years_experience": random.randint(1, 20),
            "primary_skill": random.choice(skills),
            "secondary_skill": random.choice(skills),
            "certification_count": random.randint(0, 8),
            
            # Optional fields (randomly include 0-3 additional fields)
            **({"bonus_eligible": random.choice([True, False])} if random.random() > 0.3 else {}),
            **({"manager_id": random.randint(1, 50)} if random.random() > 0.4 else {}),
            **({"training_hours": random.randint(0, 120)} if random.random() > 0.5 else {}),
            **({"last_review_date": (base_date + timedelta(days=random.randint(30, 365))).strftime("%Y-%m-%d")} if random.random() > 0.6 else {})
        }
        
        records.append(record)
    
    return records

def save_json_data(data, filename="employee_data.json"):
    """Save data to JSON file with pretty formatting"""
    with open(filename, 'w', encoding='utf-8') as f:
        json.dump(data, f, indent=2, ensure_ascii=False)
    
    print(f"Generated {len(data)} records")
    print(f"Saved to: {filename}")
    print(f"File size: {len(json.dumps(data)) / 1024:.1f} KB")

def analyze_data_structure(data):
    """Analyze the data structure for Pascal type definition"""
    if not data:
        return
    
    print("\n=== Data Structure Analysis ===")
    sample_record = data[0]
    
    field_info = {}
    for record in data:
        for key, value in record.items():
            if key not in field_info:
                field_info[key] = {"type": type(value).__name__, "max_len": 0, "required": 0}
            
            field_info[key]["required"] += 1
            
            if isinstance(value, str):
                field_info[key]["max_len"] = max(field_info[key]["max_len"], len(value))
    
    print(f"Total records: {len(data)}")
    print(f"Fields per record: {len(sample_record)}")
    print("\nField Analysis:")
    
    for field, info in sorted(field_info.items()):
        required_pct = (info["required"] / len(data)) * 100
        if info["type"] == "str":
            print(f"  {field:20} | {info['type']:8} | Max: {info['max_len']:3} chars | {required_pct:5.1f}% present")
        else:
            print(f"  {field:20} | {info['type']:8} | {required_pct:5.1f}% present")

if __name__ == "__main__":
    # Generate data
    employee_data = generate_sample_data(200)
    
    # Save to JSON
    save_json_data(employee_data, "employee_data.json")
    
    # Analyze structure
    analyze_data_structure(employee_data)
