import json
import sys
import re


def extract_violation_info(entry):
    # Try to find function pairs from the qualifier
    qualifier = entry.get("qualifier", "")
    match = re.search(r"Functions\s+'([^']+)'\s+and\s+'([^']+)'", qualifier)
    if not match:
        return None
    f1, f2 = sorted([match.group(1), match.group(2)])

    filename = entry.get("file")
    line_number = entry.get("line")

    return (f1, f2, filename, line_number)


def load_violations(filepath):
    with open(filepath, "r") as f:
        content = json.load(f)

    violations = set()
    for entry in content:
        info = extract_violation_info(entry)
        if info:
            violations.add(info)
    return violations


def compare_violation_files(file1, file2):
    v1 = load_violations(file1)
    v2 = load_violations(file2)

    if v1 == v2:
        print("✅ Both files contain the same function violation pairs at the same locations.")
    else:
        print("❌ Violations differ between files.")
        only_in_1 = v1 - v2
        only_in_2 = v2 - v1

        if only_in_1:
            print("\nViolations only in", file1)
            for v in sorted(only_in_1):
                print(v)

        if only_in_2:
            print("\nViolations only in", file2)
            for v in sorted(only_in_2):
                print(v)

    print(f"\nTotal violations in {file1}: {len(v1)}")
    print(f"Total violations in {file2}: {len(v2)}")


if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python compare_violations.py <file1.json> <file2.json>")
        sys.exit(1)

    compare_violation_files(sys.argv[1], sys.argv[2])
