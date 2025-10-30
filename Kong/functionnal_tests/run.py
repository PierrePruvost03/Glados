import glob
import os
from colorama import Fore, Style, Back
import json
import sys

def gen_test_file(lisp_code: str, vars: list[str], input: str, test_name: str):
    with open(f"functionnal_tests/tmp/{test_name}.kong", "w") as f:
        f.write(lisp_code)
        f.write("\n")
        f.write(f"Funk main() -> Int {{\n")
        f.write("".join(f"  {var}\n" for var in vars))
        f.write(f"  print({input});\n")
        f.write(f"  Return 0;\n")
        f.write(f"}}\n")

def read_tests(test_data: dict[str, str]):
    script_file = test_data.get("script_file", "")
    lisp_code = open(script_file).read() if script_file else ""

    for test in test_data["tests"]:
        current_test = test_data["tests"][test]
        test_description = current_test["description"]
        expected_output = current_test["expected"]
        input = current_test["input"]
        vars = current_test.get("vars", {}).values()
        gen_test_file(lisp_code, vars, input, test)
        output = os.popen(f"./glados functionnal_tests/tmp/{test}.kong").read().strip().split("\n")
        output = [line for line in output if "[Bytecode]" not in line and "[Execution finished]" not in line]
        if "".join(output) == expected_output:
            print(Fore.GREEN + f"[SUCCESS]" + Style.RESET_ALL +  f" {test_description}" + Style.RESET_ALL)
            success_tests.append(test_description)
        else:
            print(Fore.RED + f"[FAILED]"  + Style.RESET_ALL + f" ({test}) {test_description}" + Style.RESET_ALL)
            print(Back.RED + Fore.WHITE + f"Expected: {expected_output}" + Style.RESET_ALL)
            print(Back.RED + Fore.WHITE + f"Got: {output}" + Style.RESET_ALL)
            failed_tests.append(test_description)

def print_summary():
    print("\n" + "="*40)
    print(Fore.GREEN + f"Successful tests: {len(success_tests)} ({len(success_tests) * 100 / (len(failed_tests) + len(success_tests))}%)" + Style.RESET_ALL)
    print(Fore.RED + f"Failed tests: {len(failed_tests)} ({len(failed_tests) * 100 / (len(failed_tests) + len(success_tests))}%)" + Style.RESET_ALL)
    print("="*40 + "\n")

if __name__ == "__main__":
    success_tests = []
    failed_tests = []
    test_files = glob.glob("functionnal_tests/tests/*.json")

    print(Fore.GREEN + "Building the project..." + Style.RESET_ALL)
    os.system("rm -f glados && make")
    print(Fore.GREEN + "Running tests..." + Style.RESET_ALL)
    
    for test_file in test_files:
        with open(test_file, "r") as f:
            for test_data in json.load(f):
                read_tests(test_data)

    print_summary()
    if failed_tests:
        sys.exit(1)
    sys.exit(0)