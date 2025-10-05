import glob
import os
from colorama import Fore, Style, Back
import json

def gen_test_file(lisp_code: str, input: str, test_name: str):
    with open(f"functionnal_tests/tmp/{test_name}.lsp", "w") as f:
        f.write(lisp_code)
        f.write("\n")
        f.write(input)

def read_tests(test_data: dict[str, str]):
    lisp_code = open(test_data["lisp_file"]).read()

    for test in test_data["tests"]:
        current_test = test_data["tests"][test]
        test_description = current_test["description"]
        expected_output = current_test["expected"]
        input = current_test["input"]
        gen_test_file(lisp_code, input, test)
        output = os.popen(f"./glados < functionnal_tests/tmp/{test}.lsp").read().strip().split("\n")[-2]
        if output == expected_output:
            print(Fore.GREEN + f"[SUCCESS]" + Style.RESET_ALL +  f" {test_description}" + Style.RESET_ALL)
            success_tests.append(test_description)
        else:
            print(Fore.RED + f"[FAILED]"  + Style.RESET_ALL + f" {test_description}" + Style.RESET_ALL)
            print(Back.RED + Fore.WHITE + f"Expected: {expected_output}" + Style.RESET_ALL)
            print(Back.RED + Fore.WHITE + f"Got: {output}" + Style.RESET_ALL)
            failed_tests.append(test_description)

def print_summary():
    print("\n" + "="*40)
    print(Fore.GREEN + f"Successful tests: {len(success_tests)} ({len(success_tests) * 100 / (len(failed_tests) + len(success_tests))}%)" + Style.RESET_ALL)
    for test in success_tests:
        print(Fore.GREEN + f"- {test}" + Style.RESET_ALL)
    print(Fore.RED + f"Failed tests: {len(failed_tests)} ({len(failed_tests) * 100 / (len(failed_tests) + len(success_tests))}%)" + Style.RESET_ALL)
    for test in failed_tests:
        print(Fore.RED + f"- {test}" + Style.RESET_ALL)
    print("="*40 + "\n")

if __name__ == "__main__":
    success_tests = []
    failed_tests = []
    test_files = glob.glob("functionnal_tests/tests/*.json")

    print(Fore.GREEN + "Building the project..." + Style.RESET_ALL)
    os.system("stack build")
    print(Fore.GREEN + "Running tests..." + Style.RESET_ALL)
    
    for test_file in test_files:
        with open(test_file, "r") as f:
            for test_data in json.load(f):
                read_tests(test_data)

    print_summary()