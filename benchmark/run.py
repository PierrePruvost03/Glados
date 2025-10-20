import subprocess
from concurrent.futures import ThreadPoolExecutor
import glob
import json
from colorama import Fore, Style
import time
import pandas as pd
import os.path
import argparse

parser = argparse.ArgumentParser(prog="Glados Benchmark",
                                 description="Benchmarks the glados")

parser.add_argument("--test", action="store_true", default=False)
parser.add_argument("--release", required=True)

args = parser.parse_args()

commands: dict[str, str] = {}

print(args)

BINARY_NAME = "./glados"

DATA_PATH = "benchmark/data.csv"

TEMP_DATA_PATH = "benchmark/temp_data.csv"

TEST_MODE = args.test

RELEASE = args.release


test_files = glob.glob("benchmark/tests/*.json")
for test_file in test_files:
    with open(test_file, "r") as f:
        for test_data in json.load(f):
            for test_name in test_data:
                scrip_name: str = test_data.get(test_name).get("script", "")
                commands[test_name] = f"{BINARY_NAME} < {scrip_name}" if scrip_name else None

default_df = pd.DataFrame(columns=["RELEASE"]  + list(commands.keys()))

df: pd.DataFrame = pd.DataFrame()

if TEST_MODE:
    df = pd.read_csv(TEMP_DATA_PATH) if os.path.exists(TEMP_DATA_PATH) else default_df
else:
    df = pd.read_csv(DATA_PATH) if os.path.exists(DATA_PATH) else default_df

def run_command_with_timing(command: str) -> subprocess.CompletedProcess:
    start = time.perf_counter()
    result = subprocess.run(command, shell=True, capture_output=True, text=True)
    end = time.perf_counter()
    execution_time = end - start

    return {
        'script': command,
        'execution_time': execution_time,
    }

def run_benchmarks_parallel(commands: dict[str, str]) -> None:
    global df
    df = df[df['RELEASE'] != int(RELEASE)].reset_index(drop=True)
    new_row = {'RELEASE': RELEASE}
    
    with ThreadPoolExecutor() as executor:
        future_to_command = {executor.submit(run_command_with_timing, cmd): cmd for cmd in list(commands.values())}

        for future in future_to_command:
            command = future_to_command[future]
            current_test_name = [name for name, cmd in commands.items() if cmd == command][0] 
            result = future.result()
            print(Fore.GREEN + f"[SUCCESS]" + Style.RESET_ALL + f" Command: {command} executed in {result['execution_time']:.4f} seconds" + Style.RESET_ALL)
            new_row[current_test_name] = f"{result['execution_time']}s"
    df = pd.concat([df, pd.DataFrame([new_row])], ignore_index=True)

def main() -> None:
    print(Fore.GREEN + "Building the project..." + Style.RESET_ALL)
    subprocess.run(["make", "re"])
    print(Fore.GREEN + "Running tests..." + Style.RESET_ALL)
    run_benchmarks_parallel(commands=commands)
    df.to_csv(TEMP_DATA_PATH if TEST_MODE else DATA_PATH , index=False)
    print(df)


if __name__ == '__main__':
    main()
