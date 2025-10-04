#!/bin/bash

GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m'

if [ ! -d ".venv" ]; then
    echo -e "${GREEN}[+] Creating venv${NC}"
    python3 -m venv .venv
fi

echo -e "${GREEN}[+] Activating venv${NC}"
source .venv/bin/activate
echo -e "${GREEN}[+] Installing packages${NC}"
pip install -r functionnal_tests/requirements.txt || { echo -e "${RED}[!] Installation error${NC}"; exit 1; }
echo -e "${GREEN}[+] Done${NC}"