#!/bin/bash

# Script to set up the symbolic link for the outputs directory

# Set colors for output
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Get the current script directory and project root
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
PROJECT_ROOT="$SCRIPT_DIR"

echo -e "${YELLOW}Setting up ABL Comment Detector outputs directory...${NC}"

# Ensure outputs directory exists
mkdir -p "$PROJECT_ROOT/outputs"

# Set up symbolic link to ensure outputs go to the root-level outputs directory
# Remove the src/CSharp/outputs directory if it exists and is not a symbolic link
if [ -d "$PROJECT_ROOT/src/CSharp/outputs" ] && [ ! -L "$PROJECT_ROOT/src/CSharp/outputs" ]; then
    echo -e "${YELLOW}Removing existing outputs directory in src/CSharp...${NC}"
    rm -rf "$PROJECT_ROOT/src/CSharp/outputs"
fi

# Create symbolic link if it doesn't exist
if [ ! -L "$PROJECT_ROOT/src/CSharp/outputs" ]; then
    echo -e "${YELLOW}Creating symbolic link for outputs directory...${NC}"
    ln -sf "$PROJECT_ROOT/outputs" "$PROJECT_ROOT/src/CSharp/outputs"
fi

echo -e "${GREEN}Setup complete!${NC} Output files will now be stored in the $PROJECT_ROOT/outputs directory." 