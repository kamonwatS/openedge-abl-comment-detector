#!/bin/bash

# Script to run the ABL Comment Detector from the project root

# Set colors for output
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Get the current script directory and project root
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
PROJECT_ROOT="$SCRIPT_DIR"

# Help function
show_help() {
    echo "ABL Comment Detector"
    echo "==================="
    echo ""
    echo "Usage:"
    echo "  $0 [options] <path>       # Process a file or directory"
    echo "  $0 --help                 # Display this help message"
    echo ""
    echo "Examples:"
    echo "  $0 inputs/sample-code.p            # Process a single file"
    echo "  $0 inputs                          # Process all files in a directory"
    echo "  $0 --verify inputs/WRSBQ7072.W     # Process a file and verify results"
    echo ""
    echo "Options:"
    echo "  --verify                  # Run verification after processing"
    exit 0
}

# Check for help flag
if [[ "$1" == "--help" || "$1" == "-h" ]]; then
    show_help
fi

# Check for verification flag
VERIFY=false
if [[ "$1" == "--verify" ]]; then
    VERIFY=true
    shift
fi

# Check if a path was provided
if [ $# -eq 0 ]; then
    echo -e "${RED}Error:${NC} No input path provided."
    echo "Run '$0 --help' for usage information."
    exit 1
fi

# Get the input path
INPUT_PATH="$1"

# Check if running from the project root
if [ ! -f "$PROJECT_ROOT/src/CSharp/AblCommentDetector.csproj" ]; then
    echo -e "${RED}Error:${NC} This script must be run from the project root directory."
    echo "Please change to the project root directory and try again."
    exit 1
fi

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

# Run the ABL Comment Detector
echo -e "${YELLOW}Running ABL Comment Detector on:${NC} $INPUT_PATH"

# Always run from project root to ensure proper relative path resolution
cd "$PROJECT_ROOT"
dotnet run --project src/CSharp/AblCommentDetector.csproj -- "$INPUT_PATH"

# Check if verification is requested
if [ "$VERIFY" = true ]; then
    echo -e "\n${YELLOW}Verifying results...${NC}"
    "$PROJECT_ROOT/src/Shell/verify-results.sh" "$INPUT_PATH"
fi

echo -e "\n${GREEN}Process complete.${NC} Results are in the outputs directory." 