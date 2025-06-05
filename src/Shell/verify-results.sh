#!/bin/bash

# Script to verify OpenEdge ABL Comment Detector results against reference file

# Set colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

# Set script and project root directories
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
PROJECT_ROOT="$( cd "$SCRIPT_DIR/../.." &> /dev/null && pwd )"

# Help function
show_help() {
    echo "OpenEdge ABL Comment Detector Verification Script"
    echo "================================================="
    echo ""
    echo "Usage:"
    echo "  $0 [file_path]       # Verify results using a specific ABL file"
    echo "  $0 --help            # Display this help message"
    echo ""
    echo "Examples:"
    echo "  $0                   # Uses default file (sample-code.p)"
    echo "  $0 inputs/WRSBQ7072.W"
    echo "  $0 inputs/WXZCDOC1.P"
    echo ""
    echo "The script will run the ABL Comment Detector on the specified file"
    echo "and compare the results with the reference file in tests/compare-test-data.txt"
    exit 0
}

# Check for help flag
if [[ "$1" == "--help" || "$1" == "-h" ]]; then
    show_help
fi

# Get the file to test from command line argument or use default
if [ $# -eq 0 ]; then
    # No arguments, use default
    TEST_FILE="inputs/sample-code.p"
else
    # Use the provided path directly
    TEST_FILE="$1"
fi

# Check if the file exists (using relative path to project root)
if [ ! -f "$PROJECT_ROOT/$TEST_FILE" ] && [ ! -f "$TEST_FILE" ]; then
    echo -e "${RED}Error:${NC} File '$TEST_FILE' does not exist."
    echo "Run '$0 --help' for usage information."
    exit 1
fi

# Ensure outputs directory exists and symbolic link is set up
mkdir -p "$PROJECT_ROOT/outputs"
# Set up symbolic link for outputs directory if needed
if [ -d "$PROJECT_ROOT/src/CSharp/outputs" ] && [ ! -L "$PROJECT_ROOT/src/CSharp/outputs" ]; then
    rm -rf "$PROJECT_ROOT/src/CSharp/outputs"
fi
if [ ! -L "$PROJECT_ROOT/src/CSharp/outputs" ]; then
    ln -sf "$PROJECT_ROOT/outputs" "$PROJECT_ROOT/src/CSharp/outputs"
fi

# Store output and reference file paths
OUTPUT_FILE="$PROJECT_ROOT/outputs/analysis-result.txt"
REFERENCE_FILE="$PROJECT_ROOT/tests/compare-test-data.txt"

# Verify that the reference file exists
if [ ! -f "$REFERENCE_FILE" ]; then
    echo -e "${YELLOW}Warning:${NC} Reference file '$REFERENCE_FILE' does not exist."
    echo "Results will be generated but not compared."
fi

echo -e "${YELLOW}Running OpenEdge ABL Comment Detector on file:${NC} $(basename "$TEST_FILE")"

# Run from project root to ensure proper relative path resolution
cd "$PROJECT_ROOT"
dotnet run --project src/CSharp/AblCommentDetector.csproj -- "$TEST_FILE"

# Make sure the output file exists
if [ ! -f "$OUTPUT_FILE" ]; then
    echo -e "${RED}Error:${NC} Output file not generated at '$OUTPUT_FILE'."
    exit 1
fi

# Compare with reference file if it exists
if [ -f "$REFERENCE_FILE" ]; then
    # Store the exit code of the diff command
    diff -q "$OUTPUT_FILE" "$REFERENCE_FILE"
    DIFF_RESULT=$?
    
    if [ $DIFF_RESULT -eq 0 ]; then
        echo -e "${GREEN}✓ SUCCESS:${NC} Results match the reference file."
        exit 0
    else
        echo -e "${RED}✗ FAILURE:${NC} Results differ from the reference file."
        echo "Differences:"
        diff -u "$REFERENCE_FILE" "$OUTPUT_FILE"
        exit 1
    fi
else
    echo -e "${YELLOW}Note:${NC} No comparison performed. Check output at: $OUTPUT_FILE"
    exit 0
fi 