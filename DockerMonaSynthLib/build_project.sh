#!/bin/bash

# Set default LLVM version (you can change this to 14 or 16 as per your needs)
LLVM_VERSION=13

# Define paths for include and lib dirs based on the selected LLVM version
INCLUDE_DIR="/usr/lib/llvm-${LLVM_VERSION}/include"
LIB_DIR="/usr/lib/llvm-${LLVM_VERSION}/lib"
LLVM_CONFIG="/usr/bin/llvm-config-${LLVM_VERSION}"

# Check if LLVM is installed, if not, install it via apt (for Ubuntu/Debian)
if ! command -v llvm-config-${LLVM_VERSION} &> /dev/null; then
    echo "LLVM ${LLVM_VERSION} is not installed. Installing..."
    wget https://apt.llvm.org/llvm.sh && chmod +x llvm.sh && ./llvm.sh ${LLVM_VERSION}
else
    echo "LLVM ${LLVM_VERSION} is already installed."
fi

# Create or update the cabal.project.local file with the correct LLVM paths
PROJECT_LOCAL_FILE="cabal.project.local"

cat <<EOL > ${PROJECT_LOCAL_FILE}
package llvm-ffi
  flags: +llvm${LLVM_VERSION}0
  extra-include-dirs: ${INCLUDE_DIR}
  extra-lib-dirs: ${LIB_DIR}
EOL

echo "Updated ${PROJECT_LOCAL_FILE} with LLVM ${LLVM_VERSION} paths."

# Ensure the package list is updated
echo "Updating Cabal package list..."
cabal update

# Set the necessary environment variables for LLVM
export LLVM_CONFIG=${LLVM_CONFIG}
export C_INCLUDE_PATH=${INCLUDE_DIR}
export LIBRARY_PATH=${LIB_DIR}

# Build the project dependencies and executable
echo "Building project dependencies..."
cabal build --only-dependencies
cabal build

echo "Project built successfully with LLVM ${LLVM_VERSION}."

