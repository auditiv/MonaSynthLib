# Start from a Haskell base image with GHC 9.6
FROM haskell:9.6

# Install LLVM 13 and LLVM 16 along with necessary system dependencies
RUN apt-get update && apt-get install -y \
    llvm-13 \
    llvm-13-dev \
    llvm-16 \
    llvm-16-dev \
    llvm-16-tools \
    libffi-dev \
    libgmp-dev \
    libsox-fmt-all \
    libsox-dev \
    sox \
    zlib1g-dev \
    libncurses5-dev \
    && apt-get clean

# Set working directory inside the container
WORKDIR /usr/src/app


COPY monasynthlib/ ./

# Set LLVM_CONFIG to LLVM 16 for building the project
ENV LLVM_CONFIG=/usr/bin/llvm-config-16
ENV C_INCLUDE_PATH=/usr/lib/llvm-16/include
ENV LIBRARY_PATH=/usr/lib/llvm-16/lib

# Ensure the Cabal package list is updated
RUN cabal update

# Build the project dependencies and executable using LLVM 16
RUN cabal build --only-dependencies
RUN cabal build

# After building with LLVM 16, switch to LLVM 13 for runtime
ENV LLVM_CONFIG=/usr/bin/llvm-config-13
ENV C_INCLUDE_PATH=/usr/lib64/llvm13/include
ENV LIBRARY_PATH=/usr/lib64/llvm13/lib
#
# Setup audio streaming inside the container

# Copy the audio setup script
COPY setup_audio_stream.sh /usr/src/app/setup_audio_stream.sh
RUN chmod +x /usr/src/app/setup_audio_stream.sh

# Expose port 8000 for Icecast stream
EXPOSE 8000
# Specify the command to run the executable with LLVM 13
CMD ["cabal", "run", "monallvsa"]

