# Use an official base image
FROM ubuntu:latest

# Install dependencies
# Combining installation commands into a single RUN to reduce layers. 
# Also, clean up apt cache to reduce image size.
RUN apt-get update && apt-get install -y \
    build-essential \
    cabal-install \
    git \
    darcs \
 && rm -rf /var/lib/apt/lists/*

# Clone the repository
RUN git clone https://gitlab.com/zugz/tersmu.git /app/tersmu

# Change to the cloned repository's directory
WORKDIR /app/tersmu

# Update cabal and install the application
# Combining cabal update and make install into a single RUN command for efficiency
RUN cabal update && make install

# Set environment variables
# Setting PATH here as it applies to the image at runtime
ENV PATH=$PATH:/root/.cabal/bin

