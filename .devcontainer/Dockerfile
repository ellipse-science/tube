# Base image already has 'vscode' user
#FROM mcr.microsoft.com/devcontainers/base:ubuntu
FROM ubuntu:22.04

USER root

# Avoid warnings by switching to noninteractive
ENV DEBIAN_FRONTEND=noninteractive

# Install essential tools and R dependencies
RUN apt-get update -o Acquire::Check-Valid-Until=false && \
    apt-get install -y --no-install-recommends \
        bash \
        curl \
        yarn \
        git \
        build-essential \
        libssl-dev \
        libcurl4-openssl-dev \
        libxml2-dev \
        locales \
        software-properties-common \
        wget \
        ca-certificates \
        dirmngr \
        sudo \
        python3-pip \
        python3-venv \
        python3-dev \
        python3-setuptools \
        gnupg \
        openssh-client \
        openssl && \
    locale-gen en_US.UTF-8 && \
    rm -rf /var/lib/apt/lists/*

# Set up locale
RUN locale-gen en_US.UTF-8
# French also
RUN locale-gen fr_FR.UTF-8
# Set the locale
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US:en
ENV LC_ALL=en_US.UTF-8


# Declare build-time arguments for host user and Docker socket
ARG HOST_UID=1000
ARG HOST_GID=1000
ARG HOST_USER=vscode
ARG HOST_GROUP=vscode
ARG DOCKER_GID=988

# Create group and user matching host
RUN groupadd -g ${HOST_GID} ${HOST_GROUP} && \
    useradd -m -s /bin/bash -u ${HOST_UID} -g ${HOST_GID} ${HOST_USER} && \
    echo "${HOST_USER} ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/${HOST_USER} && \
    chmod 0440 /etc/sudoers.d/${HOST_USER} && \
    passwd -d ${HOST_USER} && \
    mkdir -p /home/${HOST_USER} && \
    chown -R ${HOST_USER}:${HOST_GROUP} /home/${HOST_USER}

# Create docker group with matching GID and add user to it
# Handle the case where the group might already exist with a different name
RUN if ! getent group ${DOCKER_GID} > /dev/null 2>&1; then \
        groupadd -g ${DOCKER_GID} docker; \
    fi && \
    DOCKER_GROUP_NAME=$(getent group ${DOCKER_GID} | cut -d: -f1) && \
    usermod -aG "$DOCKER_GROUP_NAME" ${HOST_USER}

# Install Docker CLI
RUN curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg && \
    echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null && \
    apt-get update -o Acquire::Check-Valid-Until=false && \
    apt-get install -y --no-install-recommends docker-ce-cli && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /home/${HOST_USER}

# Install latest R from CRAN
RUN curl -fsSL https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | tee /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc && \
    echo "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/" | tee /etc/apt/sources.list.d/cran.list && \
    apt-get update -o Acquire::Check-Valid-Until=false && \
    apt-get install -y --no-install-recommends r-base r-base-dev && \
    rm -rf /var/lib/apt/lists/*

# Install AWS CLI and aws-vault
RUN curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip" \
  && unzip awscliv2.zip \
  && ./aws/install \
  && rm -rf aws awscliv2.zip \
  && curl -L -o aws-vault https://github.com/99designs/aws-vault/releases/latest/download/aws-vault-linux-amd64 \
  && chmod +x aws-vault \
  && mv aws-vault /usr/local/bin/

# Temporarily unset R_LIBS_USER to avoid polluting the user's personal library
ENV R_LIBS_USER=""

# Install into the default system library
RUN Rscript -e "install.packages('stringi', type = 'source', repos='https://cloud.r-project.org')" && \
    Rscript -e "install.packages('languageserver', repos='https://cloud.r-project.org')"

# Switch to the non-root user
USER ${HOST_USER}

# Set env vars and create necessary directories
ENV VOLTA_HOME=/home/${HOST_USER}/.volta
ENV PATH=$VOLTA_HOME/bin:$PATH
ENV PATH=/home/${HOST_USER}/.local/bin:$PATH

# Install Node.js using Volta
RUN curl https://get.volta.sh | bash \
  && export VOLTA_HOME=/home/${HOST_USER}/.volta \
  && export PATH=$VOLTA_HOME/bin:$PATH \
  && $VOLTA_HOME/bin/volta install node@20.18.1 \
  && $VOLTA_HOME/bin/volta install yarn@1.22.22 

# Create common directories if they don't exist
RUN mkdir -p /home/${HOST_USER}/.aws /home/${HOST_USER}/.ssh /home/${HOST_USER}/.awsvault /home/${HOST_USER}/.local/lib/R/site-library

# Set ownership (just in case)
RUN chown -R ${HOST_USER}:${HOST_GROUP} /home/${HOST_USER}

# Setup bash and radian history files
RUN touch /home/${HOST_USER}/.bash_history /home/${HOST_USER}/.radian_history

# Install Radian in a virtual environment (single installation)
RUN python3 -m venv /home/${HOST_USER}/.venv && \
    /home/${HOST_USER}/.venv/bin/pip install --upgrade pip && \
    /home/${HOST_USER}/.venv/bin/pip install radian

# Environment variables
ENV RUN_MODE=devcontainer
ENV AWS_SDK_LOAD_CONFIG=1
ENV R_LIBS_USER=/home/${HOST_USER}/.local/lib/R/site-library
ENV AWS_VAULT_BACKEND=file
ENV AWS_VAULT_FILE_DIR=/home/${HOST_USER}/.aws-vault

# Default to non-root user
USER ${HOST_USER}

# Switch back to dialog for any ad-hoc use of apt-get
ENV DEBIAN_FRONTEND=dialog

# Default command to keep container running
CMD [ "sleep", "infinity" ]
