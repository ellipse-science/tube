# Use a smaller footprint base image
FROM ubuntu:24.04

# Avoid warnings by switching to noninteractive
ENV DEBIAN_FRONTEND=noninteractive

# Configure timezone and install basic tools and dependencies
ENV TZ=America/Toronto
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone \
  && apt-get update && apt-get install -y \
  curl \
  git \
  wget \
  unzip \
  python3-pip \
  python3-venv \
  python3-dev \
  python3-setuptools \
  build-essential \
  software-properties-common \
  apt-transport-https \
  gnupg \
  r-base \
  r-base-dev \
  passwd \
  sudo \
  vim \
  locales \
  docker.io \
  libcurl4-openssl-dev \
  libssl-dev \
  libxml2-dev \
  libfontconfig1-dev \
  libharfbuzz-dev \
  libfribidi-dev \
  libfreetype6-dev \
  libpng-dev \
  libtiff5-dev \
  libjpeg-dev \
  libmagick++-dev \
  && rm -rf /var/lib/apt/lists/*

# Set up locale
RUN locale-gen en_US.UTF-8
# French also
RUN locale-gen fr_FR.UTF-8
# Set the locale
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US:en
ENV LC_ALL=en_US.UTF-8

# Install AWS CLI and aws-vault
RUN curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip" \
  && unzip awscliv2.zip \
  && ./aws/install \
  && rm -rf aws awscliv2.zip \
  && curl -L -o aws-vault https://github.com/99designs/aws-vault/releases/latest/download/aws-vault-linux-amd64 \
  && chmod +x aws-vault \
  && mv aws-vault /usr/local/bin/

# Rename the default 'ubuntu' user to 'devuser'
RUN usermod -l devuser -d /home/devuser -m ubuntu \
  && groupmod -n devuser ubuntu \
  && echo "devuser ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/devuser \
  && chmod 0440 /etc/sudoers.d/devuser \
  && passwd -d devuser \
  && mkdir -p /home/devuser \
  && chown -R devuser:devuser /home/devuser \
  && (getent group docker && groupmod -g 988 docker || groupadd -g 988 docker) \
  && usermod -aG docker devuser

# Install code-server
RUN curl -fsSL https://code-server.dev/install.sh | sh

# Create code-server config
RUN mkdir -p /home/devuser/.config/code-server && \
    echo 'bind-addr: 0.0.0.0:8080\nauth: none\ncert: false' > /home/devuser/.config/code-server/config.yaml && \
    chown -R devuser:devuser /home/devuser/.config

COPY entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

EXPOSE 8080

# Switch to the non-root user
USER devuser

# Set env vars and create necessary directories
ENV VOLTA_HOME=/home/devuser/.volta
ENV PATH=$VOLTA_HOME/bin:$PATH
ENV PATH=/home/devuser/.local/bin:$PATH

# Install Radian in a virtual environment
RUN python3 -m venv /home/devuser/venv \
  && . /home/devuser/venv/bin/activate \
  && pip install --upgrade pip \
  && pip install radian

# Install Node.js using Volta
RUN curl https://get.volta.sh | bash \
  && export VOLTA_HOME=/home/devuser/.volta \
  && export PATH=$VOLTA_HOME/bin:$PATH \
  && $VOLTA_HOME/bin/volta install node@18.17.1 \
  && $VOLTA_HOME/bin/volta install yarn@1.22.22 

# Switch back to dialog for any ad-hoc use of apt-get
ENV DEBIAN_FRONTEND=dialog

# Link the Radian binary to the PATH
RUN mkdir -p /home/devuser/.local/bin \
  && ln -s /home/devuser/venv/bin/radian /home/devuser/.local/bin/radian

# Set the default shell to bash
ENV SHELL=/bin/bash
CMD ["/usr/local/bin/entrypoint.sh"]
