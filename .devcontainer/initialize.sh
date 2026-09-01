#!/bin/bash
# Set up environment for devcontainer build
HOST_UID=$(id -u)
HOST_GID=$(id -g)
HOST_USER=$USER
HOST_GROUP=$USER
DOCKER_GID=$(getent group docker | cut -d: -f3)
R_LIBS_HOST_DIR=${HOME}/.local/lib/R/site-library

mkdir -p "${R_LIBS_HOST_DIR}"

# Map host ~/.bashrc into the container only on Linux; fall back to /dev/null
# elsewhere so the mount stays valid without clobbering non-Linux hosts.
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
  [ -e "${HOME}/.bashrc" ] || touch "${HOME}/.bashrc"
  HOST_BASHRC="${HOME}/.bashrc"
else
  HOST_BASHRC="/dev/null"
fi

ENV_FILE="$(dirname "$0")/.env"
NEW_ENV_CONTENT="HOST_UID=${HOST_UID}
HOST_GID=${HOST_GID}
HOST_USER=${HOST_USER}
HOST_GROUP=${HOST_GROUP}
HOME=${HOME}
DOCKER_GID=${DOCKER_GID}
R_LIBS_HOST_DIR=${R_LIBS_HOST_DIR}
HOST_BASHRC=${HOST_BASHRC}"

# Only update .env file if content has changed
if [ ! -f "$ENV_FILE" ] || [ "$(cat "$ENV_FILE")" != "$NEW_ENV_CONTENT" ]; then
  echo "$NEW_ENV_CONTENT" > "$ENV_FILE"
  echo "Updated .env with: HOST_UID=$HOST_UID HOST_GID=$HOST_GID HOST_USER=$HOST_USER HOST_GROUP=$HOST_GROUP DOCKER_GID=$DOCKER_GID"
else
  echo ".env file unchanged, skipping update"
fi
echo 'Checking Docker socket permissions...'
ls -la /var/run/docker.sock
