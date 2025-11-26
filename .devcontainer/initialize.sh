#!/bin/bash
# Set up environment for devcontainer build
HOST_UID=$(id -u)
HOST_GID=$(id -g)
HOST_USER=$USER
HOST_GROUP=$USER

# Create .env file for docker-compose to pick up
cat > "$(dirname "$0")/.env" <<EOF
HOST_UID=${HOST_UID}
HOST_GID=${HOST_GID}
HOST_USER=${HOST_USER}
HOST_GROUP=${HOST_GROUP}
HOME=${HOME}
EOF

echo "Created .env with: HOST_UID=$HOST_UID HOST_GID=$HOST_GID HOST_USER=$HOST_USER"
echo 'Checking Docker socket permissions...'
ls -la /var/run/docker.sock
