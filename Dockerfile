# Telamon-generated Dockerfile for coboltwin
# Stages: base → claude (development with Claude Code)
# Legacy stack: GnuCOBOL + Java/Maven + Python

ARG JAVA_VERSION=21
ARG UID=1000
ARG GID=1000

# ==============================================================================
# BASE - COBOL + Java + Maven + Python development environment
# ==============================================================================
FROM eclipse-temurin:${JAVA_VERSION}-jdk-jammy AS base

ARG UID=1000
ARG GID=1000

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

ENV DEBIAN_FRONTEND=noninteractive

# System packages + GnuCOBOL + Python + Maven
RUN apt-get update && apt-get install --no-install-recommends -y \
    gnucobol \
    libcob4-dev \
    maven \
    python3 \
    python3-venv \
    python3-pip \
    curl \
    ca-certificates \
    git \
    openssh-client \
    vim \
    less \
    make \
    build-essential \
    unzip \
    zip \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN groupadd -g ${GID} dev \
    && useradd -m -u ${UID} -g ${GID} -s /bin/bash dev \
    && echo 'dev ALL=(ALL) NOPASSWD: ALL' >> /etc/sudoers

USER dev
WORKDIR /home/dev

# Shell environment setup
ENV BASH_ENV=/home/dev/.bash_env
RUN touch "$BASH_ENV" && echo '. "$BASH_ENV"' >> "$HOME/.bashrc"

# Python virtual environment
ENV VIRTUAL_ENV=/home/dev/venv
RUN python3 -m venv $VIRTUAL_ENV
ENV PATH="$VIRTUAL_ENV/bin:$PATH"

RUN echo 'export VIRTUAL_ENV=/home/dev/venv' >> "$BASH_ENV" \
    && echo 'export PATH="$VIRTUAL_ENV/bin:$PATH"' >> "$BASH_ENV"

RUN pip install --upgrade pip setuptools wheel

WORKDIR /src

# Install Python dependencies if present
COPY --chown=dev:dev requirements.tx[t] .
RUN if [ -f requirements.txt ]; then pip install --no-cache-dir -r requirements.txt; fi

CMD ["bash"]

# ==============================================================================
# CLAUDE - Development + Claude Code for LLM-assisted work
# ==============================================================================
FROM base AS claude

# Node.js via nvm (for Claude Code)
ENV NODE_VERSION=24.11.1
ENV NVM_DIR=/home/dev/.nvm

RUN curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.3/install.sh | bash \
    && . $NVM_DIR/nvm.sh \
    && nvm install $NODE_VERSION \
    && nvm alias default $NODE_VERSION

RUN echo 'export NVM_DIR="$HOME/.nvm"' >> "$BASH_ENV" \
    && echo '[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"' >> "$BASH_ENV"

# Install Claude Code natively
RUN curl -fsSL https://claude.ai/install.sh | bash \
    && echo 'export PATH="$HOME/.local/bin:$PATH"' >> "$BASH_ENV"

# Git configuration
RUN echo 'git config --global core.sshCommand "ssh -i /home/dev/.ssh/id_ed25519"' >> "$BASH_ENV" \
    && echo 'git config --global --add safe.directory /src' >> "$BASH_ENV"

CMD ["bash"]