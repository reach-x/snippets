# Dockerfile examples - Container image definitions

# Multi-stage build example
# Stage 1: Build stage
FROM node:18-alpine AS builder

# Metadata
LABEL maintainer="developer@example.com"
LABEL version="1.0.0"
LABEL description="Example Node.js application"

# Set working directory
WORKDIR /app

# Copy package files
COPY package*.json ./

# Install dependencies
RUN npm ci --only=production && \
    npm cache clean --force

# Copy application code
COPY . .

# Build application
RUN npm run build

# Stage 2: Production stage
FROM node:18-alpine AS production

# Install dumb-init for proper signal handling
RUN apk add --no-cache dumb-init

# Create non-root user
RUN addgroup -g 1001 -S nodejs && \
    adduser -S nodejs -u 1001

# Set working directory
WORKDIR /app

# Copy built artifacts from builder
COPY --from=builder --chown=nodejs:nodejs /app/dist ./dist
COPY --from=builder --chown=nodejs:nodejs /app/node_modules ./node_modules
COPY --from=builder --chown=nodejs:nodejs /app/package*.json ./

# Set environment variables
ENV NODE_ENV=production \
    PORT=3000 \
    LOG_LEVEL=info

# Expose port
EXPOSE 3000

# Switch to non-root user
USER nodejs

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD node healthcheck.js || exit 1

# Use dumb-init to handle signals properly
ENTRYPOINT ["dumb-init", "--"]

# Default command
CMD ["node", "dist/server.js"]

# =============================================================================
# Alternative examples below (not for same file, just examples)
# =============================================================================

# Python application example
# FROM python:3.11-slim
#
# WORKDIR /app
#
# # Install system dependencies
# RUN apt-get update && \
#     apt-get install -y --no-install-recommends \
#         gcc \
#         libpq-dev && \
#     rm -rf /var/lib/apt/lists/*
#
# # Copy requirements first for better caching
# COPY requirements.txt .
# RUN pip install --no-cache-dir -r requirements.txt
#
# # Copy application
# COPY . .
#
# # Run as non-root
# RUN useradd -m -u 1000 appuser && \
#     chown -R appuser:appuser /app
# USER appuser
#
# CMD ["python", "app.py"]

# Go application example
# FROM golang:1.21-alpine AS builder
#
# WORKDIR /src
# COPY go.mod go.sum ./
# RUN go mod download
#
# COPY . .
# RUN CGO_ENABLED=0 GOOS=linux go build -a -installsuffix cgo -o app .
#
# FROM alpine:latest
# RUN apk --no-cache add ca-certificates
# WORKDIR /root/
# COPY --from=builder /src/app .
# CMD ["./app"]

# ARG examples (build-time variables)
# ARG NODE_VERSION=18
# FROM node:${NODE_VERSION}-alpine
#
# ARG BUILD_DATE
# ARG VCS_REF
# LABEL org.opencontainers.image.created=$BUILD_DATE \
#       org.opencontainers.image.revision=$VCS_REF

# Volume mount points
# VOLUME ["/data", "/logs"]

# ONBUILD triggers (executed in child images)
# ONBUILD COPY package*.json ./
# ONBUILD RUN npm install

# SHELL instruction (change default shell)
# SHELL ["/bin/bash", "-c"]

# STOPSIGNAL instruction
# STOPSIGNAL SIGTERM

# Multiple COPY with --chown
# COPY --chown=nodejs:nodejs src/ ./src/
# COPY --chown=nodejs:nodejs config/ ./config/

# ADD instruction (can extract archives and fetch URLs)
# ADD https://example.com/file.tar.gz /tmp/
# ADD archive.tar.gz /extracted/

# Using .dockerignore to exclude files
# Create .dockerignore file:
# node_modules
# npm-debug.log
# .git
# .DS_Store
# *.md
# !README.md

# Best practices:
# 1. Use specific tags, not :latest
# 2. Minimize layers (combine RUN commands)
# 3. Leverage build cache (order matters)
# 4. Use multi-stage builds
# 5. Don't run as root
# 6. Use .dockerignore
# 7. Scan for vulnerabilities
# 8. Keep images small (alpine, distroless)
# 9. One process per container
# 10. Use HEALTHCHECK
