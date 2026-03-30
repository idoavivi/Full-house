FROM node:20-alpine

WORKDIR /app

# Install dependencies first (for Docker layer caching)
COPY package*.json ./
RUN npm ci --only=production=false

# Copy source and build
COPY tsconfig.json ./
COPY src/ ./src/
RUN npm run build

# Remove dev dependencies
RUN npm prune --production

# Create persistent data directory
RUN mkdir -p /data

ENV DB_PATH=/data/babynight.db
ENV NODE_ENV=production
ENV PORT=3000

EXPOSE 3000

CMD ["node", "dist/index.js"]
