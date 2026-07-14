FROM python:3.12-slim

WORKDIR /app

COPY pyproject.toml README.md LICENSE ./
COPY src ./src
COPY pwa ./pwa
COPY scripts ./scripts

RUN pip install --no-cache-dir .

# /data (SQLite, cancel table) and /app/config.yaml are provided as volumes.
EXPOSE 8300

CMD ["trainpunct", "serve", "--host", "0.0.0.0", "--port", "8300"]
