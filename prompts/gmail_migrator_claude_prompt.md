# One‑Shot Prompt: Build a High‑Throughput Gmail→Gmail Migrator in Python

You are Claude Code. Generate a **production-grade, high‑throughput Gmail→Gmail migration tool** in Python as a complete, runnable repository. Follow the spec precisely. Output **all files** with correct paths and contents. Avoid long prose; include brief README instructions only.

---

## Goals
- Copy **all emails** (and their labels, read/starred state, dates) from a **source** Gmail account to a **destination** Gmail account **as fast as safely possible** via the official Gmail API.
- Be **resumable**, **idempotent**, and **fault tolerant** with aggressive retry+backoff, QPS throttling, and persistent checkpointing.
- Preserve message `internalDate` (use `internalDateSource=receivedTime`), labels (system + custom), and threading (Gmail will re-thread on import).

---

## Tech & Constraints
- Language: **Python 3.11**.
- Packaging: plain repo with `requirements.txt` (no poetry/pipenv).
- OAuth & API: `google-api-python-client`, `google-auth`, `google-auth-oauthlib`, `google-auth-httplib2`.
- Concurrency: **ThreadPoolExecutor** with **one Gmail service per worker**.
- Resumability: **SQLite** checkpoint DB.
- Logging: human-readable console + CSV of moved items + structured error log.
- Code style: **full, descriptive variable names** (no single-letter variables). Prefer explicitness over cleverness.
- CLI: single entrypoint `main.py` that takes `--config path/to/config.yaml`.
- License: MIT.

---

## Repository Layout (exact)
```
gmail-migrator/
├─ README.md
├─ requirements.txt
├─ config.example.yaml
├─ main.py
├─ migrator/
│  ├─ __init__.py
│  ├─ auth.py
│  ├─ gmail_client.py
│  ├─ label_mapper.py
│  ├─ lister.py
│  ├─ downloader.py
│  ├─ uploader.py
│  ├─ mover.py
│  ├─ checkpoint.py
│  ├─ ratelimit.py
│  ├─ metrics.py
│  └─ util.py
└─ state/              # created at runtime
```

---

## Functional Requirements

### Authentication
- Implement `build_gmail_service(credentials_file, token_file, scopes)` in `migrator/auth.py`.
- Scopes:
  - Source: `gmail.readonly`
  - Destination: `gmail.insert` and `gmail.labels`
- Store tokens in the provided `token_file`. Refresh as needed.

### Listing
- `MessageLister.iter_ids()` must support chunking to control memory/retries:
  - `chunk_by: date | label | none`
  - If `date`: iterate windows of `chunk_size_days`, using `after:`/`before:` query modifiers.
  - Accept a base `query` from config; append date windows if applicable.
- Page through `users.messages.list` using `maxResults` from config.

### Downloading
- `GmailClient.get_raw_message(message_id, format='raw')` returns RFC‑822 bytes (base64url‑decoded).
- Include a method to fetch **labels** for a message (minimal extra call if needed): retain system flags like `UNREAD`, `STARRED`, `IMPORTANT`.
- Use a **Downloader** with a `ThreadPoolExecutor(max_workers)`. Inject a shared **TokenBucket** for QPS throttling and **Backoff** for retries.

### Uploading
- `GmailClient.import_raw(raw_bytes, label_ids, internal_date_source)` calls `users.messages.import` with `internalDateSource='receivedTime'` and `neverMarkSpam=True`.
- `LabelMapper`:
  - Build a map from **source label names → destination labelIds**.
  - Create missing destination labels up front via `users.labels.create`.
  - Preserve system labels when `preserve_system_labels=true` and custom labels when `preserve_custom_labels=true`.
  - Optional `add_label_on_destination` is created/added to each imported message.
- `Uploader.upload_many(...)` streams imports in parallel, mapping labels and honoring throttle/backoff.

### Checkpointing
- SQLite file: `state/checkpoints.sqlite`. Create tables if missing:
  - `progress(message_id PRIMARY KEY, status TEXT, dest_message_id TEXT, attempts INT, last_error TEXT, size_bytes INT, sha256 TEXT)`
  - `metadata(key TEXT PRIMARY KEY, value TEXT)`
- Expose methods: `mark_enqueued`, `mark_downloaded`, `mark_uploaded`, `mark_failed`, `pending_ids(limit)`, `already_done(message_id)`.

### Rate Limiting & Backoff
- `TokenBucket` supporting target `qps` with jitter `[min_ms, max_ms]` from config.
- `Backoff` for retryable errors (`rateLimitExceeded`, `userRateLimitExceeded`, `backendError`, `internalError`, timeouts). Use exponential backoff with full jitter and a max cap.

### Metrics
- Track moving throughput: messages/sec, bytes/sec, success/fail counts, ETA.
- Display a `tqdm` progress bar or periodic log lines (avoid verbose per-message logs).

### Mover Orchestration
- `MessageMover.run()` should:
  1. Build the label map once.
  2. Iterate chunks of message IDs from `MessageLister`.
  3. Submit download tasks with bounded in‑flight limit (`max_inflight`).
  4. As downloads complete, immediately enqueue uploads (streaming pipeline).
  5. On retryable errors, back off and requeue; on fatal, record in checkpoint + error log and continue.
  6. Periodically persist progress, update metrics, and print status lines.

### Configuration
Provide `config.example.yaml` with the exact keys below (and read them in `main.py`):
```yaml
source_account:
  credentials_file: source_oauth.json
  token_file: source_token.json
  query: ""                 # e.g., newer_than:2y OR label:Important
destination_account:
  credentials_file: dest_oauth.json
  token_file: dest_token.json

migration:
  max_workers: 16
  page_size: 500
  max_inflight: 2000
  preserve_system_labels: true
  preserve_custom_labels: true
  add_label_on_destination: "MIGRATED"
  internal_date_source: "receivedTime"
  throttle_qps: 8
  jitter_ms: [50, 250]
  chunk_by: "date"          # date | label | none
  chunk_size_days: 30

logging:
  level: INFO
  csv_log_file: moved.csv
  error_log_file: errors.log
```

### Logging & Auditing
- Write a CSV `moved.csv` with columns: `source_message_id,destination_message_id,size_bytes,sha256,status,attempts`.
- Append an error line in `errors.log` with timestamp, message_id, exception name, and text.

### Safety & Integrity
- Respect Gmail max raw message size (~35 MB). For larger messages, log and skip with reason.
- Preserve `UNREAD`/`STARRED` when present. System labels like `INBOX`, `SENT`, etc., should be applied if configured.
- Ensure graceful shutdown: on SIGINT, flush queues, commit checkpoints, and exit cleanly.

---

## Non‑Functional Requirements
- Deterministic, readable code with docstrings and type hints.
- Separate pure helpers in `util.py` (`base64url_encode/decode`, `chunked`, `now_ms`).
- Tests are optional; if included, add a minimal sanity test for util and checkpoint modules.

---

## README Content (concise, but include)
- What it does
- Setup steps:
  1. Create two Google Cloud OAuth client credentials (desktop app), download as JSON for **source** and **destination**.
  2. Place them as `source_oauth.json` and `dest_oauth.json` (or update config).
  3. `python -m venv .venv && source .venv/bin/activate`
  4. `pip install -r requirements.txt`
  5. Copy `config.example.yaml` → `config.yaml` and edit.
  6. Run: `python main.py --config config.yaml`
- Notes on quotas, best practices (chunking, backoff), and how to resume a stopped run.

---

## Deliverables
Produce the **full repository** exactly as described, with all Python modules implemented and ready to run. Do not omit files. Keep explanations brief. Use explicit variable names (no single-letter variables).

**Begin by printing the repository tree, then emit each file with a fenced code block and the correct path.** Avoid extraneous commentary.
