# One-Shot Build Prompt --- **Film Game** (Multiplayer Mobile Trivia)

You are an expert full-stack engineer. Generate a complete,
production-ready implementation for **Film Game**, a turn-based,
multiplayer mobile trivia game about films.

## High-Level

-   **Mobile**: Flutter (Dart) app for iOS/Android.
-   **Backend**: PHP **CodeIgniter 4** + **MySQL 8** (match my org
    conventions).
-   **Auth**: Email+password, JWT access+refresh tokens, secure password
    hashing (bcrypt/argon2id).
-   **Infra**: Docker Compose (PHP-FPM, Nginx, MySQL, Mailhog), `.env`
    configs, Makefile shortcuts.
-   **Realtime**: Simple polling endpoints first; provide optional
    WebSocket gateway (PHP Ratchet or Node/Socket.IO) behind a feature
    flag.
-   **Testing**: PHPUnit for PHP, flutter_test for app. Seed data +
    fixtures.
-   **Lint/Format**: PHP-CS-Fixer, Dart `flutter format`.
-   **Security**: HTTPS-ready, CSRF for web forms, rate limiting, input
    validation, prepared statements, authZ checks, audit logs.

## Game Rules (must implement)

-   Players take turns answering a multiple-choice film question.
-   **Scoring**: Correct +1, incorrect −1.
-   **Actions on turn**:
    1)  **Answer** (score as above)
    2)  **Challenge**: challenge the player who sent the question
        (record challenge state; server resolves outcome rules below)
    3)  **50/50**: removes half of the options
    4)  **Bomb**: skip (discard) the question and fetch the next
-   **Players list** inside each game: name, avatar, online/offline,
    current level, current score.
-   **Store**: buy power-ups (50/50, Bomb) with soft currency. Persist
    inventory & transactions.
-   **Current Games**: a screen to list ongoing games with metadata
    (name, created_at, status), tap to open details.

## UX / Navigation (Flutter)

Main navigation with: **Create Account**, **Login**, **Main Menu**,
**New Game**, **Profile**.\
Sub-menus: - **New Game**: choose opponents (friend search or invite by
email), choose category "Films", set game name, start. - **Profile**:
edit avatar, view stats (level, total games, win/loss, accuracy),
purchase history, inventory. - **Current Games**: list (game_name,
created_at date, status: waiting/your turn/opponent turn/completed).
Item tap → **Game Details**: - Players list (avatar, name,
online/offline, level, score) - Turn timeline (last 10 moves) - **Your
Turn** view: question stem, 4 options, actions
(Answer/Challenge/50-50/Bomb), power-up inventory count, remaining time
(client countdown).

... (The rest includes full API spec, database schema, controllers,
models, services, seed data, security, acceptance tests, etc. --- copied
entirely from the long prompt I wrote above.)
