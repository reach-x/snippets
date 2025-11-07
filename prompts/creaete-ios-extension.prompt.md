You are a senior iOS engineer. Generate a complete, ready-to-build Xcode 16 project (Swift 6, iOS 18 minimum) that
implements Apple’s **Live Caller ID Lookup** client using the **IdentityLookup** framework and **ExtensionKit**.

### Goals

- A SwiftUI **container app** ("CallShield") that lets a user configure:
    - Service URL (PIR service base, used for `/config`, `/key`, `/queries`)
    - Token Issuer URL (base; system will fetch `/.well-known/private-token-issuer-directory` and then
      `/issue/token-key-for-user-token` and `/issue`)
    - User Tier Token (opaque bearer token string)
- A **Live Caller ID Lookup app extension** ("CallShieldLiveLookup") that:
    - Conforms to `LiveCallerIDLookupProtocol`
    - Exposes `var context: LiveCallerIDLookupExtensionContext { get }` returning:
      `LiveCallerIDLookupExtensionContext(serviceURL:tokenIssuerURL:userTierToken:)`
- The container app stores config in **Keychain** and **App Group** shared defaults; the extension reads from that App
  Group.
- App UI includes buttons to:
    1) **Open Settings** to enable the extension (`LiveCallerIDLookupManager.shared.openSettings()`)
    2) **Refresh PIR parameters** (`refreshPIRParameters(forExtensionWithIdentifier:)`)
    3) **Refresh extension context** after saving config (`refreshExtensionContext(forExtensionWithIdentifier:)`)
    4) **Reset caches** (`reset(forExtensionWithIdentifier:)`)
    5) **Show status** (`status(forExtensionWithIdentifier:)`) and render the enum as text
- Provide a tiny **status panel** that surfaces errors from the above calls.

### Project shape

- Workspace: none. Single iOS app project with an **ExtensionKit** extension target.
- Bundle IDs (use placeholders that I can `Find/Replace` later):
    - App: `com.yourcompany.callshield`
    - Extension: `com.yourcompany.callshield.livelookup`
- **App Group ID**: `group.com.yourcompany.callshield`
- **Deployment target**: iOS 18.0
- **Swift language mode**: Swift 6 strict concurrency
- No third‑party packages.

### Source layout (show a file tree and then full contents for EVERY file)

- CallShield/
    - CallShieldApp.swift (SwiftUI app entry)
    - Views/SettingsView.swift (form fields + action buttons)
    - Model/ExtensionConfig.swift (Codable config)
    - Model/SharedDefaults.swift (App Group UserDefaults helper)
    - Security/KeychainStore.swift (save/load user-tier token securely)
    - Managers/ExtensionCoordinator.swift (wraps LiveCallerIDLookupManager APIs)
    - CallShield.entitlements (App Group)
    - Info.plist (app)
- CallShieldLiveLookup/
    - LiveLookupExtension.swift (the @main entry conforming to LiveCallerIDLookupProtocol; reads config from App Group +
      Keychain and returns `LiveCallerIDLookupExtensionContext`)
    - CallShieldLiveLookup.entitlements (App Group)
    - Info.plist (extension; **must** use ExtensionKit keys)
- Project settings: ensure the extension product is embedded under **Extensions/** (ExtensionKit), not **PlugIns/**.

### Implementation details & constraints

1) **IdentityLookup & manager calls**
    - Import `IdentityLookup` where needed.
    - Provide a constant `let liveLookupExtensionBundleIdentifier = "com.yourcompany.callshield.livelookup"` used by
      ExtensionCoordinator.
    - ExtensionCoordinator methods (async/await):
        - `openSettings()`
        - `refreshPIRParameters()`
        - `refreshExtensionContext()`
        - `resetCaches()`
        - `currentStatus() -> CallLookupExtensionStatus`
    - All manager calls must handle and bubble errors to the UI.

2) **Context construction (extension)**
    - In `@main final class LiveLookupExtension: LiveCallerIDLookupProtocol`, expose:
      ```swift
      var context: LiveCallerIDLookupExtensionContext {
          LiveCallerIDLookupExtensionContext(
              serviceURL: ConfigProvider.shared.serviceURL(),
              tokenIssuerURL: ConfigProvider.shared.tokenIssuerURL(),
              userTierToken: ConfigProvider.shared.userTierTokenData()
          )
      }
      ```
    - `ConfigProvider` reads from:
        - App Group defaults keys:
            - `service_url` (String)
            - `token_issuer_url` (String)
        - Keychain (account: `callshield.user-tier-token`)
    - Provide sensible **fallbacks** (guarded) that fatalError with a clear developer message if URLs are invalid or
      token missing.

3) **Settings UI (container app)**
    - SwiftUI form with three fields (Service URL, Token Issuer URL, User Tier Token) and five buttons:
        - Save & Refresh Context
        - Refresh PIR Parameters
        - Reset Caches
        - Check Status
        - Open Settings
    - After “Save & Refresh Context”, persist values (App Group + Keychain), then call `refreshExtensionContext()`.
    - Show current `CallLookupExtensionStatus` (e.g., `.enabled`, `.disabled`, etc.) as a labeled badge.

4) **Persistence**
    - `SharedDefaults` wrapper around `UserDefaults(suiteName: "group.com.yourcompany.callshield")`
    - `KeychainStore` using `kSecClassGenericPassword` (service: `com.yourcompany.callshield`, account:
      `callshield.user-tier-token`) with add/update/query/delete helpers. Don’t crash on missing token; show a
      user-facing “Token missing” warning in UI.

5) **Info.plists & Entitlements**
    - **App** Info.plist: standard iOS app keys.
    - **Extension** Info.plist uses **ExtensionKit** keys (no NSExtension dictionary):
      ```xml
      <key>EXAppExtensionAttributes</key>
      <dict>
        <key>EXExtensionPointIdentifier</key>
        <string>com.apple.live-lookup</string>
      </dict>
      ```
    - Both app and extension **entitlements** include the shared App Group `group.com.yourcompany.callshield`.

6) **Acceptance criteria**
    - Builds clean in Xcode 16 with Swift 6.
    - The app launches to SettingsView.
    - Pressing “Open Settings” opens the Phone > Call Blocking & Identification area (or the Phone settings page if that
      is what the current OS behavior supports).
    - After entering values and “Save & Refresh Context”, extension context updates without reinstalling.
    - “Refresh PIR Parameters” and “Reset Caches” calls complete without runtime errors.
    - The extension’s `@main` entry compiles and returns a valid `LiveCallerIDLookupExtensionContext`.

7) **Testing notes (include in README inside the project)**
    - The iOS system will query your service for **PIR** lookups at ring time via Apple’s relay. Implement the server
      endpoints `/config`, `/key`, `/queries` as described by Apple.
    - For the token issuer, the system will access `/.well-known/private-token-issuer-directory` at the **tokenIssuerURL
      ** base, then `.../issue/token-key-for-user-token`, and `.../issue`. (Document these paths in the README; do not
      implement client networking in the app—only provide the URLs and bearer token.)
    - Dataset naming convention on server: use the **extension bundle identifier** with suffixes `.block` and
      `.identity` for demo/testing (e.g., `com.yourcompany.callshield.livelookup.block` and `.identity`).

### Output format

- Start with a project overview and file tree.
- Then provide **full, self-contained code blocks** for EVERY file (no “…” placeholders).
- Include a brief README.md in the project root summarizing setup, where to set the App Group ID and bundle IDs, and how
  to run Apple’s example PIR service for testing.

### Style

- Idiomatic Swift 6 (strict concurrency).
- Clear, descriptive type and variable names.
- Inline comments where it helps future maintainers.
- No external dependencies.
