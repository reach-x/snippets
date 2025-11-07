=== ONE‑SHOT PROMPT FOR CLAUDE: Build “Temp Monitor Pro” (iOS Weather Alerts) ===

You are a senior iOS engineer. Generate a complete, production‑quality SwiftUI app named **Temp Monitor Pro** (bundle id
suggestion: `com.tempmonitorpro.app`) that monitors outdoor temperature for multiple locations and fires **local
notifications** when the temperature **enters a configured range** while **warming up or cooling down**. The domain is *
*TempMonitorPro.com** (use it for any in‑app links like Privacy/Support placeholders).

Your output must include **all source files**, **Info.plist entries**, **entitlements**, and a **README** with setup
steps. Return files as a clearly structured listing with **file paths** and **code blocks** for each file. Do not omit
code. Keep code self‑contained (no external services besides Apple frameworks).

---

## Functional Requirements

1. **Monitors**
    - Support up to **10 saved locations** (user‑chosen) **plus an optional Current Location monitor**.
    - Each monitor has a **temperature range [X, Y]** (in °F or °C) and a **direction filter**:
        - `warming` (temp rising and entering range),
        - `cooling` (temp falling and entering range),
        - `any` (either).
    - Entering‑range logic: trigger once when the current temperature transitions from **outside** to **inside** the
      range and the **direction condition** is satisfied compared to the last sampled temperature.
    - Include **per‑monitor enable/disable** and **per‑monitor custom alert message** (optional).

2. **Default & Custom Alert Message**
    - Default template (when no custom message is set):
        - `"$Location has (warmed up or cooled down) to $degrees"`
            - Substitute `(warmed up or cooled down)` with “warmed up” or “cooled down” based on trend.
            - `$Location` → monitor’s display name.
            - `$degrees` → current temperature with unit (e.g., `72°F`).
    - Support additional tokens if present in a custom template: `$unit`, `$low`, `$high`, `$trend`, `$timestamp` (
      ISO‑8601 local). Unknown tokens are left as‑is.

3. **Sampling Cadence**
    - **Target**: check temps **every 15 minutes**. Implement with **BGTaskScheduler** (app refresh) and **Background
      Fetch** as a fallback. Set earliest begin ≈ 15 minutes; acknowledge that iOS does not guarantee exact cadence.
    - When the **Current Location** monitor is enabled, also start **significant location change** updates to re‑sample
      upon movement.

4. **Weather Source**
    - Use **Apple WeatherKit** (`WeatherService`) to fetch current temperature for a coordinate.
    - Handle failures/retries gracefully; fail a monitor without crashing the whole batch.
    - Provide a **MockWeatherProvider** (compile‑time selectable) for previews/tests.

5. **Units**
    - Global setting: **Fahrenheit (default)** or **Celsius**. Convert as needed for range checks and display.
    - Store raw values and units safely; avoid double‑converting.

6. **Persistence**
    - Persist monitors and app settings in `UserDefaults` via `Codable`. Keep a small state per monitor:
        - last temperature value (double + unit),
        - last sample date,
        - previous in‑range flag (to prevent duplicate alerts until exiting and re‑entering).

7. **Notifications**
    - Local notifications via `UNUserNotificationCenter`. Request permission at first launch flow.
    - Rate‑limit: only notify on **enter range**; require exit before notifying again.
    - Provide a **Test Notification** button per monitor to preview the template.

8. **UI (SwiftUI)**
    - iOS 16+ minimum. Single‑scene app.
    - **Tab 1 — Monitors:** List of all monitors with: name, latest temp, status (Inside/Outside), last updated time,
      toggle enabled. Add/Edit screens include:
        - Location picker (search by place name using MapKit), or manual lat/long.
        - Range inputs (X..Y) with validation (X ≤ Y).
        - Direction (warming/cooling/any).
        - Custom message (optional, multiline).
        - “Save” and “Delete” (when editing).
    - **Tab 2 — Settings:**
        - Units (°F/°C), background refresh info, toggle for **Enable Current Location monitor**.
        - When enabling Current Location, request `WhenInUse` then `Always` (explain why).
        - Interval selector (15 / 30 / 60 minutes) to adjust **earliest begin** (document iOS limits).
        - Buttons: “Run Check Now”, “Export Monitors (JSON)”, “Import Monitors (JSON)”.

9. **Location Search**
    - Use **MapKit** search to resolve place names to coordinates.
    - Display a short **display name** (e.g., “San Francisco, CA”) pulled via reverse‑geocoding.

10. **Error Handling & Edge Cases**
    - If last temperature is unavailable, sample without alerting (need a baseline).
    - If WeatherKit returns in a different unit, convert correctly.
    - Gracefully handle denied location permissions (hide Current Location option).
    - Avoid alert spam at app launch by using a “first sample primes state, no alert” rule.

11. **Testing/Preview**
    - Add a preprocessor flag (e.g., `USE_MOCK_WEATHER`) to switch providers.
    - Include a small `UnitTests` target for range‑entry logic and template rendering.

---

## Technical Requirements

- **Frameworks**: SwiftUI, WeatherKit, CoreLocation, MapKit, UserNotifications, BackgroundTasks.
- **Deployment Target**: iOS 16.4+ (WeatherKit).
- **Code Style**: Swift Concurrency (`async/await`), `ObservableObject` view models, `@MainActor` where appropriate.
- **Architecture**: Light MVVM.
- **Persistence**: `UserDefaults` with a small wrapper; JSON export/import.
- **Concurrency**: Limit concurrent WeatherKit calls (e.g., 2 at a time) and add jitter to avoid thundering herd.
- **Background**: Register `BGAppRefreshTask` identifier `com.tempmonitorpro.app.refresh` and reschedule on completion.
  Also set `setMinimumBackgroundFetchInterval(15*60)` in `UIApplicationDelegate` integration.
- **Permissions / Capabilities**:
    - WeatherKit capability enabled.
    - Location: `NSLocationWhenInUseUsageDescription`, `NSLocationAlwaysAndWhenInUseUsageDescription`.
    - Background modes: **fetch** and **location**.
    - `BGTaskSchedulerPermittedIdentifiers` includes the refresh task id.

---

## Implementation Details (must implement)

1. **Models**
    - `TemperatureUnit: String, Codable` (fahrenheit, celsius).
    - `Trend: String, Codable` (warming, cooling, steady).
    - `TempRange: Codable` { `low: Double`, `high: Double` } with clamped/ordered init.
    - `DirectionFilter: String, Codable` (warming, cooling, any).
    - `MonitoredLocation: Codable, Identifiable` with fields:
        - `id: UUID`
        - `name: String`
        - `latitude: Double`, `longitude: Double`
        - `range: TempRange`
        - `direction: DirectionFilter`
        - `enabled: Bool`
        - `customMessage: String?`
        - `lastSample: Date?`
        - `lastTempValue: Double?`
        - `lastTempUnit: TemperatureUnit?`
        - `wasInsideRange: Bool`
    - `AppSettings: Codable` { `unit: TemperatureUnit`, `checkIntervalMinutes: Int`, `currentLocationEnabled: Bool` }

2. **Services**
    - `WeatherProvider` protocol →
      `currentTemperature(for coordinate: CLLocationCoordinate2D, in unit: TemperatureUnit) async throws -> (value: Double, unit: TemperatureUnit)`
    - `WeatherKitProvider` implementation via `WeatherService.shared.weather(for:)` (read `currentWeather.temperature`).
    - `MockWeatherProvider` returning deterministic or random temps for previews/tests.
    - `LocationService` (CoreLocation) for current coordinate + reverse geocoding.
    - `NotificationService` to request auth and schedule local notifications.
    - `PersistenceService` wrapping `UserDefaults` (`load/save monitors`, `load/save settings`, `export/import JSON`).
    - `SchedulerService` to register/schedule and run the background job:
        - Register `BGAppRefreshTask` handler.
        - Public `scheduleNext(in minutes: Int)` (sets earliest begin to that many minutes).
        - Public `runNow()` to trigger an immediate check (foreground).

3. **Range‑Entry Evaluation**
    - Helper
      `evaluateEntry(previous: Double?, current: Double, range: TempRange, direction: DirectionFilter) -> (entered: Bool, trend: Trend)`:
        - If `previous == nil` → return `(false, .steady)`.
        - Compute `wasInside = previous.map { inRange($0) } ?? false`, `isInside = inRange(current)`.
        - `trend = current > (previous ?? current) ? .warming : (current < previous! ? .cooling : .steady)`.
        -
        `entered = (!wasInside && isInside) && (direction == .any || (direction == .warming && trend == .warming) || (direction == .cooling && trend == .cooling))`.

4. **Template Rendering**
    - `render(template: String?, context: AlertContext) -> String` with default string:
        - `"$Location has (warmed up or cooled down) to $degrees"`
    - Replace tokens: `$Location`, `$degrees` (`Int` rounded + unit), `$unit`, `$low`, `$high`, `$trend`, `$timestamp`.
    - Replace the `(warmed up or cooled down)` parenthetical with the detected trend phrase.

5. **View Models**
    - `MonitorsViewModel` (ObservableObject)
        - Publishes `[MonitoredLocation]`, loading/saving via persistence.
        - Provides `add`, `update`, `remove`, `toggleEnabled`.
        - `sampleAll(provider:)` that fetches temps for enabled monitors (and current location if enabled), applies
          evaluation, updates state, and fires notifications when needed.
        - Concurrency: `TaskGroup` with small concurrency limit.
    - `SettingsViewModel` for app settings, notification permission status, manual run, import/export.

6. **UI**
    - `ContentView` with `TabView` → `MonitorsListView` and `SettingsView`.
    - `MonitorsListView`: list + toolbar “Add Monitor” (presents `AddEditMonitorView`).
    - `AddEditMonitorView`: picker for direction, text fields for range, location search view, custom message textarea,
      validation UX.
    - `LocationSearchView`: MapKit search + result selection + reverse‑geocode name.
    - Accessibility labels and Dynamic Type support.

7. **App Lifecycle**
    - `@main` app integrates an `AppDelegate` via `UIApplicationDelegateAdaptor` to set background fetch interval and
      register BGTask.
    - Reschedule refresh on app foreground/background transitions.

8. **Info.plist (include exact keys)**
    - `NSLocationWhenInUseUsageDescription` — “Temp Monitor Pro uses your location to check weather for your area.”
    - `NSLocationAlwaysAndWhenInUseUsageDescription` — “Allow background temperature checks for your current location.”
    - `UIBackgroundModes` → `fetch`, `location`
    - `BGTaskSchedulerPermittedIdentifiers` → array with `com.tempmonitorpro.app.refresh`

9. **Entitlements**
    - Enable **WeatherKit** capability (`com.apple.developer.weatherkit` true). Provide a `.entitlements` file example.

10. **Unit Tests**
- Tests for `evaluateEntry` and template rendering.

---

## Output Format (strict)

Return a hierarchical listing with these top‑level entries and full code for each:

- `TempMonitorPro/TempMonitorProApp.swift`
- `TempMonitorPro/AppDelegate.swift`
- `TempMonitorPro/Models/` (all model files)
- `TempMonitorPro/Services/` (weather, location, notifications, persistence, scheduler)
- `TempMonitorPro/ViewModels/` (monitors + settings)
- `TempMonitorPro/Views/` (content + list + add/edit + search + settings)
- `TempMonitorPro/Support/Info.plist` (full XML)
- `TempMonitorPro/Support/TempMonitorPro.entitlements` (full XML/Plist)
- `TempMonitorPro/Preview Content/` (if used)
- `README.md` (setup/WeatherKit notes, background scheduling caveats, build/run steps)
- `TempMonitorProTests/…` (at least 2 unit test files)

For each Swift file, start with a comment header block describing the file purpose. Include import statements. Use
clear, descriptive variable names (no single‑letter names).

---

## Setup & Build Notes (include in README you output)

1. **Xcode** 15+. iOS Deployment Target: 16.4+.
2. **Capabilities**: Add **WeatherKit**, **Background Modes** (Fetch, Location). Ensure Team is selected so WeatherKit
   entitlement activates.
3. **WeatherKit**: On first run, device should be signed into iCloud; add your bundle identifier to the Apple Developer
   WeatherKit configuration.
4. **Location**: App will request When‑In‑Use; enabling Current Location monitor asks for Always permission.
5. **Background**: iOS does not guarantee exact 15‑minute intervals; the app requests every 15 minutes via BGTask and
   Background Fetch. For strict timing, consider adding a server that pushes via APNs (out of scope here).
6. **Testing**: Use the `Run Check Now` button or set `USE_MOCK_WEATHER` to simulate temps.

---

## Small UX Touches (implement)

- Input sanitization for range fields; show validation error if X > Y.
- Friendly inline explanations in Settings re: background timing limits.
- Confirm before deleting a monitor.
- Pull‑to‑refresh on Monitors list runs a sampling pass in foreground.
- Badge count reflects number of monitors currently inside range.

---

## Deliverables Summary (what you must return)

- Full SwiftUI codebase with the structure above.
- All required plists/entitlements.
- README with precise step‑by‑step setup and screenshots guidance (annotate where necessary).
- Unit tests demonstrating the range‑entry algorithm and template engine.

Return the full project as code blocks in your response, ready to copy into a new Xcode project.

=== END OF ONE‑SHOT PROMPT ===
