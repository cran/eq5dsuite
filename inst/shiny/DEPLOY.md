# Deploying the eq5dsuite Shiny app on Shiny Server

For the open-source edition of Shiny Server, serving the app at its own
address. Other platforms are covered briefly at the end.

The app runs in one of two modes.

**Locally** — the default — it is a tool on your own machine. There are no
limits, nothing is hidden, and your data never leave the computer.

**Online** it is a shared service. It limits what can be uploaded, keeps every
file it writes inside a folder belonging to that session alone, ends idle
sessions, sanitises errors, and tells the user what happens to their data.

Nothing about the analyses differs between the two.

---

## 1. The application directory

Shiny Server runs the application directory itself. That means `app.R` must
**return** the application, not start it: `run_app()` calls `runApp()`, and
`runApp()` inside `runApp()` is an error Shiny refuses by name. Use
`eq5d_app()`, which hands back the app object with the online settings already
in force.

It is written `eq5dsuite:::eq5d_app()`, with three colons, throughout this
guide. That is deliberate: the app's supporting functions are internal to the
package rather than part of the interface a user analysing EQ-5D data sees, so
`eq5dsuite::eq5d_app()` with two colons will not find it. Copy the three
colons exactly.

```
/srv/shiny-server/eq5d/app.R
```

```r
eq5dsuite:::eq5d_app(online = TRUE)
```

That is the whole file. Limits and wording come from the options and
environment variables in section 3.

```bash
sudo mkdir -p /srv/shiny-server/eq5d
sudo chown -R shiny:shiny /srv/shiny-server/eq5d
```

eq5dsuite and its dependencies must be installed somewhere the `shiny` user can
read — the site library, not a personal one:

```bash
sudo R -e 'install.packages(c("shiny","bslib","DT","readxl","rmarkdown","zip","writexl"))'
sudo R -e 'install.packages("eq5dsuite")'
```

`pandoc` is needed for the Word report. It ships with RStudio Server; on a
server without it, `sudo apt install pandoc`.

Check the app loads as the `shiny` user before wiring anything up:

```bash
sudo -u shiny R -e 'print(class(eq5dsuite:::eq5d_app(online = TRUE)))'
# [1] "shiny.appobj"
```

---

## 2. `shiny-server.conf`

`/etc/shiny-server/shiny-server.conf`:

```
run_as shiny;

# One R process serves every session of an app, so keep it modest and let it
# be reaped promptly when the last person leaves.
simple_scheduler 20;
app_idle_timeout 30;
app_init_timeout 120;

# Defence in depth: the app sets this itself in online mode.
sanitize_errors true;

# Logs are written per process and removed when it exits cleanly. Leave that
# alone unless you are debugging - see section 6.
preserve_logs false;

server {
  listen 3838 127.0.0.1;        # behind the proxy only, never public

  location / {
    app_dir /srv/shiny-server/eq5d;
    log_dir /var/log/shiny-server;
  }
}
```

```bash
sudo systemctl restart shiny-server
```

Directive names have changed between Shiny Server versions; check yours
against the admin guide for the version you have installed.

### `simple_scheduler` is the thing to understand

The open-source edition has one scheduler. **A single R process serves every
session of the app**, up to `simple_scheduler` concurrent connections. Three
consequences, and they shape everything below:

- **Memory is shared.** Every session's uploaded data, working copy and saved
  results live in one process. At the default 50,000-row limit, allow roughly
  150 MB per active session. `simple_scheduler 20` with 50,000-row datasets can
  reach several gigabytes; lower the scheduler, the row limit, or both, and
  watch it under load.
- **Value sets are shared.** See section 4.
- **`tempdir()` is shared.** The app already keeps each session's files in a
  subdirectory of its own and deletes them when the session ends, which is why
  that matters — see section 5.

`app_idle_timeout 30` reaps the process 30 seconds after the last person
disconnects, which also clears whatever was in memory. Do **not** set it to
`0`: that means never.

---

## 3. Settings

Each is read from an R option first, then an environment variable, then the
default.

| what | option | environment variable | default |
|---|---|---|---|
| online mode | `eq5dsuite.online` | `EQ5DSUITE_ONLINE` | off |
| maximum upload, MB | `eq5dsuite.max_upload_mb` | `EQ5DSUITE_MAX_UPLOAD_MB` | `10` |
| maximum rows | `eq5dsuite.max_rows` | `EQ5DSUITE_MAX_ROWS` | `50000` |
| maximum columns | `eq5dsuite.max_cols` | `EQ5DSUITE_MAX_COLS` | `200` |
| idle timeout, minutes | `eq5dsuite.idle_minutes` | `EQ5DSUITE_IDLE_MINUTES` | `30` |
| accepted file types | `eq5dsuite.allowed_types` | `EQ5DSUITE_ALLOWED_TYPES` | `csv,xlsx,xls` |

`app.R` already passes `online = TRUE`, so `EQ5DSUITE_ONLINE` is not needed.
Set the limits alongside it, in the same file, where they are visible to
whoever reads it next:

```r
options(
  eq5dsuite.max_rows     = 25000,
  eq5dsuite.max_upload_mb = 8,
  eq5dsuite.idle_minutes = 20
)
eq5dsuite:::eq5d_app(online = TRUE)
```

Check what a deployment will apply without starting it:

```bash
sudo -u shiny R -e 'str(eq5dsuite:::eq5d_online_config(online = TRUE))'
```

The row and column limits are checked **after the file is read**, so they hold
whatever the browser allowed through. The type is checked on the server too,
not only by the file picker.

The defaults are a starting point. The row limit is not about speed — the PCHC
table takes 0.25 s at 50,000 rows and 0.69 s at 200,000 — but about memory in
the one shared process. Revisit the numbers after load testing.

### `.rds` is not accepted online

Reading a serialised R object from an untrusted source is not a safe
operation. `.rds` is accepted locally and not online, and that is deliberate.
Adding `rds` to `EQ5DSUITE_ALLOWED_TYPES` would undo it; don't.

### The wording users see

The three notices — what happens to uploaded data, the line beside the file
picker, and the pointer to running the app locally — are in one function and
replaced without touching the app. Put your data protection officer's final
text in `app.R`:

```r
options(
  eq5dsuite.online.notice.data   = "...",
  eq5dsuite.online.notice.upload = "...",
  eq5dsuite.online.notice.local  = "..."
)
```

`eq5dsuite:::eq5d_online_notice("data")` shows what is currently in force.

---

## 4. Value sets are shared by the whole process

This is the one thing about the app that does not behave per session, and the
open-source Shiny Server is where it matters most.

Value sets live in an environment held in `options("eq.env")`, which belongs to
the **R process**. With `simple_scheduler`, one process serves every session of
the app. A value set added by one session would be visible to every other
session in that process until it was reaped, and a value set written to the
cache would persist for every session afterwards.

So the online app must never add, change or remove a value set. It doesn't: its
only value-set call is `eqvs_display()`, to list what is available, and a test
asserts that `eqvs_add()`, `eqvs_drop()` and `update_value_sets()` appear
nowhere in it. `update_value_sets()` refuses outright while the app is online.

**Update value sets from outside the app**, as the user that runs it, then
restart:

```bash
sudo -u shiny R -e 'eq5dsuite::update_value_sets()'
sudo systemctl restart shiny-server
```

The cache is at `tools::R_user_dir("eq5dsuite", "cache")` — for the `shiny`
user, `~shiny/.cache/R/eq5dsuite`. The app needs to read it, not write it.

---

## 5. Temporary files

Uploaded data do reach the disk, briefly. Shiny writes each uploaded file to
an upload directory under `tempdir()` and removes it when the session ends.
Everything the app itself writes -- the Word report, the archive -- goes into
a `tempfile()` directory of the session's own, deleted in
`session$onSessionEnded()`. Nothing is written to the application directory,
and no path is ever built from an uploaded file's name. Do not describe the
service as keeping data only in memory.

That covers an orderly exit. A process killed outright leaves its directory
behind, so clean up periodically. `/etc/cron.daily/eq5d-tmp`:

```bash
#!/bin/sh
find /tmp -maxdepth 2 -name 'eq5d*' -mmin +720 -exec rm -rf {} + 2>/dev/null
```

```bash
sudo chmod +x /etc/cron.daily/eq5d-tmp
```

To keep the service's temporary files apart from every other service, and
discarded when it stops, give it a private `/tmp`:

```bash
sudo systemctl edit shiny-server
```

```
[Service]
PrivateTmp=true
```

A private `/tmp` is still on disk unless `/tmp` itself is memory-backed. If
uploaded data must not reach the disk at all, also mount a memory-backed
filesystem there -- for example `TemporaryFileSystem=/tmp:size=2G` in the
same section (systemd 238 or later), sized for the largest uploads and
reports you expect -- and remember that memory can still be swapped out. With
`PrivateTmp`, the cron job above becomes unnecessary: systemd discards the
private `/tmp` when the service stops.

---

## 6. Logs

Shiny Server writes one log per R process to `/var/log/shiny-server/`, removed
when the process exits cleanly (`preserve_logs false`).

The app keeps uploaded data out of them: `shiny.sanitize.errors` is set in
online mode, so an uncaught error reaches the browser as a generic message; and
the analysis warnings — the follow-up one names values from the uploaded file —
are shown to the person who uploaded it and muffled rather than written to
stderr.

Even so, **treat these logs as potentially sensitive**. Do not ship them to a
shared aggregator without checking what reaches them, and leave
`preserve_logs false` unless you are actively debugging.

---

## 7. The web address

Shiny Server speaks plain HTTP and has **no authentication and no TLS** in the
open-source edition. Anyone with the address can use the app. That is
acceptable here only because the app stores nothing — but it means the proxy in
front of it does the TLS, and you should decide deliberately whether the
address should be discoverable.

nginx, `/etc/nginx/sites-available/eq5d`:

```nginx
server {
  listen 443 ssl http2;
  server_name eq5d.example.org;

  ssl_certificate     /etc/letsencrypt/live/eq5d.example.org/fullchain.pem;
  ssl_certificate_key /etc/letsencrypt/live/eq5d.example.org/privkey.pem;

  # At least the app's own limit, or the upload fails before the app sees it.
  client_max_body_size 12m;

  location / {
    proxy_pass http://127.0.0.1:3838;
    proxy_http_version 1.1;

    # Shiny needs the websocket, and it must outlive an idle session or the
    # app's own one-minute warning never appears.
    proxy_set_header Upgrade $http_upgrade;
    proxy_set_header Connection "upgrade";
    proxy_read_timeout 3600s;
    proxy_send_timeout 3600s;

    proxy_set_header Host $host;
    proxy_set_header X-Real-IP $remote_addr;
    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_set_header X-Forwarded-Proto $scheme;
  }
}
```

```bash
sudo ln -s /etc/nginx/sites-available/eq5d /etc/nginx/sites-enabled/
sudo nginx -t && sudo systemctl reload nginx
```

`client_max_body_size` must be at least `EQ5DSUITE_MAX_UPLOAD_MB`; a little
above it, so the app's own message is what the user sees rather than a 413 from
nginx.

Check the firewall allows 443 and **not** 3838 — `listen 3838 127.0.0.1` in
section 2 already binds Shiny Server to the loopback, which is the belt to
that braces.

---

## 8. Before going live

- [ ] `sudo -u shiny R -e 'class(eq5dsuite:::eq5d_app(online = TRUE))'` says `shiny.appobj`
- [ ] `eq5dsuite:::eq5d_online_config(online = TRUE)` reports the limits you meant
- [ ] `simple_scheduler` and the row limit suit the memory the box has
- [ ] nginx's `client_max_body_size` is above the app's upload limit
- [ ] 3838 is not reachable from outside; 443 is, with a valid certificate
- [ ] the final data-protection wording is in `app.R`
- [ ] value sets are up to date, updated from outside the app
- [ ] temporary files are cleaned, or `PrivateTmp=true` is set
- [ ] the logs are not going anywhere they should not
- [ ] a file over the row limit is refused with a message that makes sense
- [ ] an idle session warns after 29 minutes, then ends
- [ ] a `.rds` upload is refused

---

## Other platforms

The `app.R` above — `eq5dsuite:::eq5d_app(online = TRUE)` — is what every
platform wants, since each runs the application directory itself. What differs:

**Posit Connect** runs each app in its own process with its own `TMPDIR`, and
handles authentication, TLS and timeouts itself. Sections 2, 5 and 7 do not
apply; set the limits as environment variables on the content item, and the
upload limit with `Applications.MaxUploadSizeMB`. Because processes are not
shared between users, the value-set caution in section 4 is weaker — though it
still holds within one process.

**ShinyProxy** gives each session its own container, so temporary files and
value sets are naturally isolated and section 5's clean-up is unnecessary. Make
its `proxy.heartbeat-timeout` longer than the app's idle timeout.

**shinyapps.io** gives you no control over `TMPDIR`, the proxy or the
timeouts, and recycles instances on its own schedule. The app's own limits still
apply. Check the account's upload limit against `EQ5DSUITE_MAX_UPLOAD_MB`, and
note that the idle timeout set in the dashboard may fire before the app's
warning.
