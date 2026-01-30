# tersmu

**tersmu** is a semantic parser for [Lojban](https://www.lojban.org). It translates Lojban text into predicate logic.

> **Disclaimer:** This is an unofficial project. While it aims to follow baseline prescription where feasible, the semantics it assigns to Lojban may in some cases be non-standard or incorrect.

---

## Production Build (Docker)

To build a slim, production-ready image and run the HTTP REST API:

```bash
# Build the production image
docker build -t tersmu .

# Run the API
docker run --rm -p 8080:8080 tersmu
```

The API will be available at `http://localhost:8080`. See [HTTP REST API](#http-rest-api) for usage.

### Run the CLI parser

You can also run the command-line parser using the same production image:

```bash
# Parse a file
docker run --rm -it --entrypoint tersmu tersmu examples/1.jbo

# Parse from stdin (paste Lojban, then Ctrl-D)
docker run --rm -it --entrypoint tersmu tersmu
```

---

## Development (Hot Reloading)

For local development, you can use `docker-compose` with a container that watches your source files and automatically reloads the server using `ghcid`.

### Prerequisites

The parser files must be generated before the application can run. Since the necessary tools (Python, Make) are installed within the Docker image, you should run the generation commands using `docker compose`:

```bash
# Start the container
docker compose -f docker-compose.dev.yml up -d

# Generate pappy files and Haskell modules inside the container
docker compose -f docker-compose.dev.yml exec tersmu python3 scripts/gen_pappy.py Lojban.pest Lojban.pappy.rhs -o Lojban.pappy
docker compose -f docker-compose.dev.yml exec tersmu python3 scripts/gen_pappy.py Morphology.pest Morphology.pappy.rhs -o Morphology.pappy
docker compose -f docker-compose.dev.yml exec tersmu make pappy/pappy/pappy
docker compose -f docker-compose.dev.yml exec tersmu make Pappy/Parse.hs Lojban.hs Morphology.hs
```

### Start the development environment

If you haven't already:

```bash
docker compose -f docker-compose.dev.yml up --build
```

- **Hot Reloading:** Your local directory is mounted into the container. Any changes to `.hs` files will trigger an automatic incremental rebuild and restart of `tersmu-server` via `ghcid`.
- **Persistent Cache:** Cabal dependencies and build artifacts are stored in Docker volumes (`cabal_store`, `cabal_dist`) to keep rebuilds fast.
- **Port:** The dev server listens on port `8080`.

### Workflow

1. Edit your `.hs` files locally
2. Save the file
3. `ghcid` inside the container automatically detects changes, recompiles, and restarts the server
4. Test your changes at `http://localhost:8080`

### Rebuilding generated files

If you modify the `.pest` or `.pappy.rhs` grammar files, you need to regenerate the parser files. You can do this inside the running container:

```bash
docker compose -f docker-compose.dev.yml exec tersmu python3 scripts/gen_pappy.py Lojban.pest Lojban.pappy.rhs -o Lojban.pappy
docker compose -f docker-compose.dev.yml exec tersmu make Lojban.hs
```

`ghcid` will notice the changed `.hs` files and reload automatically.

---

## HTTP REST API

The server exposes a small REST API on port **8080**.

| Method | Path     | Description                    |
|--------|----------|--------------------------------|
| `GET`  | `/`      | API description                |
| `GET`  | `/health`| Health check                   |
| `POST` | `/`      | Parse Lojban text (request body) |
| `POST` | `/parse` | Parse Lojban text (request body) |

### Parse Lojban

Send the Lojban text as the request body (plain text, UTF-8). One line per sentence; blank lines are allowed.

**Response:** The API returns **JSON** (`Content-Type: application/json`). All string values are trimmed of leading/trailing spaces and newlines.

**Single line:** One JSON object with keys:
- `input` — the trimmed input line
- `logical` — logical form (bracket notation), or `null` on error
- `canonical` — canonicalized Lojban form, or `null` on error
- `error` — `null` on success, or the error message (morphology/parse) on failure

**Multiple lines:** One JSON object with key `results` — an array of objects, each with the same keys as above.

**Example (curl):**

```bash
# Single line
curl -s -X POST -H "Content-Type: text/plain; charset=utf-8" \
  -d "mi klama le zarci" \
  http://localhost:8080/parse

# From file
curl -s -X POST -H "Content-Type: text/plain; charset=utf-8" \
  --data-binary @examples/1.jbo \
  http://localhost:8080/parse
```

**Example response (success):**

```json
{
  "input": "mi klama le zarci",
  "logical": "non-veridical: zarci(c0)\nklama(mi,c0)",
  "canonical": "ju'a nai cy no zarci\n.i mi klama cy no",
  "error": null
}
```

**Example response (parse error):**

```json
{
  "input": "mi klama",
  "logical": null,
  "canonical": null,
  "error": "Parse error:\n\t{...}\n\t ^"
}
```

### Health check

```bash
curl -s http://localhost:8080/health
# ok
```

### Testing the API with the example suite

With the container running (`docker run -d -p 8080:8080 --name tersmu-api tersmu`):

```bash
./test_api_examples.sh
```

Optional: use a different API base URL:

```bash
TERSMU_API_URL=http://your-host:8080 ./test_api_examples.sh
```

---

## Command-line usage (tersmu)

When run locally (see [Installation](#installation)) or via `docker run ... --entrypoint tersmu tersmu`:

```bash
tersmu [OPTIONS] [FILE]
```

- **No FILE:** read from stdin (interactive; one paragraph per input).
- **FILE:** read from file. Use `-L` / `--lines` to treat each line as a separate Lojban text.

**Options:**

| Option        | Long         | Description |
|---------------|--------------|-------------|
| `-l`          | `--loj`      | Output logical form only |
| `-j`          | `--jbo`      | Output forethoughtful Lojban form only |
| `-L`          | `--lines`    | One line = one Lojban text |
| `-p`          | `--paragraphs` | One blank-line-separated block = one text |
| `-u`          | `--utf8`     | Output UTF-8 (default: ASCII) |
| `-h`          | `--help`     | Show help |
| `-v`          | `--version`  | Show version |

**Examples:**

```bash
# Parse a file (one line per sentence)
tersmu -L examples/1.jbo

# Parse stdin
echo "mi klama le zarci" | tersmu -L
```

## Documentation

| Path | Description |
|------|-------------|
| [docs/overview.md](docs/overview.md) | Architecture and code overview |
| [docs/BUGS.md](docs/BUGS.md) | Known limitations |
| [docs/TODO.md](docs/TODO.md) | Planned work |
| [docs/CHANGELOG.md](docs/CHANGELOG.md) | Version history |
| [docs/design-notes.md](docs/design-notes.md) | Design notes |
| [docs/notes/](docs/notes/) | Topic notes (bridi-operators, drt, illocution, questions, etc.) |

---

## WebAssembly Build

tersmu can be compiled to WebAssembly for use in web browsers.

### Building the WASM version

The easiest way to build the WASM module is using the provided script, which uses a specialized Docker environment (`Dockerfile.wasm`) to handle the cross-compilation toolchain:

```bash
./build_wasm.sh
```

This script:
1. Builds the `tersmu-wasm-builder` Docker image.
2. Extracts the compiled `tersmu.wasm` from the container.
3. Places it in the `wasm-web-app/` directory alongside the HTML/JS frontend.

### Running the WASM web app

Once built, you can serve the web app locally:

```bash
cd wasm-web-app
python3 -m http.server 8000
```

Then open http://localhost:8000 in your browser.

See [wasm-web-app/README.md](wasm-web-app/README.md) for more details.

---

## Hacking

- **Parser sources:** The canonical grammar is in **Pest** format (`.pest` + `.pappy.rhs`) for portability to Rust. The Makefile generates `Lojban.pappy` and `Morphology.pappy` from `Lojban.pest`/`Lojban.pappy.rhs` and `Morphology.pest`/`Morphology.pappy.rhs` via `scripts/gen_pappy.py`, then a patched [Pappy](http://mbays.freeshell.org/pappy) generates `Lojban.hs` and `Morphology.hs`. To refresh `.pest` and `.pappy.rhs` from edited `.pappy` files, run `python3 scripts/pappy_to_pest.py Lojban.pappy` (and similarly for Morphology). See the [Makefile](Makefile) for targets and dependencies.
- **Architecture:** See [docs/overview.md](docs/overview.md).

---

## License and thanks

- **License:** GPL-3. See [COPYING](COPYING).
- **Thanks:** John Clifford, selpa'i, Alex Burka, tsani, and especially Jorge Llambías (see [docs/THANKS.md](docs/THANKS.md)).
