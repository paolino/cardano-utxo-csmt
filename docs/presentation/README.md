# Presentation Files

This directory contains the "Light Clients" presentation built with Marp.

## File Structure

| File | Purpose |
|------|---------|
| `light-clients.md` | Slide source (Marp markdown) |
| `index.html` | Generated presentation (do not edit) |
| `slides.md` | mkdocs wrapper with embedded iframe |
| `diagrams/*.mmd` | Mermaid diagram sources |

## Editing Slides

Edit `light-clients.md`, then rebuild:

```bash
just slides
```

Or manually:

```bash
marp docs/presentation/light-clients.md -o docs/presentation/index.html
```

## Editing Diagrams

Mermaid diagrams are stored in `diagrams/*.mmd` for easy editing.

1. Edit the `.mmd` file (e.g., `diagrams/architecture.mmd`)
2. Run `just slides` - this will:
   - Convert `.mmd` files to mermaid.ink URLs
   - Update `light-clients.md` with new URLs
   - Build the HTML presentation

The diagram name must match the HTML comment in the slides:
```html
<!-- architecture --><img src="...">
```

## Marp Features

- Slides separated by `---`
- Frontmatter for theme configuration
- [Marp documentation](https://marp.app/)

## Viewing

```bash
mkdocs serve
```

Then open: `http://127.0.0.1:8000/cardano-utxo-csmt/presentation/`

**Controls:** Arrow keys (navigate) | F (fullscreen)

## Why `.md` instead of `.txt`?

Marp requires `.md` extension. mkdocs will also render it as a page, but we use `index.html` (Marp output) as the primary presentation.
