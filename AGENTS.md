# AGENTS.md — yassinesR

Notes for AI agents and contributors working on this repo.

## Scope

`yassinesR` is the user's personal R toolkit. It hosts **two coexisting
theme systems** plus shared helpers:

* **Yassine theme** — the original `theme_yassine()` / `theme_yassine_dark()`
  plus the Yassine palette (`yassine_colors()`, `scale_*_yassine()`).
  Lives in `R/theme.R`, `R/colors.R`, `R/helpers.R`, `R/examples.R`.

* **Majorelle theme** — Apple HIG typographic scaffolding + Jardin Majorelle
  palette. Lives in `R/majorelle_palette.R`, `R/majorelle_theme.R`,
  `R/majorelle_scales.R`, `R/majorelle_raincloud.R`. Two variants:
  `rabat` (white surface) and `palmeraie` (sun-warmed terracotta `#D97461`).

The two systems do not share state; they're independent and can be used
side-by-side in the same session.

## Variant state

`use_majorelle_defaults(variant = ...)` writes `options("majorelle.variant")`.
`majorelle_active_variant()` reads it. All Majorelle helpers (`annotate_*`,
`highlight_series`, `scale_*_majorelle`, `theme_majorelle`) read this
option so callers don't thread `variant=` through every call.

To force a specific variant for a single plot regardless of session state,
pass `variant=` explicitly to `theme_majorelle()` and the discrete scales.

## Conventions worth remembering

* Plot title weight is 400 — display sizes carry their visual weight
  through size, not bold. Facet panel titles stay bold.
* Plot titles align to the plot region, not the axes
  (`plot.title.position = "plot"`).
* No gridlines by default. Spines are gray-80 on `rabat` and a light
  terracotta tint on `palmeraie`.
* Filled-circle scatter (shape 21) with a near-black contour on `rabat`,
  white on `palmeraie` (warm surface needs a light edge for definition).
* Tabular numerals on tick labels.
* Tiered log ticks: long at 1×10ᵏ, mid at 5×10ᵏ, short at intermediate
  mantissas plus 1.5/2.5/7.5 to fill the 1-2 and 10-20 visual gaps.

## Cross-language sibling

A sister Python package mirrors the Majorelle subset (themes, palettes,
helpers, raincloud) so plots render consistently in both languages.
Keep the two in sync when changing tokens or helper APIs.

## Development workflow

1. After editing R sources, run `devtools::document()` to regenerate
   `man/*.Rd` from the roxygen comments and refresh `NAMESPACE`.
2. `devtools::check()` for full R CMD check.
3. Tests live in `tests/testthat/`; add coverage for new helpers.

## Adding a new Majorelle variant

1. Append a new entry to `majorelle_variants` in `R/majorelle_theme.R`
   (mirror the existing `rabat` / `palmeraie` keys).
2. If the variant needs a new sequential cmap, point `cmap` at one of
   `"blues"` / `"golds"` / `"terracottas"` / `"greens"` (or add a new
   sequential palette in `R/majorelle_palette.R` and extend
   `.seq_palette()` in `R/majorelle_scales.R`).
3. Update the Python sibling's `THEME_VARIANTS` to match.
4. Re-render the demo set in both languages and commit the new outputs
   for visual review.

## Files NOT to touch without intent

* `LICENSE`, `LICENSE.md` — Apache 2.0 license text.
* `tests/testthat/setup-*.R` if present — test fixtures.
* Generated `man/*.Rd` — regenerate via `devtools::document()` rather
  than editing by hand.
