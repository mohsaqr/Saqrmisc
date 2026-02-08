# Documentation Pages

Self-contained HTML documentation for R packages, all sharing the same CSS styling system.

## Pages

| File | Package | Version | Description |
|------|---------|---------|-------------|
| `index.html` | Saqrmisc | — | Main package docs |
| `tna.html` | tna | v1.1.0 | Transition Network Analysis |
| `cograph.html` | cograph | v1.5.2 | Network visualization |
| `tna.Rmd` | tna | v1.1.0 | Knittable R Markdown source (outputs `tna_knit.html`) |
| `debug_tna.Rmd` | tna | v1.1.0 | Debug/test report (outputs `debug_tna.html`) |

## Styling System

All pages use the same self-contained CSS (no external dependencies). The full CSS is inlined in each HTML file's `<style>` block.

### CSS Variables

```css
:root {
  --bg: #ffffff;           /* Page background */
  --fg: #1a1a2e;           /* Text color */
  --muted: #6b7280;        /* Secondary text */
  --accent: #2563eb;       /* Links, highlights */
  --accent-light: #eff6ff; /* Code badge backgrounds */
  --border: #e5e7eb;       /* Borders, separators */
  --code-bg: #f8fafc;      /* Code block background */
  --code-border: #e2e8f0;  /* Code block border */
  --table-stripe: #f9fafb; /* Even row stripe */
  --header-bg: #0f172a;    /* Sidebar background */
  --success: #059669;      /* Output label color */
  --warn: #d97706;         /* Warning/note color */
  --sidebar-w: 280px;      /* Sidebar width */
  --output-bg: #f0fdf4;    /* Output block background */
  --output-border: #86efac; /* Output block border */
}
```

### Layout Components

**Sidebar** — Fixed left panel (`280px`), dark background, two-level navigation. Section headers are uppercase, function links are monospace with hover highlights.

```html
<aside class="sidebar">
  <div class="logo">
    <h1>package-name</h1>
    <p>v1.0.0 &middot; R Package</p>
  </div>
  <nav>
    <ul>
      <li><a href="#section">Section Name</a>
        <ul>
          <li><a href="#function">function()</a></li>
        </ul>
      </li>
    </ul>
  </nav>
</aside>
```

**Main content** — Right of sidebar, `max-width: 900px`, generous padding.

```html
<div class="main">
  <!-- All content here -->
</div>
```

### Content Blocks

**Hero** — Gradient banner at top with title, description, and badge pills.

```html
<div class="hero" id="getting-started">
  <h1>package-name</h1>
  <p>Description text.</p>
  <div class="badges">
    <span class="badge badge-blue">v1.0.0</span>
    <span class="badge badge-green">CRAN</span>
    <span class="badge badge-purple">Feature</span>
    <span class="badge badge-amber">R &ge; 4.1.0</span>
  </div>
</div>
```

Badge colors: `badge-blue`, `badge-green`, `badge-purple`, `badge-amber`, `badge-teal`, `badge-rose`.

**Install box** — Gradient background box for installation instructions.

```html
<div class="install-box">
  <h4>Install</h4>
  <pre><code>install.packages("pkg")</code></pre>
</div>
```

**Quick-ref cards** — 2-column grid of feature summary cards.

```html
<div class="quick-ref">
  <div class="quick-ref-card">
    <h4>Category</h4>
    <div class="funcs">func1() &middot; func2() &middot; func3()</div>
  </div>
</div>
```

**Function signature** — Blue left-border code block.

```html
<div class="signature"><code>function_name(param1, param2 = "default", ...)</code></div>
```

**Parameter table** — Standard table, key params only.

```html
<table>
  <thead><tr><th>Parameter</th><th>Description</th><th>Default</th></tr></thead>
  <tbody>
    <tr><td><code>param</code></td><td>Description</td><td><code>value</code></td></tr>
  </tbody>
</table>
```

**Returns block** — Blue left-border info box.

```html
<div class="returns"><strong>Returns:</strong> Description of return value.</div>
```

**Note/warning** — Yellow left-border box.

```html
<div class="note">Important note text.</div>
```

**Output block** — Green background with "Output" label.

```html
<div class="output"><code>Output text here</code></div>
```

**Collapsible examples** — Expandable sections for code examples.

```html
<details>
  <summary>Example title</summary>
  <div class="detail-content">
    <pre><code>example_code()</code></pre>
  </div>
</details>
```

**Section headers** — `<h2>` for major sections (with top border), `<h3>` for functions (code-styled).

```html
<h2 id="section-id">Section Name</h2>
<h3 id="function_name"><code>function_name()</code></h3>
```

### Per-Function Documentation Pattern

Each function follows this structure:

```
1. <h3> with function name in <code>
2. <p> one-line description
3. .signature block with abbreviated signature
4. (Optional) <details> "Full signature" for functions with many params
5. Parameter table (key params only)
6. .returns block
7. (Optional) .note block for caveats
8. <details> collapsible examples
```

For functions with 20+ params (like `splot`, `sn_nodes`, `sn_edges`), show an abbreviated signature in the `.signature` block and put the complete signature in a collapsible `<details>` section.

### Responsive

At `< 900px` viewport width, the sidebar hides and main content goes full-width.

## How to Create a New Page

1. Copy the `<style>` block from any existing page (they're identical)
2. Create sidebar navigation matching your section/function structure
3. Add content using the blocks documented above
4. Keep it self-contained — no external CSS/JS dependencies

Template:

```html
<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>package - R Package Documentation</title>
<style>
  /* Copy full CSS from any existing page */
</style>
</head>
<body>
<aside class="sidebar">
  <div class="logo">
    <h1>package</h1>
    <p>v1.0.0 &middot; R Package</p>
  </div>
  <nav><ul>
    <li><a href="#getting-started">Getting Started</a></li>
    <!-- sections -->
  </ul></nav>
</aside>
<div class="main">
  <div class="hero" id="getting-started">
    <h1>package</h1>
    <p>Package description.</p>
    <div class="badges">
      <span class="badge badge-blue">v1.0.0</span>
    </div>
  </div>
  <!-- content -->
</div>
</body>
</html>
```

## How to Update

### Updating function signatures

Verify against the actual R package:

```r
# Check a single function
args(package::function_name)

# List all exports
sort(getNamespaceExports("package"))
```

### Updating citations

Verify against Crossref API:

```bash
curl -s "https://api.crossref.org/works/DOI" | jq '.message | {title, author, published}'
```

### Updating version numbers

Search-and-replace the version string in both the sidebar logo and the hero badge:

```
sidebar: <p>v1.1.0 &middot; R Package</p>
hero:    <span class="badge badge-blue">v1.1.0</span>
footer:  <strong>pkg</strong> v1.1.0
```

### Verification checklist

Before publishing, verify:

1. **Function signatures** — `Rscript -e "args(pkg::fn)"` for every documented function
2. **URLs** — `curl -sI URL` for every link (HEAD request, check for 200/301/302)
3. **Citations** — Crossref API for DOIs (authors, year, title, pages)
4. **Sidebar links** — Every `href="#id"` has a matching `id=""` in the content
5. **Responsive** — Narrow the browser window to confirm sidebar hides

### Publishing with GitHub Pages

1. Go to repo Settings > Pages
2. Set source to "Deploy from a branch"
3. Set branch to `main`, folder to `/docs`
4. Pages will be at `https://username.github.io/repo-name/`
   - `index.html` → `https://username.github.io/Saqrmisc/`
   - `tna.html` → `https://username.github.io/Saqrmisc/tna.html`
   - `cograph.html` → `https://username.github.io/Saqrmisc/cograph.html`
