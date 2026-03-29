#!/usr/bin/env python3
"""Generate docs/index.html from images/ and code/ directories."""

import os
import re
from pathlib import Path

REPO = "JasonBernstein1/myTidyTuesday"
BRANCH = "main"
RAW_BASE = f"https://raw.githubusercontent.com/{REPO}/{BRANCH}"
GITHUB_BASE = f"https://github.com/{REPO}/blob/{BRANCH}"

ROOT = Path(__file__).parent.parent
IMAGES_DIR = ROOT / "images"
CODE_DIR = ROOT / "code"
DOCS_DIR = ROOT / "docs"

CODE_EXTENSIONS = [".R", ".py", ".qmd", ".Rmd"]


def parse_key(filename):
    """Extract (year, week) from filenames like 2023_week12.png"""
    m = re.match(r"(\d{4})_week(\d+)", filename)
    if m:
        return (int(m.group(1)), int(m.group(2)))
    return None


def find_code_file(stem):
    for ext in CODE_EXTENSIONS:
        path = CODE_DIR / (stem + ext)
        if path.exists():
            return path.name
    return None


def make_title(year, week):
    return f"{year} &mdash; Week {week}"


def generate():
    posts = []
    for img_path in sorted(IMAGES_DIR.glob("*.png")):
        stem = img_path.stem
        key = parse_key(stem)
        if key is None:
            continue
        year, week = key
        code_file = find_code_file(stem)
        posts.append(
            {
                "stem": stem,
                "year": year,
                "week": week,
                "title": make_title(year, week),
                "image_url": f"{RAW_BASE}/images/{img_path.name}",
                "code_url": f"{GITHUB_BASE}/code/{code_file}" if code_file else None,
                "code_file": code_file,
            }
        )

    # Newest first
    posts.sort(key=lambda p: (p["year"], p["week"]), reverse=True)

    cards_html = ""
    for p in posts:
        code_link = (
            f'<a href="{p["code_url"]}" target="_blank" rel="noopener">View code</a>'
            if p["code_url"]
            else "<span>No code</span>"
        )
        cards_html += f"""
    <article class="card">
      <h2>{p["title"]}</h2>
      <a href="{p["image_url"]}" target="_blank" rel="noopener">
        <img src="{p["image_url"]}" alt="TidyTuesday {p["title"]}" loading="lazy">
      </a>
      <footer>{code_link}</footer>
    </article>"""

    html = f"""<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>My TidyTuesday Visualizations</title>
  <style>
    *, *::before, *::after {{ box-sizing: border-box; margin: 0; padding: 0; }}
    body {{
      font-family: system-ui, sans-serif;
      background: #f8f8f8;
      color: #222;
      padding: 2rem 1rem;
    }}
    header {{
      max-width: 1100px;
      margin: 0 auto 2rem;
    }}
    header h1 {{ font-size: 1.8rem; margin-bottom: 0.3rem; }}
    header p {{ color: #555; font-size: 0.95rem; }}
    header p a {{ color: #0366d6; text-decoration: none; }}
    header p a:hover {{ text-decoration: underline; }}
    .grid {{
      display: grid;
      grid-template-columns: repeat(auto-fill, minmax(320px, 1fr));
      gap: 1.5rem;
      max-width: 1100px;
      margin: 0 auto;
    }}
    .card {{
      background: #fff;
      border: 1px solid #ddd;
      border-radius: 6px;
      overflow: hidden;
      display: flex;
      flex-direction: column;
    }}
    .card h2 {{
      font-size: 1rem;
      font-weight: 600;
      padding: 0.75rem 1rem 0.5rem;
      color: #333;
    }}
    .card a img {{
      display: block;
      width: 100%;
      height: auto;
      border-bottom: 1px solid #eee;
    }}
    .card footer {{
      padding: 0.6rem 1rem;
      font-size: 0.875rem;
    }}
    .card footer a {{
      color: #0366d6;
      text-decoration: none;
    }}
    .card footer a:hover {{ text-decoration: underline; }}
  </style>
</head>
<body>
  <header>
    <h1>My TidyTuesday</h1>
    <p>Weekly data visualizations &mdash; <a href="https://github.com/{REPO}" target="_blank" rel="noopener">source on GitHub</a></p>
  </header>
  <main class="grid">{cards_html}
  </main>
</body>
</html>
"""

    DOCS_DIR.mkdir(exist_ok=True)
    out = DOCS_DIR / "index.html"
    out.write_text(html, encoding="utf-8")
    print(f"Generated {out} with {len(posts)} posts.")


if __name__ == "__main__":
    generate()
