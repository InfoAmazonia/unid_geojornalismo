#!/usr/bin/env python3

import html
import json
import shutil
from pathlib import Path
from urllib.parse import quote

ROOT = Path(__file__).resolve().parents[1]
SOURCE = ROOT / "pages"
OUTPUT = ROOT / "_site"


def public_path(directory: Path) -> str:
    relative = directory.relative_to(SOURCE)
    return "/".join(quote(part) for part in relative.parts) + "/"


def load_projects():
    projects = []

    for metadata_path in sorted(SOURCE.rglob("project.json")):
        metadata = json.loads(metadata_path.read_text(encoding="utf-8"))

        if not metadata.get("listed", True):
            continue

        title = metadata.get("title")
        if not title:
            raise SystemExit(f"Missing title in {metadata_path}")

        index_path = metadata_path.parent / "index.html"
        if not index_path.is_file():
            raise SystemExit(
                f"Listed project has no index.html: {metadata_path.parent}"
            )

        projects.append(
            {
                "title": title,
                "description": metadata.get("description", ""),
                "path": public_path(metadata_path.parent),
            }
        )

    projects.sort(key=lambda item: item["title"].casefold())
    return projects


def render_index(projects):
    if projects:
        entries = []

        for project in projects:
            description = (
                f"<p>{html.escape(project['description'])}</p>"
                if project["description"]
                else ""
            )

            entries.append(
                f"""
                <article>
                  <h2>
                    <a href="{html.escape(project['path'], quote=True)}">
                      {html.escape(project['title'])}
                    </a>
                  </h2>
                  {description}
                  <a href="{html.escape(project['path'], quote=True)}">
                    Acessar projeto →
                  </a>
                </article>
                """
            )

        projects_html = "\n".join(entries)

    else:
        projects_html = "<p>Nenhum projeto público está listado no momento.</p>"

    return f"""<!doctype html>
<html lang="pt-BR">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>Projetos da Unidade de Geojornalismo — InfoAmazonia</title>
</head>
<body>
  <main>
    <h1>Projetos da Unidade de Geojornalismo</h1>

    <p>
      Aplicações, visualizações e outros projetos públicos desenvolvidos
      pela InfoAmazonia.
    </p>

    {projects_html}
  </main>
</body>
</html>
"""


def main():
    projects = load_projects()

    if OUTPUT.exists():
        shutil.rmtree(OUTPUT)

    shutil.copytree(SOURCE, OUTPUT)

    (OUTPUT / "index.html").write_text(
        render_index(projects),
        encoding="utf-8"
    )

    (OUTPUT / ".nojekyll").touch()


if __name__ == "__main__":
    main()
