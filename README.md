# Maribondo

Personal website and blog generated from Org mode files with Emacs, `org-publish`,
and the local `ox-tufte` exporter. The generated HTML is published to GitHub
Pages.

## Project layout

- `content/`: source files. Edit these first.
- `content/posts/`: blog post source files.
- `public/`: generated HTML, CSS, fonts, and other files deployed to GitHub
  Pages.
- `build-site.el`: build entry point that runs dependency setup, index updates,
  publishing, and asset copying.
- `lisp/site-*.el`: helpers for paths and dependencies, post metadata, HTML,
  and publishing.
- `build.sh`: shell wrapper that runs the build with `emacs -Q`.
- `lisp/ox-tufte.el`: local Org exporter used to produce Tufte-style HTML.
- `tufte.css`, `ox-tufte.css`, and `site.css`: styles copied into the generated
  site. Custom navigation and archive styles live in `site.css`.
- `et-book/`: bundled web fonts copied into `public/et-book/`.
- `.github/workflows/publish.yml`: GitHub Actions workflow that builds and
  deploys `public/` to the `gh-pages` branch on every push to `main`.

## Requirements

Install Emacs. The build script uses Emacs in batch mode:

```sh
./build.sh
```

On the first run, `build-site.el` installs the `htmlize` package into the local
`.packages/` directory. That directory is ignored by git.

## Editing the site

Edit Org source files under `content/`.

Main pages:

- Home page: `content/index.org`
- About page: `content/about.org`
- Posts archive page: `content/posts/index.org`

Posts:

- Add new posts as `.org` files under `content/posts/`.
- Use a filename that should become the URL slug, for example:
  `content/posts/my-new-post.org` becomes `public/posts/my-new-post.html`.
- Include at least `#+TITLE:` and `#+DATE:`. Posts without both are ignored by
  the generated latest-posts and archive lists.
- `#+DESCRIPTION:` is optional, but recommended because it is used in the posts
  archive. If it is missing, the build script tries to use the first paragraph.

Drafts:

- Store unpublished notes in `content/posts/drafts/`. They can be committed and
  remain available to Org-roam, but the site build excludes that directory:
  drafts create no HTML and never appear on the home page or posts archive.
- To publish a draft, move its `.org` file from `content/posts/drafts/` to
  `content/posts/`, ensure it has `#+TITLE:` and `#+DATE:`, then rebuild,
  review, commit, and push.
- To take a published post offline, move its `.org` file into
  `content/posts/drafts/` and delete its existing
  `public/posts/<slug>.html`. Rebuild, review, commit, and push. Deleting the
  old HTML page prevents it from remaining available at its former URL.

## Working in Emacs

Your Emacs configuration treats `content/posts/` as an Org-roam vault. These
commands are available globally:

- `C-c N b`: switch Org-roam to the blog vault.
- `C-c N m`: switch Org-roam back to the main notes vault.
- `C-c n f`: find a blog note by title.
- `C-c n i`: insert a link to another Org-roam note.
- `C-c n b`: open or close the Org-roam backlinks buffer.

Use `C-x C-f` to open or create post files and `C-x C-s` to save them. To move
or delete a post and its generated HTML page without leaving Emacs, open the
containing directory with `M-x dired`; use `R` to rename or move a file and `d`
followed by `x` to delete one. Org-roam automatically tracks the draft
subdirectory, so drafts remain searchable and linkable.

To build, run `M-x compile` and enter `./build.sh` when Emacs asks for the
compile command. Use `C-x v d` to review the changed files before committing.

Example post header:

```org
#+TITLE: My New Post
#+DATE: <2026-09-10 Thu>
#+AUTHOR: André Meneses
#+FILETAGS: :tag:
#+DESCRIPTION: A short summary for the archive page.
```

## Rebuilding locally

Run:

```sh
./build.sh
```

The build script does three important things:

1. Builds an Org ID lookup database in `.org-id-locations`.
2. Rewrites the generated HTML blocks inside `content/index.org` and
   `content/posts/index.org`.
3. Publishes the site from `content/` into `public/`, then copies the root
   stylesheets and bundled fonts into that output directory.

Because the build rewrites the latest-posts and archive blocks, do not manually
maintain those generated blocks. Add or edit post metadata instead, then rebuild.

## Previewing locally

After running `./build.sh`, serve the generated `public/` directory:

```sh
cd public
python3 -m http.server 8000
```

Then open:

```text
http://localhost:8000
```

You can also open `public/index.html` directly in a browser, but using a local
HTTP server is closer to how the site behaves once deployed.

## Publishing

The repository publishes through GitHub Actions:

1. Commit the source changes and generated output you want to publish.
2. Push to `main`.
3. The workflow in `.github/workflows/publish.yml` runs `./build.sh`.
4. The workflow deploys the `public/` directory to the `gh-pages` branch.

Do not edit the `gh-pages` branch directly. Treat `main` plus the Org files in
`content/` as the source of truth.

## Common update workflow

```sh
# Edit or add files under content/
./build.sh

# Preview the result
cd public
python3 -m http.server 8000
```

In another terminal, check what changed:

```sh
git status --short
git diff
```

Expected build changes usually include:

- Edited source file under `content/`
- Regenerated `content/index.org` latest-posts block
- Regenerated `content/posts/index.org` archive block
- Generated HTML under `public/`

## Troubleshooting

If `./build.sh` fails while installing packages, check network access to MELPA
and GNU ELPA. The script refreshes package metadata when `.packages/` is empty.

Build paths are resolved from the scripts themselves. You can invoke
`/path/to/repository/build.sh` from any directory. If Org ID links export
incorrectly, rebuild to refresh `.org-id-locations` and check that the target
note has an ID.

If a new post does not show on the home page or archive, confirm that it has both
`#+TITLE:` and `#+DATE:` lines. The home page shows the five most recent valid
posts.

If the deployed site differs from local output, run `./build.sh` locally and
compare `public/` before pushing. The GitHub Action rebuilds from scratch on
Ubuntu with `emacs-nox`.
