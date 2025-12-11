This is the personal website of Finn Völkel, hosted by [GitHub Pages](http://pages.github.com). You can find it at https://finnvolkel.com .

It is mainly a clone of Joshua Lande's page which can be found at https://github.com/joshualande/joshualande.github.io .

### Local development

installing the dependencies
```bash
gem install jekyll bundler
```
you might need to configure bundler to use your user directory:
```
bundle config set --local path 'vendor/bundle'
```
installing or updating the gems
```bash
bundle install
```
build the site locally
```bash
bundle exec jekyll serve
```
it then be served under `localhost:4000`.

### Creating posts from Obsidian

Use the `obsidian-to-jekyll.py` script to convert Obsidian markdown files to Jekyll posts:

```bash
# Normal post (listed in archive)
python obsidian-to-jekyll.py '/path/to/obsidian/My Post.md'

# Hidden post (accessible by link but not listed in archive)
python obsidian-to-jekyll.py --hidden '/path/to/obsidian/My Post.md'
```

The script handles:
- Obsidian wikilink images (`![[image.png]]`) - copies to `assets/` and converts syntax
- Obsidian wikilinks (`[[note|display]]`) - converts to plain text
- Display math (`$$...$$`) - ensures proper line spacing for MathJax
- Bare URLs - converts to clickable links
