#!/usr/bin/env python3
"""
Convert an Obsidian markdown file to a Jekyll blog post.

Usage:
    python obsidian-to-jekyll.py <path-to-obsidian-file>
"""

import sys
import re
import shutil
from datetime import datetime
from pathlib import Path


def process_images(content, obsidian_path, assets_dir):
    """
    Find image references, copy images to assets, and update paths.

    Handles Obsidian's wikilink image syntax: ![[image_name.png]]

    Args:
        content: The markdown content
        obsidian_path: Path to the source Obsidian file
        assets_dir: Path to the Jekyll assets directory

    Returns:
        Updated content with corrected image paths
    """
    # Find all Obsidian wikilink image references like ![[image_name.png]]
    image_pattern = r'!\[\[([^\]]+\.(?:png|jpg|jpeg|gif|svg|webp))\]\]'
    matches = re.findall(image_pattern, content, re.IGNORECASE)

    source_images_dir = obsidian_path.parent / 'images'
    copied_images = []

    for image_name in matches:
        source_image = source_images_dir / image_name
        dest_image = assets_dir / image_name

        if source_image.exists():
            shutil.copy2(source_image, dest_image)
            copied_images.append(image_name)
        else:
            print(f"  Warning: Image not found: {source_image}")

    # Convert Obsidian wikilinks to standard markdown with assets path
    # ![[image.png]] -> ![image](assets/image.png)
    def replace_image(match):
        image_name = match.group(1)
        # Use filename without extension as alt text
        alt_text = Path(image_name).stem
        return f'![{alt_text}](assets/{image_name})'

    content = re.sub(image_pattern, replace_image, content, flags=re.IGNORECASE)

    return content, copied_images


def transform_urls(content):
    """
    Transform bare URLs into markdown links.

    https://example.com -> <https://example.com>

    Skips URLs that are already in markdown link syntax [text](url) or <url>.
    """
    # Pattern for bare URLs not already in markdown links
    # Negative lookbehind for ]( or < to avoid matching URLs already in links
    url_pattern = r'(?<!\]\()(?<![<\[])https?://[^\s\)\]>]+'

    def make_link(match):
        url = match.group(0)
        return f'<{url}>'

    content = re.sub(url_pattern, make_link, content)
    return content


def find_post_permalink(note_name, posts_dir):
    """
    Find a matching blog post and extract its permalink.

    Args:
        note_name: The note name from the wikilink
        posts_dir: Path to the _posts directory

    Returns:
        Permalink string if found, None otherwise
    """
    # Slugify the note name to match against post filenames
    slug = slugify(note_name)

    # Search for posts containing this slug
    for post_file in posts_dir.glob('*.md'):
        if slug in post_file.stem:
            # Read the post to extract permalink from front matter
            try:
                with open(post_file, 'r', encoding='utf-8') as f:
                    content = f.read()
                    # Extract permalink from front matter
                    permalink_match = re.search(r'^permalink:\s*(.+)$', content, re.MULTILINE)
                    if permalink_match:
                        return permalink_match.group(1).strip()
            except Exception:
                continue

    return None


def transform_wikilinks(content, posts_dir=None):
    """
    Transform Obsidian wikilinks to plain text or markdown links.

    [[#Header]] -> [Header](#header)
    [[#Header|display text]] -> [display text](#header)
    [[note_name|display text]] -> [display text](permalink) if post found, else display text
    [[note_name]] -> [note_name](permalink) if post found, else note_name

    Args:
        content: The markdown content
        posts_dir: Path to _posts directory for finding blog post links
    """
    # Handle internal header links: [[#Header|display]] -> [display](#header)
    def header_link_with_display(match):
        header = match.group(1)
        display = match.group(2)
        anchor = slugify(header)
        return f'[{display}](#{anchor})'

    content = re.sub(r'\[\[#([^\]|]+)\|([^\]]+)\]\]', header_link_with_display, content)

    # Handle internal header links: [[#Header]] -> [Header](#header)
    def header_link(match):
        header = match.group(1)
        anchor = slugify(header)
        return f'[{header}](#{anchor})'

    content = re.sub(r'\[\[#([^\]]+)\]\]', header_link, content)

    # Handle blog post links: [[note|display]] -> [display](permalink)
    if posts_dir:
        def post_link_with_display(match):
            note_name = match.group(1)
            display = match.group(2)
            permalink = find_post_permalink(note_name, posts_dir)
            if permalink:
                return f'[{display}]({permalink})'
            else:
                # If no post found, just return the display text
                return display

        content = re.sub(r'\[\[([^\]|#]+)\|([^\]]+)\]\]', post_link_with_display, content)

        # Handle blog post links: [[note]] -> [note](permalink)
        def post_link(match):
            note_name = match.group(1)
            permalink = find_post_permalink(note_name, posts_dir)
            if permalink:
                return f'[{note_name}]({permalink})'
            else:
                # If no post found, just return the note name
                return note_name

        content = re.sub(r'\[\[([^\]|#]+)\]\]', post_link, content)
    else:
        # Fallback to old behavior if posts_dir not provided
        # Match [[note|display]] and keep only display text
        content = re.sub(r'\[\[[^\]|]+\|([^\]]+)\]\]', r'\1', content)
        # Match [[note]] without display text and keep note name
        content = re.sub(r'\[\[([^\]]+)\]\]', r'\1', content)

    return content


def transform_display_math(content):
    """
    Transform $$ ... $$ display math to be on its own lines with blank lines around.

    Jekyll/MathJax requires display math to be on separate lines for proper rendering.
    """
    # First, handle single-line $$...$$ equations
    # Match $$...$$ on a single line (content doesn't contain newlines)
    # Use a function to add newlines around it

    def ensure_block_math(match):
        """Ensure display math has blank lines around it."""
        return '\n\n' + match.group(0) + '\n\n'

    # Pattern for single-line display math: $$...$$
    # This matches $$ followed by content (no $) followed by $$
    single_line_pattern = r'\$\$[^$]+\$\$'

    # Replace all single-line display math with properly spaced versions
    content = re.sub(single_line_pattern, ensure_block_math, content)

    # Clean up excessive newlines (more than 2 consecutive)
    content = re.sub(r'\n{3,}', '\n\n', content)

    # Also clean up cases where blank line + text + blank line creates too much space
    # But preserve the structure

    return content


def slugify(text):
    """Convert text to a URL-friendly slug."""
    # Convert to lowercase
    text = text.lower()
    # Replace spaces and underscores with hyphens
    text = re.sub(r'[\s_]+', '-', text)
    # Remove non-alphanumeric characters (except hyphens)
    text = re.sub(r'[^\w\-]', '', text)
    # Remove multiple consecutive hyphens
    text = re.sub(r'-+', '-', text)
    # Strip leading/trailing hyphens
    text = text.strip('-')
    return text


def create_jekyll_post(obsidian_file_path, custom_date=None, hidden=False):
    """
    Convert an Obsidian markdown file to a Jekyll post.

    Args:
        obsidian_file_path: Path to the Obsidian markdown file
        custom_date: Optional datetime object for the post date (defaults to today)
        hidden: If True, add hidden: true to hide from archive but keep accessible by link
    """
    obsidian_path = Path(obsidian_file_path)

    if not obsidian_path.exists():
        print(f"Error: File not found: {obsidian_file_path}")
        sys.exit(1)

    if not obsidian_path.suffix == '.md':
        print(f"Error: File must be a markdown file (.md): {obsidian_file_path}")
        sys.exit(1)

    # Read the Obsidian file content
    with open(obsidian_path, 'r', encoding='utf-8') as f:
        content = f.read()

    # Use the filename without extension as title
    title = obsidian_path.stem

    # Use provided date or current date
    post_date = custom_date if custom_date else datetime.now()
    date_str = post_date.strftime('%Y-%m-%d')
    date_path = post_date.strftime('%Y/%m/%d')

    # Create slug from title
    slug = slugify(title)

    # Create filename
    filename = f"{date_str}-{slug}.md"

    # Get the script directory and construct posts directory path
    script_dir = Path(__file__).parent
    posts_dir = script_dir / '_posts'
    assets_dir = script_dir / 'assets'
    output_path = posts_dir / filename

    # Process images: copy to assets and update paths
    content, copied_images = process_images(content, obsidian_path, assets_dir)

    # Transform Obsidian wikilinks to plain text or blog post links
    content = transform_wikilinks(content, posts_dir)

    # Transform bare URLs into links
    content = transform_urls(content)

    # Transform display math to be on its own lines
    content = transform_display_math(content)

    # Create Jekyll front matter
    hidden_line = "hidden: true\n" if hidden else ""
    front_matter = f"""---
layout: post
title: {title}
comments: true
redirect_from: "/{date_path}/{slug}/"
permalink: {slug}
{hidden_line}---

"""

    # Combine front matter with content
    jekyll_content = front_matter + content

    # Write the Jekyll post
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(jekyll_content)

    print(f"Successfully created Jekyll post:")
    print(f"  File: {output_path}")
    print(f"  Title: {title}")
    print(f"  Date: {date_str}")
    print(f"  Permalink: {slug}")
    if copied_images:
        print(f"  Copied images: {', '.join(copied_images)}")

    return output_path


def main():
    if len(sys.argv) < 2:
        print("Usage: python obsidian-to-jekyll.py [--hidden] <path-to-obsidian-file>")
        print("\nOptions:")
        print("  --hidden   Hide post from archive (accessible by link but not listed)")
        print("\nExample:")
        print("  python obsidian-to-jekyll.py '/home/user/obsidian/My Post.md'")
        print("  python obsidian-to-jekyll.py --hidden '/home/user/obsidian/My Post.md'")
        sys.exit(1)

    hidden = False
    if '--hidden' in sys.argv:
        hidden = True
        sys.argv.remove('--hidden')

    obsidian_file = sys.argv[1]
    create_jekyll_post(obsidian_file, hidden=hidden)


if __name__ == '__main__':
    main()
