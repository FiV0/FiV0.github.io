#!/usr/bin/env python3
"""
Convert an Obsidian markdown file to a Jekyll blog post.

Usage:
    python obsidian-to-jekyll.py <path-to-obsidian-file>
"""

import sys
import re
from datetime import datetime
from pathlib import Path


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


def create_jekyll_post(obsidian_file_path, custom_date=None):
    """
    Convert an Obsidian markdown file to a Jekyll post.

    Args:
        obsidian_file_path: Path to the Obsidian markdown file
        custom_date: Optional datetime object for the post date (defaults to today)
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
    output_path = posts_dir / filename

    # Create Jekyll front matter
    front_matter = f"""---
layout: post
title: {title}
comments: true
redirect_from: "/{date_path}/{slug}/"
permalink: {slug}
---

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

    return output_path


def main():
    if len(sys.argv) < 2:
        print("Usage: python obsidian-to-jekyll.py <path-to-obsidian-file>")
        print("\nExample:")
        print("  python obsidian-to-jekyll.py '/home/user/obsidian/My Post.md'")
        sys.exit(1)

    obsidian_file = sys.argv[1]
    create_jekyll_post(obsidian_file)


if __name__ == '__main__':
    main()
