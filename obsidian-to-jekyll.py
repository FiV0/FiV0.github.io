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
    assets_dir = script_dir / 'assets'
    output_path = posts_dir / filename

    # Process images: copy to assets and update paths
    content, copied_images = process_images(content, obsidian_path, assets_dir)

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
    if copied_images:
        print(f"  Copied images: {', '.join(copied_images)}")

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
