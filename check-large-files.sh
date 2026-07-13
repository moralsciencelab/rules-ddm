#!/usr/bin/env bash

MAX_SIZE=$((100 * 1024 * 1024)) # 100 MB

failed=0

while IFS= read -r file; do
    # Skip deleted files
    [ -f "$file" ] || continue

    size=$(stat -c%s "$file" 2>/dev/null || stat -f%z "$file")

    if [ "$size" -gt "$MAX_SIZE" ]; then
        echo "❌ $file is $(numfmt --to=iec "$size"), exceeds GitHub's 100 MB limit."
        failed=1
    fi
done < <(git diff --cached --name-only)

if [ "$failed" -eq 1 ]; then
    echo
    echo "Commit aborted. Remove or use Git LFS for oversized files."
    exit 1
fi