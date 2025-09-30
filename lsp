#!/bin/sh

# Evil jq alakazam to extract on which character the GoToDefinition was triggered
# The target will be saved in the following format
#<filename> <line>.<column>
target="$(jq -r --arg base_path "$(pwd)/" '.params | "\(.textDocument.uri | ltrimstr("file://") | ltrimstr($base_path)) \(.position.line).\(.position.character)"')"

# The above format also happens to be exactly what a "use" looks like in MLton's def-use output
# Find that line (grep) and get just the line number (cut)
lineno="$(grep -nm 1 -E "^[[:space:]]*$target[[:space:]]*$" def-use | cut -d : -f 1)"

# Starting from line n, backtrack the def-use file to find the "def"
head -n "$lineno" | tac | grep -m 1 "^variable"
