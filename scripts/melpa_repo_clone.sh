#!/bin/sh
if [ $# -lt 2 ]; then
    echo "No enough arguments were passed."
    exit 1
fi

MELPA_DIR=$1
RECIPE_NAME=$2

emacs -Q --batch \
      -l "${MELPA_DIR}/package-build/package-recipe.el" \
      -l ./scripts/melpa_recipe_clone.el \
      --eval "(get-recipe-upstream-url \"${RECIPE_NAME}\")"
