MELPA_DIR := `readlink -f ./sources/melpa`
MELPA_RECIPE_DIR := MELPA_DIR + '/recipes'

# start processing collected treesit major mode files
process:
    emacs -Q --batch -l ./scripts/process-elisp-file.el --eval "(process)"

# collect treesit major modes (files) from Emacs master
update-vendor-emacs-master:
    rg "define-derived-mode.*-ts-mode" --files-with-matches -g 'sources/emacs/lisp/*mode*/*.el' ./sources/emacs | xargs -I '{}' cp '{}' ./vendor/emacs-master/

# collect treesit major modes (files) from Melpa
melpa-repo-git-clone:
    - fd "\-ts\-mode" {{MELPA_RECIPE_DIR}} | xargs -I '{}' basename '{}' | xargs -I '{}' ./scripts/melpa_repo_clone.sh {{MELPA_DIR}} '{}' | xargs -I '{}' sh -c 'cd ./vendor/melpa/ && git clone {}'
    mv ./vendor/melpa/emacs-kotlin-ts-mode ./vendor/melpa/kotlin-ts-mode

# list major modes
list-major-modes:
    @# Emacs Master
    @ls -1 ./vendor/emacs-master/
    @# Melpa
    @fd "\-ts\-mode" {{MELPA_RECIPE_DIR}} | xargs -I '{}' basename '{}'
    


