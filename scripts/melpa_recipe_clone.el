(setq package-build-use-git-remote-hg nil)
(setq package-build-recipes-dir "./sources/melpa/recipes")


(defun get-recipe-upstream-url (recipe)
  (print (package-recipe--upstream-url (package-recipe-lookup recipe))))

