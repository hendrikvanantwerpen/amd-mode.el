amd-mode
========

AMD minor mode for Emacs to easily work with AMD dependencies. Some ideas were kindly stolen from requirejs-mode.

Features:
 * Function amd-init makes current buffer into AMD module, unless it already is.
 * AMD dependencies are sorted alphabetically, not insertion order, for easier searching. When a module is added twice, we just print the existing variable for it. Variables are unique in the argument list (but they may be used elsewehere in the code. Module IDs are normalized.
 * Function amd-add-id allows you to add a dependency by specifying a module id.
 * Function amd-add-file asks to create the file if it doesn't exist yet.

TODO:
 * Is there a chance we have nested packages?
 * Use relative path if include is from same package as us.
 * The package table should be buffer local (can differ per project, dir-vars etc).
   System defaults should be possible though.
 
Add dependency from file:
 * Prefix handling based on package.json info.
   Is there a parser for this? Regexing?
 * Select a file on disk (require 'ido)
   Use something like bookmarks-in-ido to jump to known package folders quickly.
 * If the selected file does not exist, ask if the user wants to create it.
   If not, stop.
 * If the file is not in a known package, add it based on package.json.
   If that's not found, allow the user to trim the path and give a prefix. Ask to put a package.json there.
 * Process transformations (e.g. loader plugins) on the import.
   CamelCase the var, remove dashes etc.
 * Add the import to the file.
   Can we check that we don't use a var that's already in use?
   Make sure to restore the point after the insertion to where user was editing.
 * If the file didn't exist, create it here and open in new buffer.

Open dependency:
 * Function amd-goto-dependency
 * Present list of dependencies that resolve to files
 * On select, open buffer with that one

Package lookup:
 * find-package-and-id-for-file (this does the auto-add for faster lookup)
 * add-package (name . directory) ; for project setups
 * find-file-for-id (if package is not known, allow selection of package and add)
