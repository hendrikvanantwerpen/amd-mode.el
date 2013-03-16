amd-mode
========

AMD minor mode for Emacs to easily work with AMD dependencies. Some ideas were kindly stolen from requirejs-mode.

Features:
 * Function amd-init makes current buffer into AMD module, unles it already is.
 * AMD dependencies are sorted alphabetically, not insertion order, for easier searching.
 * Function amd-add-id allows you to add a dependency by specifying a module id.

Add dependency from file:
 * Prefix handling based on /*package or package.json info.
   Is there a parser for this? Regexing?
 * Select a file on disk (require 'ido)
   Use something like bookmarks-in-ido to jump to known package folders quickly.
 * If the selected file does not exist, ask if the user wants to create it.
   If not, stop.
 * If the file is not in a known package, add it based on /*package or package.json.
   If that's not found, allow the user to trim the path and give a prefix. Ask to put a package.json there.
 * Process transformations (e.g. loader plugins) on the import.
   CamelCase the var, remove dashes etc.
 * Add the import to the file.
   Can we check that we don't use a var that's already in use?
   Make sure to restore the point after the insertion to where user was editing.
 * If the file didn't exist, create it here and open in new buffer.

In the mean time in the land of Far-far-away:
 * Start add dependency on an identifier. Search for options (think about CamelCase and dashes) that can be directly selected.
 * If we can hook into js2-mode and access the parsetree, we could even allow refactoring modules, changing their ids and even variablenames (this is tricky, the should not be bound anywhere already). Also we should do this in the whole project (project-root.el?)