amd-mode
========

AMD minor mode for Emacs to easily work with AMD dependencies. Some ideas were kindly stolen from requirejs-mode.

Features:
 * Function amd-init makes current buffer into AMD module, unless it already is.
 * AMD dependencies are sorted alphabetically, not insertion order, for easier searching. When a module is added twice, we just print the existing variable for it. Variables are unique in the argument list (but they may be used elsewehere in the code. Module IDs are normalized.
 * Function amd-add-id allows you to add a dependency by specifying a module id.
 * Function amd-add-file asks to create the file if it doesn't exist yet.
 * Resource are created relative when appropriate.
 * Will ask to create package.json or dependency when non-existent.

TODO:
 * Have buffer/directory local different package tables. Maybe together with global ones.
 * Easy way of jumping to known package folders in the file selector.
