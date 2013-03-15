amd-mode
========

AMD minor mode for Emacs to easily work with AMD dependencies. Some ideas were kindly stolen from requirejs-mode.

Envisioned features:
 * Add dependency by hand (specify id interactively).
 * Allow user to specify AMD libraries on disk which can be used in selection and where the id-namespace is handled corectly. That means selecting '~/dojo/dijit/_Widget.js' will result in the id 'dijit/_Widget' when the user sepcified 'dijit -> ~/dojo/dijit'.
 * Select dependencies from files. Allow configuration of folders to look and handle prefixes correctly.
 * Invoke dependency suggestion when point is on an identifier.
 * Remove dependency when point is on the id or on the varaible for that import.