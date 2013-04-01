amd-mode
========

Plugin provides a minor mode for handling JavaScript AMD module and plugin dependencies. It uses the package.json convention to recognize packages, deduce relative urls and convert dependencies to file paths. It provides the following functions:

    amd-mode     ; enable AMD mode with key bindings
    amd-init     ; [C-c a i] add AMD header to current file if not already present
    amd-add-dep  ; [C-c a d] add a dependency, specified manually
    amd-add-file ; [C-c a f] add dependency by selecting a file
    amd-add-pkg  ; [C-c a p] register AMD package
    amd-goto     ; [C-c a g] quickly select a dependency to open

Features
--------

 * Initialize file as AMD module when header doesn't exist yet. Automatically done when adding the first dependency.
 * Resilient to adding the same dependency or package twice by using normalized paths and resources.
 * Generates variable names for dependencies automatically and prevents duplicates.
 * When dependency file doesn't exist, give the option to create it.
 * Searches for package when adding a file and adds it automatically. For later it is advised to put them in init.el or directory variables.
 * Dependencies are put relative if they are in the same package.

Wish list
---------

 * Have a quick way to jump to a known package's root folder when selecting a file.
 * Handle global and local package definitions in a smart way. The problem is that package definitions are not just global or buffer local. They actually correspond to dependencies and are maybe best defined on a package level. We could keep them in a hash/alist by package(directory). We could even check/update the package file if we start using a new dependency, although parsing JSON might be required to do this reliably.

See also
--------

 * The requirejs-mode which served as inspiration at https://github.com/purcell/requirejs-mode.
 * AMD documentation at http://requirejs.org.

License
-------

    Copyright 2013 Hendrik van Antwerpen

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
