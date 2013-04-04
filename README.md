amd-mode
========

Provide a AMD/RequireJS minor mode and auto completion for Emacs. The minor mode is intended for use with the js2-mode major mode for Javascript. The features are:

 * Easily manipulate teh 'define' header and keep dependencies and variables in sync.
 * Prevents you from adding dependencies twice or using identical variable names.
 * Integrates with packages on disk by using js-pkg.el. This allows for selecting dependencies on disk or navigating to a dependency.
 * Auto-completion of functions, properties and constructors by using js-doc-parse generated XML documentation.

AMD/RequireJS
-------------

It provides commands to manipulate the 'define' header and add/remove dependencies. It supports custom handling of AMD plugins (e.g. custom suport for 'dojo/text!' is included). By using js-pkg.el it allows for easy navigation to dependencies or adding dependencies by selecting the file.

It provides the following interactive commands:

    amd-mode     ; enable AMD mode with key bindings
    amd-init     ; [C-c a i] add AMD header to current file if not already present
    amd-add-dep  ; [C-c a d] add a dependency, specified manually
    amd-add-file ; [C-c a f] add dependency by selecting a file
    amd-add-pkg  ; [C-c a p] register AMD package
    amd-goto     ; [C-c a g] quickly select a dependency to open
    amd-remove   ; [C-c a r] remove a dependency from the header

When selecting files as dependencies, the use of js-pkg.el makes sure that packages on disk are discovered. But unless a package is known, the 'amd-goto' will not work for resources of that package. It can be usefull to register packages that are used often/in a project, so everything works from the start. This can be done interactively by using 'amd-add-pkg' and selecting a package directory or by adding the following to 'init.el':

    (js-pkg-info-by-file "~/path/to/favorite/library/")

A default configuration which enables amd-mode when js2-mode is active is easily enabled with:

   (require 'amd-mode)
   (amd-mode-config-default)

By default only module dependencies are enabled. To enable the text plugin, do:

    (require 'amd-text-plugin)
    (amd-text-plugin-setup)

and equivalent for the dojo plugins (i.e. dojo/text):

    (require 'amd-dojo-plugins)
    (amd-dojo-plugins-setup)

Writing support for a different plugin should be quite straight-forward.

Auto-completion
---------------

The auto-completion is based on 'auto-complete' can supports functions, object properties and constructors of included module dependencies. Completions have to be added to the appropriate alists (i.e. ac-amd-properties, ac-amd-functions, ac-amd-constructors). For example:

    (setq ac-amd-properties '("module/someobj" . ("prop1", "func2(arg0, _opt_arg_)")))
    (setq ac-amd-functions '("module/somefunc" . "(arg0, arg1)"))
    (setq ac-amd-constructors '("module/Class" . "(arg0, arg1)"))

You can of course write these yourself, but for packages that support jsdoc or dojodoc there is a better way. First generate a documentation xml in dojov1 format using js-doc-parse (https://github.com/wkeese/js-doc-parse). The generate a lisp file from that using the provided XSL:

    xsltproc --stringparam name PACKAGENAME ac-amd-process-dojov1.xsl package-details.xml > ac-amd-PACKAGENAME-completions.el

Then in Emacs add those completions as follows:

    (require 'ac-amd-PACKAGENAME-completions)
    (ac-amd-PACKAGENAME-completions-setup)

A default configuration which enables auto-completion when amd-mode is active is easily enabled with:

   (require 'ac-amd)
   (ac-amd-config-default)

Wish list & known issues
------------------------

 * Updating the header sometimes results in some extra whitespace before the header.
 * Have a quick way to jump to a known package's root folder when selecting a file.
 * Handle global and local package definitions in a smart way. The problem is that package definitions are not just global or buffer local. They actually correspond to dependencies and are maybe best defined on a package level. We could keep them in a hash/alist by package(directory). We could even check/update the package file if we start using a new dependency, although parsing JSON might be required to do this reliably. Use the dependency versions maybe?

See also
--------

 * The requirejs-mode which served as inspiration at https://github.com/purcell/requirejs-mode.
 * AMD documentation at http://requirejs.org.


Install
-------

Manually: put amd-*.el file in your load-path and (require 'amd-mode) and/or .

El-get: Evaluate the following snippet and install 'amd-mode' with el-get-install.

   (setq el-get-sources
    (cons '(:name amd-mode
            :type github
            :pkgname "hendrikvanantwerpen/amd-mode.el"
            :depends (js2-mode js-pkg semver auto-complete dash s))
          el-get-sources))

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
