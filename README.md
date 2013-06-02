rails-log-manager
=================
Viewer to rails log file.
(STATUS: Experimental)

Implemented features:
 * Parse rails log.
 * List parsed sql to simple view.

Code Status
------------------
![build status](https://travis-ci.org/mathfur/rails-log-manager.png)

Install
-------
You need cabal-dev.
 1. $ git clone https://github.com/mathfur/rails-log-manager.git
 2. $ cd rails-log-manager
 3. $ cabal-dev install --only-dependencies
 4. $ cabal-dev configure
 5. $ cabal-dev build
 6. Add dist/build/rails-log-manager to PATH

Usage
-----
 1. Change directory to target rails root directory.
 2. $ rails-log-manager
 3. Open http://localhost:3000/

License
-------
Copyright &copy; 2012 mathfur
Distributed under the [MIT License][mit].
[MIT]: http://www.opensource.org/licenses/mit-license.php
