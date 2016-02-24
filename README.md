java-bridge fork
===========
The original package at [hackage](https://hackage.haskell.org/package/java-bridge).

-----


* Fixes to compile with GHC 7.10.3.

* Some new options added for `j2hs`.


WARNING
-------

(!!!) This version contains an ad-hoc fix to avoid name collision on some java enum generation (DateFormat, TextFormat).
The implications of the change __have never been examened__, it's __possible__ that __enums are broken__ (!!!)


