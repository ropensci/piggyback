Dear CRAN Maintainers,

This release fixes a bug that was introduced in the previous release,
which prevented functions from seeing more than 30 file assets on 
any given release.  This release also improves the user interface
regarding the overwrite behavior, as detailed in NEWS.md.  


## Test environments

* local OS X install, R 3.5.2
* ubuntu 14.04 (on travis-ci), R 3.5.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 note

