# piggyback 0.1.1

* switch to gh::gh_token() for token management.  Still supports the same env var approach, but also compatible with `gitcreds` and other use.
* resolve issue in `pb_upload()` when creating a new tag in the process, previously data would be attached to the previously `latest` tag instead of the newly created one. 
* resolve issue in `pb_download()` where httr would report a 401 status even after data successfully downloads. 

# piggyback 0.1.0

* address remaining authentication issue in changes to GitHub API (on pb_upload()) [#47]
* Use flat file structure on upload/download instead of encoding path [#48]
* improve performance via more aggressive memoising of `pb_info()` calls, inceasing default `piggyback_cache_duration` to 10 minutes [#46]
* Resolve bug introduced by API changes that would stop creation of tags on repos with default branch called `main` or without previous releases [#48]


# piggyback 0.0.12

* address issues in authentication due to changes in GitHub API (#37)

# piggyback 0.0.11 2020-02-25

* `guess_repo()` now infers a remote when there are multiple associated with the repo. The "upstream" (preferred) or "origin" repo is selected if either exists, otherwise the function errors and asks the user to explicitly specify a repo (#31).
* `release_info()` now works properly when there are no existing releases, which enables the usage of `pb_new_release()` on repos without a release (#29).
* Fix error on `pb_info()` under certain cases which resulted in `Error in a[[1]] : subscript out of bounds`, (#36)
* Fix CRAN unit-test on deleting file

# piggyback 0.0.10 2018-02-06

* Improve interface regarding `overwrite` behavior in `pb_upload()` (#25)
* Bugfixes for errors introduced in 0.0.9: 
   - Access all assets on a release instead of first 30.  This could break upload and download. (#23, #24)
   - Uploading of directory paths could cause download errors in `pb_download()`. (#24, #26)

# piggyback 0.0.9, 2019-01-08

* Enable re-upload and deletion of partially uploaded files (#19)

# piggyback 0.0.8, 2018-10-06

* Updates to documentation, streamlining tests
* remove dependency on `utils::askYesNo` which is only available in R >= 3.5.0

# piggyback 0.0.7, 2018-09-30

* Initial release to CRAN

--------------------------------------------

# piggyback 0.0.6, 2018-09-21

* bugfix for migrating unit test

# piggyback 0.0.6, 2018-09-21

* bugfix for migrating unit test, JOSS submission

# piggyback 0.0.5, 2018-09-21

* initial Onboarding to rOpenSci

# piggyback 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
