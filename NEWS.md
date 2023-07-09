# piggyback (development version)

* Fix bug in `pb_upload()` to correctly resolve `"latest"` tag - if there is no release tag actually named "latest" it will use the first release from `pb_releases()`.  [#75] 
* Make `pb_download()` and `pb_info()` also resolve `"latest"` similarly: if there is no release tag named "latest", use first release from `pb_releases()`
* Updated test coverage to use GHA
* Fixed error handling for `pb_list()` for no release. 
* `pb_list()` now respects the option `"piggyback.verbose"`
* Fix download token handling [#88]
* `pb_upload()` no longer prints out extra newlines [#93]
* `pb_new_release()` now warns and exits early instead of failing if a release already exists. [#95]
* Fixup test issues [#100]


# piggyback 0.1.4

* The progress bar argument `show_progress` in `pb_upload()` and `pb_download()` now defaults to `interactive()` [#72]
* Fix bug in `pb_download()` for downloading without a `gh::gh_token()` (mostly on Windows?) [#77]
* Fix bug introduced by above bugfix - missed Authorization in header
* `guess_repo()` now uses `gh::gh_tree_remote()` rather than gert - this eliminates the gert dependency. [#80]
* `pb_release_delete()` introduced to delete existing releases. [#81]
* `pb_new_release()` renamed to `pb_release_create()` to sync with the new delete function. 
* Fix offer to create new release in `pb_upload()` - also switch to using `rlang::is_interactive()` to maybe one day test this.
* Tests rewritten to primarily use GHA and write to/from the ropensci/piggyback repo.
* Added `httr::RETRY()` behaviour to `pb_download()`.


# piggyback 0.1.3

* fix bug in `pb_upload()` for uploading to a release with no assets [#67]
* avoid implicit dependency on `tibble` [#70]

# piggyback 0.1.2

* update intro vignette to remove all mentions of `pb_track()`, `pb_push()`, and `pb_pull()` which were removed as of version 0.0.0.9900
* `pb_upload()` now handles the `dir` argument to control relative path directories.
* update intro vignette to remove mention of path name handling and instead provide examples of how path names are handled.
* update intro vignette instructions for git authentication
* `pb_new_release()` now reports HTTP errors when attempting to create a new release and returns the contents of the error if it fails. 
* `pb_releases()` created - it returns a list of releases available in the repository.
* Internal function `pb_info()` refactored to search for the specified tag(s) which should improve performance. Should handle multiple tags gracefully.
* Internal function `pb_info()` (and therefore `pb_list()`, `pb_download()`, `pb_download_url()`) no longer ask about creating new releases if the release is not found. 
* `pb_upload()` is now the only function that offers (interactively) to create a new release if release is not found. If noninteractive, user must run `pb_new_release()` manually prior to uploading. 
* CLI messaging now consistently uses `{cli}` package and no longer uses clisymbols or crayon - this is to align with the imports from the `{gh}` package.
* Documentation updated.
* Add options("piggyback.verbose") TRUE/FALSE to control verbosity/messaging levels.

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
