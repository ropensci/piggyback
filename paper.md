---
title: 'Managing Larger Data on a GitHub Repository'
tags:
 - data
 - GitHub
authors:
 - name: Carl Boettiger
   orcid: 0000-0002-1642-628X
   affiliation: 1
affiliations:
 - name: University of California, Berkeley
   index: 1
date: 2018-05-30
bibliography: paper.bib
---

# Summary

Because larger (> 50 MB) data files cannot easily be committed to git,
a different approach is required to manage data associated with an analysis in a 
GitHub repository.  This package provides a simple work-around by allowing larger
(up to 2 GB) data files to piggyback on a repository as assets attached to individual
GitHub releases. `piggyback` provides a workflow similar to Git LFS [@GitLFS], in 
which data files can be tracked by type and pushed and pulled to GitHub with dedicated
commands. These files are not handled by git in any way, but instead are
uploaded, downloaded, or edited directly by calls through the GitHub API [@API3]. These
data files can be versioned manually by creating different releases.  This approach
works equally well with public or private repositories.  Data can be uploaded
and downloaded programmatically from scripts. No authentication is required to
download data from public repositories.

# References
