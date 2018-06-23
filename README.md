# grid

This is a modified version of the base R package `grid`. You can find the actual source here: https://github.com/wch/r-source/tree/trunk/src/library/grid

# Rebuild and Install

Overriding the base `grid` requires a special installation process. Use the following to rebuild and install:

```r
source("install-grid.R")
```

# Modifications

To reinstall `grid`, most of the relevant info was removed from `DESCRIPTION`. All make files were deleted, and the `man` and `vignettes` directories were removed. See the `r-source` `grid` for those files.



