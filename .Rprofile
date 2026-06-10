# .Rprofile
# We deliberately do NOT source renv/activate.R. Renv's autoloader does a
# dependency-discovery scan of the entire project on every R startup, and
# with hundreds of MB of JSONL data under data/ and output/, that scan
# takes 5-10+ minutes (effectively
# hangs Rscript). The .renvignore + synchronized.check option do not
# reliably skip it.
#
# Instead we add the renv project library to .libPaths() manually. This
# gives us all the benefits of renv (pinned package versions installed
# under renv/library/) without the autoloader scan. Explicit calls like
# `renv::restore()`, `renv::status()`, `renv::snapshot()` still work
# because the renv package itself is installed in that library.
#
# To re-enable the full renv autoloader, comment out the local() block
# below and uncomment `source("renv/activate.R")`.

local({
  renv_lib <- file.path("renv", "library", "macos", "R-4.4",
                        "aarch64-apple-darwin20")
  if (dir.exists(renv_lib)) {
    .libPaths(c(renv_lib, .libPaths()))
  }
  options(
    renv.config.synchronized.check = FALSE,
    renv.config.auto.snapshot      = FALSE,
    renv.config.autoloader.enabled = FALSE
  )
})

# source("renv/activate.R")
