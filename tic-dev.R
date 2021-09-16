# installs dependencies, runs R CMD check, runs covr::codecov()

get_stage("install") %>%
  add_step(step_install_github(c("bd-R/bdutilities@dev",
                                 "bd-R/bdDwC@dev",
                                 'bd-R/bdutilities.app@dev',
                                 'bd-R/bddwc.app@dev',
                                 'bd-R/bdchecks@dev',
                                 'bd-R/bdchecks.app@dev',
                                 'bd-R/bdclean@dev',
                                 'bd-R/bdverse@dev',
                                 'bd-R/bdtests@dev')))

do_package_checks()

if (ci_on_ghactions() && ci_has_env("BUILD_PKGDOWN")) {
  # creates pkgdown site and pushes to gh-pages branch
  # only for the runner with the "BUILD_PKGDOWN" env var set
  do_pkgdown()
}
