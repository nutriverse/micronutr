---
name: Initial CRAN release
about: Checklist of tasks for initial CRAN release
title: Checklist of tasks for initial CRAN release
labels: documentation
assignees: ernestguevarra
---

* [ ] `usethis::use_news_md()`
* [ ] `usethis::use_cran_comments()`
* [ ] Update (aspirational) install instructions in `README`
* [ ] Proofread `Title:` and `Description:`
* [ ] Check that all exported functions have `@returns` and `@examples`
* [ ] Check that `Authors@R:` includes a copyright holder (role 'cph')
* [ ] Check licensing of included files
* [ ] Review https://github.com/DavisVaughan/extrachecks
