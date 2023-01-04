# Contributing to shoredate

<!-- This CONTRIBUTING.md is adapted from https://gist.github.com/peterdesmet/e90a1b0dc17af6c12daf6e8b2f044e7c -->

[repo]: https://github.com/isakro/shoredate
[issues]: https://github.com/isakro/shoredate/issues
[new_issue]: https://github.com/isakro/shoredate/issues/new
[email]: mailto:isakroa@protonmail.com

## Code of conduct

Please note that this project is released with a [Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

## How you can contribute

There are several ways you can contribute to this project. If you want to know more about why and how to contribute to open source projects like this one, see this [Open Source Guide](https://opensource.guide/how-to-contribute/).

### Propose an idea

Have an idea for a new shoredate feature or how the methods used could be improved? Take a look at the documentation and [issue list][issues] to see if it isn't included or suggested yet. If not, suggest your idea as an [issue on GitHub][new_issue]. It helps to:

* Explain in detail how it would work.
* Keep the scope as narrow as possible.

See below if you want to contribute code for your idea as well.

### Report a bug 

Discovered a bug? Report it as an [issue on GitHub][new_issue] so it can be fixed. A good bug report makes it easier to handle, so please include:

* Your operating system name and version (e.g. Mac OS 10.13.6).
* Any details about your local setup that might be helpful in troubleshooting.
* Detailed steps to reproduce the bug.

### Improve the documentation

Noticed a typo or think a function could use a better example? Good documentation makes all the difference, so your help to improve it is very welcome.

Functions are described as comments near their code and translated to documentation using [`roxygen2`](https://klutometis.github.io/roxygen/). If you want to improve a function description:

1. Go to `R/` directory in the [code repository][repo].
2. Look for the file with the name of the function.
3. [Propose a file change](https://help.github.com/articles/editing-files-in-another-user-s-repository/) to update the function documentation in the roxygen comments (starting with `#'`).

### Contribute code 

Care to fix bugs or implement new functionality for shoredate? Have a look at the [issue list][issues] and leave a comment on the things you want to work on. See also the development guidelines below.

## Development guidelines

The [GitHub flow](https://guides.github.com/introduction/flow/) is used for the development of this package.

1. Fork [this repo][repo] and clone it to your computer. To learn more about this process, see [this guide](https://guides.github.com/activities/forking/).
2. If you have forked and cloned the project before and it has been a while since you worked on it, [pull changes from the original repo](https://help.github.com/articles/merging-an-upstream-repository-into-your-fork/) to your clone by using `git pull upstream master`.
3. Open the RStudio project file (`.Rproj`).
4. Make your changes:
    * Write your code.
    * Test your code (bonus points for adding unit tests).
    * Document your code (see function documentation above).
    * Check your code with `devtools::check()` and aim for 0 errors and warnings.
5. Commit and push your changes.
6. Submit a [pull request](https://guides.github.com/activities/forking/#making-a-pull-request).
