This is the personal website of Finn Völkel, hosted by [GitHub Pages](http://pages.github.com). You can find it at https://FiV0.github.io.

It is mainly a clone of Joshua Lande's page which can be found at https://github.com/joshualande/joshualande.github.io .

### Local development

installing the dependencies
```bash
gem install jekyll bundler
```
you might need to configure bundler to use your user directory:
```
bundle config set --local path 'vendor/bundle'
```
installing or updating the gems
```bash
bundle install
```
build the site locally
```bash
bundle exec jekyll serve
```
it then be served under `localhost:4000`.
