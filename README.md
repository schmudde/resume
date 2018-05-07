# Resume

This resume dynamically pulls the newest information from my website and updates.

## Dependencies

- [Bootstrap 3.3.7](http://getbootstrap.com/docs/3.3/getting-started/#download)
- [Less CSS](http://lesscss.org/): `sudo npm install -g less`, `which less`
- [Prince](https://www.princexml.com/)
    - Requires [libpng12](https://packages.ubuntu.com/xenial/libpng12-0)
    - libpng12 for 16.04 shouldn't conflict with libpng16 for 17.10
- [Font Awesome 4.5](https://fontawesome.com/v4.7.0/)
- `font-family: 'Avenir Next', 'Helvetica Neue', Arial, sans-serif;`

## CSS

```
resources/compile-css.sh
  |- css
    |- /bootstrap-3.3.7
      |- /less
        |- /bootstrap.less
        |- /print.less
        |- /variables.less
        |- /...
  |- print.less
  |- variables.less
  |- styles.css (included)
```

Symlinks to `print.less` and `variables.less` must be made manually!

## Usage

This generates a HTML resume. `lein run > test.html` creates the file. Use Prince to turn the HTML file into a PDF: `prince --page-size=A4 --page-margin=0mm resume.html -o resume.pdf`.

## License

Copyright © 2016 D. Schm&uuml;dde

Distributed under the Eclipse Public License
