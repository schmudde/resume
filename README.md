# resume

This resume dynamically pulls the newest information from my website and updates.

## Usage

This generates a HTML resume. `lein run > test.html` creates the file. Use Prince to turn the HTML file into a PDF: `prince --page-size=A4 --page-margin=0mm resume.html -o resume.pdf`.

## License

Copyright © 2016 D. Schm&uuml;dde

Distributed under the Eclipse Public License
