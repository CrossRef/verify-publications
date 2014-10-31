# verify-publications

Compare the SCOPUS title list with our API plus custom fetching from Elsevier site.

The contents of `resources/title_list.csv` is taken from `http://www.elsevier.com/online-tools/scopus/content-overview` and is in the format `title, pissn, eissn, publisher name`.

## Usage

FIXME: explanation

Create `/tmp/cache`. If you're on Windows, create equivalent directory and change value of `cache-dir` in code. 

Then just `lein run`.

