#!/bin/bash
# see http://www.gwern.net/About#markdown-checker

fgp () { fgrep --color=always "$@"; }
egp () { egrep --color=always "$@"; }

for PAGE in "$@"
do
    if [[ $PAGE == *.page ]]; then


        # warn if not text, perhaps due to bad copy-paste
        cat "$PAGE" | file - | fgp -v "text";

        # find bad URLS, unacceptable domains, malformed syntax, unmatched apostrophes
        fgp -e "http://dl.dropbox" -e "http://news.ycombinator.com" -e "http://github.com" \
            -e "http://www.coursera.org" -e ".wiley.com/" -e "http://www.ncbi.nlm.nih.gov/pubmed/" \
            -e "www.tandfonline.com/doi/abs/" -e "jstor.org" -e "springer.com" -e "springerlink.com" \
            -e "www.mendeley.com" "$PAGE";
        egp -e "http://www.pnas.org/content/.*/.*/.*.abstract" -e '[^\.]t\.test\(' "$PAGE";
        fgp -e "<q>" -e "</q>" -e "(www" -e ")www" -e "![](" -e "]()" -e " percent " -e "    Pearson'" \
            -e '~~~{.sh}' -e 'library("' "$PAGE";

        # look for personal uses of illegitimate statistics & weasel words, but filter out blockquotes
        fgp -e ' significant ' -e ' significantly ' -e ' obvious' -e 'basically' "$PAGE" | egrep -v '[[:space:]]*>';

        # force no highlighting, because the terminal escape codes trigger bracket-matching
        egrep --only-matching '^\[\^.*\]: ' "$PAGE" | sort | uniq --count | \
            fgrep --invert-match "      1 [^";

        # image hotlinking deprecated; impolite, and slows page loads & site compiles
        egp --only-matching '\!\[.*\]\(http://.*\)' "$PAGE";
        # indicates broken copy-paste of image location
        egp --only-matching '\!\[.*\]\(wiki/.*\)' "$PAGE";

        markdown-length-checker.hs "$PAGE";
        markdown-footnote-length.hs "$PAGE";

        HTML=$(tail -n +3 "$PAGE" | pandoc --mathml --standalone -)
        echo "$HTML" | fgp -e "<""del"">";
        echo "$HTML" | elinks -dump --force-html \
                     | fgp -e '\frac' -e '\times' -e '(http' -e ')http' -e '[http' -e ']http'  \
                           -e ' _ ' -e '[^' -e '^]' -e '<!--' -e '-->' -e '<-- ' -e '<—' -e '—>' \
                           -e '$title$' -e '<del>' \
                           -e '$description$' -e '$author$' -e '$tags$' -e '$category$' \
            -e '(!Wikipedia' -e '(!Hoogle'; # ))
    fi
done
