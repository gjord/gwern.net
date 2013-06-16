#!/bin/bash
# see http://www.gwern.net/About#markdown-checker

for PAGE in "$@"
do
    if [[ $PAGE == *.page ]]; then

        # warn if not text, perhaps due to bad copy-paste
        cat "$PAGE" | file - | fgrep -v "text";

        # find bad URLS, unacceptable domains, malformed syntax, unmatched apostrophes, illegitimate statistics
        fgrep -e "http://dl.dropbox" -e "http://news.ycombinator.com" -e "http://www.coursera.org" \
              -e ".wiley.com/" -e "http://www.ncbi.nlm.nih.gov/pubmed/" -e "www.tandfonline.com/doi/abs/" \
                 "$PAGE";
        egrep -e "http://www.pnas.org/content/.*/.*/.*.abstract" "$PAGE";
        fgrep -e "<q>" -e "</q>" -e "(www" -e ")www" -e "![](" -e " percent " -e "    Pearson'" \
              -e '~~~{.sh}' -e ' significant ' -e ' significantly ' "$PAGE";
        egrep --only-matching '^\[\^.*\]: ' "$PAGE" | sort | uniq --count | \
              fgrep --invert-match "      1 [^";

        # image hotlinking deprecated; impolite, and slows page loads & site compiles
        egrep --only-matching '\!\[.*\]\(http://.*\)' "$PAGE";

        markdown-length-checker "$PAGE";
        markdown-footnote-length "$PAGE";

        declare -r local HTML=`cat "$PAGE" | tail -n +3 | pandoc --mathml --standalone -`
        echo $HTML | fgrep -e "<""del"">";
        echo $HTML | elinks -dump --force-html \
                   | fgrep -e '\frac' -e '\times' -e '(http' -e ')http' -e '[http' -e ']http'  \
                           -e ' _ ' -e '[^' -e '^]' -e '<!--' -e '-->' -e '<-- ' -e '<—' -e '—>' \
                           -e '$title$' -e '<del>' \
                           -e '$description$' -e '$author$' -e '$tags$' -e '$category$' \
            -e '(!Wikipedia' -e '(!Hoogle'; # ))
    fi
done
