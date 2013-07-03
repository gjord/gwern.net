#!/bin/bash
# see http://www.gwern.net/About#markdown-checker

for PAGE in "$@"
do
    if [[ $PAGE == *.page ]]; then

        function fgp { fgrep --color=always "$@"; }
        function egp { egrep --color=always "$@"; }

        # warn if not text, perhaps due to bad copy-paste
        cat "$PAGE" | file - | fgp -v "text";

        # find bad URLS, unacceptable domains, malformed syntax, unmatched apostrophes, illegitimate statistics
        fgp -e "http://dl.dropbox" -e "http://news.ycombinator.com" -e "http://github.com" \
            -e "http://www.coursera.org" -e ".wiley.com/" -e "http://www.ncbi.nlm.nih.gov/pubmed/" \
            -e "www.tandfonline.com/doi/abs/" -e "jstor.org"   "$PAGE";
        egp -e "http://www.pnas.org/content/.*/.*/.*.abstract" "$PAGE";
        fgp -e "<q>" -e "</q>" -e "(www" -e ")www" -e "![](" -e " percent " -e "    Pearson'" \
              -e '~~~{.sh}' -e ' significant ' -e ' significantly ' "$PAGE";
        # force no highlighting, because the terminal escape codes trigger bracket-matching
        egrep --only-matching '^\[\^.*\]: ' "$PAGE" | sort | uniq --count | \
            fgrep --invert-match "      1 [^";

        # image hotlinking deprecated; impolite, and slows page loads & site compiles
        egp --only-matching '\!\[.*\]\(http://.*\)' "$PAGE";
        # indicates broken copy-paste of image location
        egp --only-matching '\!\[.*\]\(wiki/.*\)' "$PAGE";

        markdown-length-checker "$PAGE";
        markdown-footnote-length "$PAGE";

        declare -r local HTML=`cat "$PAGE" | tail -n +3 | pandoc --mathml --standalone -`
        echo $HTML | fgp -e "<""del"">";
        echo $HTML | elinks -dump --force-html \
                   | fgp -e '\frac' -e '\times' -e '(http' -e ')http' -e '[http' -e ']http'  \
                           -e ' _ ' -e '[^' -e '^]' -e '<!--' -e '-->' -e '<-- ' -e '<—' -e '—>' \
                           -e '$title$' -e '<del>' \
                           -e '$description$' -e '$author$' -e '$tags$' -e '$category$' \
            -e '(!Wikipedia' -e '(!Hoogle'; # ))
    fi
done
