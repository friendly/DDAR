#!/bin/sed -f -i~

#sed -i~ 
'/<a href="#"/ { \
    N \
    /\n.*<img class=/ { \
        s/^\( *<a href="\).*\(\n.*src="\)\([^"]*\)\(.*\)/\1\3" class="fancybox">\2\3\4/ \
    } \
}' 
