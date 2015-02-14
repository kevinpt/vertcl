#!/bin/sh
# Remove comments from VHDL lines that end with %2008 DEBUG%
find . -name "*.vhdl" | xargs -n1 sed -i -e "s/^--\(.*%2008 DEBUG%\)$/\1/"
