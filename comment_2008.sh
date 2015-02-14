#!/bin/sh
# Comment out VHDL lines that end with %2008 DEBUG%
find . -name "*.vhdl" | xargs -n1 sed -i -e "/^[^-].*%2008 DEBUG%$/ s/^/--/"
