
#VERSION := $(shell grep -e "^version" doc/conf.py | sed -e "s/.*'\([^']*\)'/\1/")
VERSION := 1.0
PROJ_NAME := project-name
DIST_NAME := $(PROJ_NAME)-$(VERSION)
DEFAULT_STD := 2008

EXCLUDE_RTL := 

EXT_LIBS := extras
