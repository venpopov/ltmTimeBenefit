#!/bin/bash
# This script is used to publish the website to gh-pages ensuring that the version of
# the documentation is backed up in a folder with the same name as the version
# get the current version from the version file
source _environment

# add a v to the version
DOCS_VERSION=v$(echo $DOCS_VERSION)

# create a dir _site/$(DOCS_VERSION) if it does not exist
mkdir -p _site/$DOCS_VERSION

# copy all files from _site to _site/$DOCS_VERSION, except folders that start with v followed by a number
rsync -av --exclude='v[0-9]*' _site/ _site/$DOCS_VERSION

# push to gh-pages
quarto publish gh-pages --no-render --no-prompt



