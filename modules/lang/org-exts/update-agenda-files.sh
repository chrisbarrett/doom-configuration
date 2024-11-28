#!/usr/bin/env bash

set -e -o pipefail

org_dir=${ORG_DIRECTORY:-$HOME/org}

cd "$org_dir"

{
    find . -maxdepth 1 -type f -name '*.org'
    find archive -type f -name '*.org'
    rg --follow --files-with-matches '^(CLOCK:|[*]+ +(TODO|WAIT))' roam -g '!attach' -g '!daily'

} >"$org_dir"/org-agenda-files
