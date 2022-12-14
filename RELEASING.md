# Releasing

This is the release process for artifacts in this repository.

## Prerequisits

Git Flow AVH Edition:
- https://github.com/petervanderdoes/gitflow-avh/wiki/Installation
- https://github.com/petervanderdoes/gitflow-avh/wiki/Reference:-git-flow-release

Verify version to be

    git flow version
    # 1.12.3 (AVH Edition)

    git flow init --defaults --showcommands

GREN

    nvm install --lts
    nvm use --lts
    npm install github-release-notes@0.17.2 -g # latest @0.17.3 is broken

## Git Flow release

Print current version(s)

    git fetch origin main develop
    git describe

New release version

    VERSION=x.y.z
    git checkout main
    git pull --ff-only origin main 
    git checkout develop
    git pull --ff-only origin develop
    git flow release start \
        --nofetch --showcommands \
        $VERSION
    
    
    node ./bump-version.js
    git diff

Take a look ...

    git commit -am "bump to $VERSION"

... and release

    git flow release finish \
        --nopush --nokeep --nofetch --showcommands \
        -m "released" \
        $VERSION
    git push origin main
    git push origin $VERSION

Release in github (requires your github token `export GREN_GITHUB_TOKEN=...`)

    yarn release:gh
    yarn release:changelog

 Update changelog and push develop

    git push origin develop
    
    git describe
