#! /bin/bash

TIMESTAMP=$(date +"%F %T")

# git_repos=("org" "Documents/Arduino" "Documents/C" "Documents/MATLAB" "Documents/Python" "OpenFOAM/swint-10/run/swint/OpenFOAM_RUN" "OpenFOAM/swint-10/applications/OpenFOAM_solvers")
git_repos_file=$HOME/bin/$HOSTNAME/.git_repos
git_repos=$(cat $git_repos_file | sed "1 i All" | percol)

if [[ $git_repos == "All" ]]; then
    git_repos=$(cat $git_repos_file)
fi

for repo in $git_repos; do
    echo =========================$repo=========================
    cd $HOME/$repo
    # git status
    git pull
    git commit -m "$HOSTNAME at $TIMESTAMP"
    git push
done
