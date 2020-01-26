#!/usr/bin/env bash

branch=$1
rev=$2

PACKAGE=stylish-haskell
echo Downloading and running $PACKAGE...

RELEASES=$(curl --silent https://github.com/jaspervdj/$PACKAGE/releases)
URL=https://github.com/$(echo $RELEASES | grep -o '\"[^\"]*-linux-x86_64\.tar\.gz\"' | sed s/\"//g | head -n1)
VERSION=$(echo $URL | sed -e 's/.*-\(v[\.0-9]\+-linux-x86_64\)\.tar\.gz/\1/')
TEMP=$(mktemp --directory .$PACKAGE-XXXXX)
stl=$TEMP/$PACKAGE-$VERSION/$PACKAGE

cleanup(){
    rm -r $TEMP
}
trap cleanup EXIT

curl --progress-bar --location -o$TEMP/$PACKAGE.tar.gz $URL
tar -xzf $TEMP/$PACKAGE.tar.gz -C$TEMP

echo "____________"
repo=erd
git clone --depth=50 --branch=$branch https://github.com/mmzx/erd.git $repo
find $repo -name '*.hs' | xargs $stl -i
pushd $repo
git checkout $rev
changed=$(git diff --name-only --diff-filter=d $rev)
popd
rm -rf $repo

if [[ ${#changed[@]} -gt 1 ]]
then
    echo "_________________________________"
    echo >&2 "Please run 'stylish-haskell':"
    echo >&2 "stack exec -- stylish-haskell -i" "${changed[@]}"
    echo "_________________________________"
    echo "Version of" $($stl --version)
    exit 1
fi


