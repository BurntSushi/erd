#!/usr/bin/env bash

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

curl --progress-bar --location -o$TEMP/$PACKAGE.tar.gz $URL
tar -xzf $TEMP/$PACKAGE.tar.gz -C$TEMP

shopt -s globstar
changed=()
for i in $1/**/*.hs; do
    $stl "$i" >"$i".tmp              ## To run on CI
    # stylish-haskell "$i" >"$i".tmp ## To run locally
    if ! diff -Nau "$i" "$i".tmp
    then
        changed+=("$i")
    fi
    rm "$i".tmp
done

if [[ ${#changed[@]} -ne 0 ]]
then
    echo "_________________________________"
    echo >&2 "Please run 'stylish-haskell':"
    echo >&2 "stylish-haskell -i" "${changed[@]}"
    echo "_________________________________"
    echo "Version of" $($stl --version)
    exit 1
fi

trap cleanup EXIT
