org=jinjor
lib=elm-binary-decoder
dest=lib
version=master

if [ ! -e $lib ]; then
    git clone https://github.com/$org/$lib.git
fi

git -C $lib pull origin $version &&
rm -rf $dest
cp -r $lib/src $dest &&

repos=`cat $lib/elm-package.json | jq .repository`
user=`echo $repos | cut -d / -f4`
project=`echo $repos | cut -d / -f5 | cut -d . -f1`
ns1=`echo $user"$"$project | sed s/-/_/g`

repos=`cat elm-package.json | jq .repository`
user=`echo $repos | cut -d / -f4`
project=`echo $repos | cut -d / -f5 | cut -d . -f1`
ns2=`echo $user"$"$project | sed s/-/_/g`

find $dest/Native -name *.js | xargs sed -i "" s/$ns1/$ns2/g
