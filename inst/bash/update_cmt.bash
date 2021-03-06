#/usr/local/bin/bash

#cmtDir=~/data/global/quakes/cmt17/ndk

pushd $1/ndk #cmtDir

# if full download required

if [ ! -f jan76_dec17.ndk ]; then
    echo jan76_dec17.ndk "File not found!"
    wget -q https://www.ldeo.columbia.edu/~gcmt/projects/CMT/catalog/jan76_dec17.ndk
fi

# wget https://www.ldeo.columbia.edu/~gcmt/projects/CMT/catalog/jan76_dec17.ndk

# 2018 monthly files
wget –q -r -np -nH --cut-dirs=5 -R index.html* https://www.ldeo.columbia.edu/~gcmt/projects/CMT/catalog/NEW_MONTHLY/2018/

# 2019 monthly files
wget –q -r -np -nH --cut-dirs=5 -R index.html* https://www.ldeo.columbia.edu/~gcmt/projects/CMT/catalog/NEW_MONTHLY/2019/

# 2020 monthly files
wget –q -r -np -nH --cut-dirs=5 -R index.html* https://www.ldeo.columbia.edu/~gcmt/projects/CMT/catalog/NEW_MONTHLY/2020/


rm qcmt.ndk
wget –q  https://www.ldeo.columbia.edu/~gcmt/projects/CMT/catalog/NEW_QUICK/qcmt.ndk

rm robots.txt

popd
