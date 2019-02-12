#!/usr/local/bin/bash

#cmtDir=/Users/msandifo/data/global/quakes/cmt17/ndk

pushd $1/ndk

cat ./jan76_dec17.ndk  ./2018/*18.ndk |	sed '$!N;$!N;$!N;$!N;s/\n/ /g'  > ../tmpc.ndkj

cat ./qcmt.ndk  |	sed '$!N;$!N;$!N;$!N;s/\n/ /g'  > ../tmpq.ndkj

popd
