#!/usr/local/bin/bash

cmtDir=/Users/msandifo/data/global/quakes/cmt17/ndk

pushd $cmtDir

cat $cmtDir/jan76_dec17.ndk  $cmtDir/2018/*18.ndk |	sed '$!N;$!N;$!N;$!N;s/\n/ /g'  > $cmtDir/tmpc.ndkj

cat $cmtDir/qcmt.ndk  |	sed '$!N;$!N;$!N;$!N;s/\n/ /g'  > $cmtDir/tmpq.ndkj

popd
