#!/bin/bash

set -e
set -u

# Find latest version of Windows SDK

WinSDKIncludePath=/c/Program\ Files\ \(x86\)/Windows\ Kits/10/Include

# local latest
unset -v latest
for dir in "$WinSDKIncludePath"/*/; do
  [ -e "$dir" ] || continue
  [[ ! -v latest ]] || [[ $dir -nt $latest ]] && latest=$dir
done

[[ ! -v latest ]] && (echo "Could not find installed Windows 10 SDK. Please install Windows 10 SDK"; exit 1)

dir=${latest%*/}      # remove the trailing "/"
WinSDKVer=${dir##*/}    # print everything after the final "/"

echo "Found Windows 10 SDK version $WinSDKVer"

echo "Writing source/capture/msgmon.capture.vcxproj"
sed -E "s/\\\$WINSDKVER/$WinSDKVer/" < source/capture/msgmon.capture.vcxproj.in > source/capture/msgmon.capture.vcxproj

echo "Writing source/recorder/msgmon.recorder.vcxproj"
sed -E "s/\\\$WINSDKVER/$WinSDKVer/" < source/recorder/msgmon.recorder.vcxproj.in > source/recorder/msgmon.recorder.vcxproj

echo "Done. Now run source/build.sh"
