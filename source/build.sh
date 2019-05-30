#!/bin/bash

set -e
set -u

# Really naive build ;-)

display_usage ( ) {
    echo "build.sh [-x86] [-x64] [-debug]"
    echo
    echo "Build msgmon"
    echo "  -debug      Debug build"
    echo "  -x86        Build x86 target"
    echo "  -x64        Build x64 target"
    echo ""
    echo "If no parameters specified, builds both, in release version"
    exit 1
}

warn ( ) {
    echo "$*"
}

die ( ) {
    echo
    echo "$*"
    echo
    exit 1
}

# Environment. Update this as required for platform versions

MSBUILD="C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\MSBuild\\15.0\\Bin\\MSBuild.exe"
export BDS="C:\\Program Files (x86)\\Embarcadero\\Studio\\19.0"

# Default parameters

X64_BUILD=false
X86_BUILD=false
DEBUG_BUILD=false
CONFIG=Release

# Parse args
while [[ $# -gt 0 ]] ; do
    key="$1"
    case $key in
        -x64)
            X64_BUILD=true
            ;;
        -x86)
            X86_BUILD=true
            ;;
        -debug)
            DEBUG_BUILD=true
            ;;
        -h|-?)
            display_usage
            ;;
    esac
    shift # past argument
done

if [ "$X86_BUILD" = false ] && [ "$X64_BUILD" = false ]; then
    X64_BUILD=true
    X86_BUILD=true
fi

if [ "$DEBUG_BUILD" = true ]; then
    CONFIG=Debug
fi

pushd capture

if [ "$X86_BUILD" = true ]; then
    "$MSBUILD" //m //p:Configuration="$CONFIG" //p:PLATFORM="x86" || die "Failed to build x86 client"
fi

if [ "$X64_BUILD" = true ]; then
    "$MSBUILD" //m //p:Configuration="$CONFIG" //p:PLATFORM="x64" || die "Failed to build x64 capture"
fi
popd

# Install latest msgmon.man, updating paths accordingly

pushd ../build/bin/$CONFIG

WARNACCESS=0
MANFILESRCPATH=../../../source/capture/MsgMon.man
MANFILEBINPATH=./MsgMon.source.man

cmp --silent $MANFILESRCPATH $MANFILEBINPATH || {
    # If the file has 
    cp $MANFILESRCPATH $MANFILEBINPATH

    MANFILEPATH=$(pwd | sed -e 's/^\///' -e 's/\//\\/g' -e 's/^./\0:/')
    MANFILEPATHESC=$(echo "$MANFILEPATH" | sed -e 's/\\/\\\\/'g)
    sed -e "s/msgmon.capture.x86.dll/$MANFILEPATHESC\\\\\\0/g" < $MANFILEBINPATH > ./MsgMon.man

    # If we fail to install because of permissions, we don't kill the script
    # we just warn at the end.
    wevtutil um MsgMon.man || [ $? -eq 5 ] && WARNACCESS=1
    [ $WARNACCESS -eq 0 ] && wevtutil im MsgMon.man || [ $? -eq 5 ] && WARNACCESS=1
    if [[ WARNACCESS -eq 0 ]]; then
        # Successfully installed so let's just record that success
        cp $MANFILESRCPATH $MANFILEBINPATH
    fi
}

popd

# Build x86 client app

pushd client

if [ "$X86_BUILD" = true ]; then
    "$MSBUILD" //m //p:Config="$CONFIG" //p:PLATFORM="Win32" || die "Failed to build x86 client"
fi

popd

# Build x64 host app

pushd x64host

if [ "$X64_BUILD" = true ]; then
    "$MSBUILD" //m //p:Config="$CONFIG" //p:PLATFORM="Win64" || die "Failed to build x64 host"
fi

popd

if [ $WARNACCESS -ne 0 ]; then
    echo ""
    echo "==============================================================================="
    echo "WARNING"
    echo "-------"
    echo "The manifest was not installed due to insufficient permissions. If you have not"
    echo "already installed it, start an Administrative terminal and either run this"
    echo "script again, or run: "
    echo "  wevtutil um $MANFILEPATH\\MsgMon.man"
    echo "  wevtutil im $MANFILEPATH\\MsgMon.man"
    echo "==============================================================================="
fi
