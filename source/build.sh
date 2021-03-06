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
if [[ ! -f $MSBUILD ]]; then
  MSBUILD="C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Professional\\MSBuild\\15.0\\Bin\\MSBuild.exe"
fi
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

#
# Build capture DLL for x86 and x64
#

pushd capture

if [ "$X86_BUILD" = true ]; then
    "$MSBUILD" //m //p:Configuration="$CONFIG" //p:PLATFORM="x86" || die "Failed to build x86 capture"
fi

if [ "$X64_BUILD" = true ]; then
    "$MSBUILD" //m //p:Configuration="$CONFIG" //p:PLATFORM="x64" || die "Failed to build x64 capture"
fi
popd

# Build x86 client app

pushd client

if [ "$X86_BUILD" = true ]; then
    "$MSBUILD" //m //p:Config="$CONFIG" //p:PLATFORM="Win32" || die "Failed to build x86 client"
fi

popd

# Build x86 recorder app -- controls event trace and loads events into database

pushd recorder

if [ "$X86_BUILD" = true ]; then
    "$MSBUILD" //m //p:Configuration="$CONFIG" //p:PLATFORM="x86" || die "Failed to build x86 recorder"
fi

if [ "$X64_BUILD" = true ]; then
    "$MSBUILD" //m //p:Configuration="$CONFIG" //p:PLATFORM="x64" || die "Failed to build x64 recorder"
fi

popd

# Copy sqlite DLL into target folder

cp ext/sqlite/sqlite3.dll ../build/bin/$CONFIG/
