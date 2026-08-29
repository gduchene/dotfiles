#!/usr/bin/env bash

usage() {
  echo >&2 "usage: $0 [-b URL] [-v VERSION]"
}

base_url=https://github.com/be5invis/Iosevka/archive/refs/tags/
version=v34.8.1
while getopts b:v: opt; do
  case "${opt}" in
    b) base_url="${OPTARG%/}/" ;;
    v) version="${OPTARG}"     ;;
    *) usage; exit 1           ;;
  esac
done

set -euxo pipefail

CUR="${PWD}"
TMP="$(mktemp -d)" && trap 'rm -Rf "${TMP}"' EXIT
URL="${base_url}${version}.tar.gz"

cp private-build-plans.toml "${TMP}"
pushd "${TMP}"
curl --location --silent "${URL}" | tar --extract --gzip --strip-components=1
npm ci
npm run build ttf::IosevkaBaguette
cp -R dist "${CUR}"
popd
