#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'

#
# Build emacs 29 similarly to the current emacs 28 installed via flatpack in Fedora
#
# Don't forget to byte-recompile-directory all packages if you're moving to a
# new emacs executable, or the packages will hallucinate when loaded. A failsafe
# way to do that is either delete your elpaca or straight directory (if you use
# these) or if you have your configuration in version control, nuke your local
# emacs.d and clone it again (although you would lose all history and things
# that are not under version control).
#
# This script is designed to run from inside of a local emacs git checkout.
#

sudo dnf builddep -y emacs
sudo dnf install -y libtree-sitter-devel wxBase-devel wxGTK3-devel libwebp-devel ImageMagick-devel

# NOTE: Both CC and PKG_CONFIG_PATH appear to be unused
CC='CFLAGS=-DMAIL_USE_LOCKF -O2 -flto=auto -ffat-lto-objects -fexceptions -g -grecord-gcc-switches -pipe -Wall -Werror=format-security -Wp,-U_FORTIFY_SOURCE,-D_FORTIFY_SOURCE=3 -Wp,-D_GLIBCXX_ASSERTIONS -specs=/usr/lib/rpm/redhat/redhat-hardened-cc1 -fstack-protector-strong -specs=/usr/lib/rpm/redhat/redhat-annobin-cc1  -m64  -mtune=generic -fasynchronous-unwind-tables -fstack-clash-protection -fcf-protection -fno-omit-frame-pointer -mno-omit-leaf-frame-pointer
LDFLAGS=-Wl,-z,relro gcc'
PKG_CONFIG_PATH=:/usr/lib64/pkgconfig:/usr/share/pkgconfig

# I had issues exectuting these-- mayb be needed
# make distclean
# make uninstall

git checkout emacs-29
git pull
git reset --hard HEAD

./autogen.sh

sudo ./configure \
     --prefix=$HOME/.local \
     --with-imagemagick \
     --without-pop \
     --with-mailutils \
     --with-pgtk \
     --without-x \
     --with-xwidgets \
     --with-cairo \
     --without-compress-install \
     --with-native-compilation \
     --with-json \
     --with-dbus \
     --with-gif \
     --with-jpeg \
     --with-png \
     --with-rsvg \
     --with-tiff \
     --with-webp \
     --with-gpm \
     --with-modules \
     --with-harfbuzz \
     build_alias=x86_64-redhat-linux-gnu \
     host_alias=x86_64-redhat-linux-gnu

sudo make -j8
sudo make install
