#!/bin/sh

mkdir -p local
DESTDIR="$(pwd)"/local
export LD_LIBRARY_PATH="$DESTDIR/lib:$LD_LIBRARY_PATH"
export PKG_CONFIG_PATH="$DESTDIR/lib/pkgconfig:$PKG_CONFIG_PATH"
export PATH="$DESTDIR/bin:$PATH"

if ! [ -f "$DESTDIR/lib/libbitcoin.so" ] ; then

    (git clone git@github.com:bitcoin-core/secp256k1.git && \
            cd secp256k1 && \
            ./autogen.sh && \
            ./configure --prefix="$DESTDIR" --enable-module-recovery && \
            make && \
            make install && \
            cd ..) \
        || exit 2

    (git clone git@github.com:libbitcoin/libbitcoin.git && \
            cd libbitcoin && \
            git checkout v3.0.0 && \
            ./autogen.sh && \
            ./configure --prefix="$DESTDIR" --with-icu && \
            make && \
            make install && \
            cd ..) \
        || exit 2
fi

if ! [ -f "$DESTDIR/lib/libbitcoin-c.so" ] ; then

    (git clone git@github.com:vbmithr/libbitcoin-c.git && \
            cd libbitcoin-c && \
            git checkout makepkg && \
            ./autogen.sh && \
            ./configure --prefix="$DESTDIR" && \
            make && \
            make install && \
            cd ..) \
        || exit 2

fi

if ! [ -d "ocaml-libbitcoin" ] ; then
    git clone git@github.com:vbmithr/ocaml-base58.git || exit 2
    git clone git@github.com:vbmithr/ocaml-libbitcoin.git || exit 2
    git clone git@github.com:vbmithr/ocaml-blockexplorer.git || exit 2
    git clone git@github.com:vbmithr/ocaml-crowdsale-tools.git || exit 2
else
    git -C ocaml-base58 pull
    git -C ocaml-libbitcoin pull
    git -C ocaml-blockexplorer pull
    git -C ocaml-crowdsale-tools pull
fi

opam install \
     jbuilder \
     base \
     ctypes-foreign \
     ctypes \
     rresult \
     ptime \
     ezjsonm \
     ocplib-json-typed \
     hex \
     stdio \
     cmdliner \
     sodium \
 || exit 2
opam pin add configurator git@github.com:janestreet/configurator.git --yes \
 || exit 2

jbuilder build @install @runtest

echo To launch one of the tools, do:
echo "  " export LD_LIBRARY_PATH="$DESTDIR/lib:\$LD_LIBRARY_PATH"
echo "  " export PKG_CONFIG_PATH="$DESTDIR/lib/pkgconfig:\$PKG_CONFIG_PATH"
echo "  " export PATH="$DESTDIR/bin:\$PATH"
echo And then call one of: `ls -1 _build/install/default/bin/`
