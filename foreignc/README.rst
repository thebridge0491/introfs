Introfs.Foreignc
===========================================
.. .rst to .html: rst2html5 foo.rst > foo.html
..                pandoc -s -f rst -t html5 -o foo.html foo.rst

FFI sub-package for FSharp Intro examples project.

Installation
------------
source code tarball download:
    
        # [aria2c --check-certificate=false | wget --no-check-certificate | curl -kOL]
        
        FETCHCMD='aria2c --check-certificate=false'
        
        $FETCHCMD https://bitbucket.org/thebridge0491/introfs/[get | archive]/master.zip

version control repository clone:
        
        git clone https://bitbucket.org/thebridge0491/introfs.git

build example with make:
[sh] ./configure.sh [--prefix=$PREFIX] [--help]

make build [test]

make nugetadd [nugetinstall]

build example with msbuild:
[env LD_LIBRARY_PATH=$PREFIX/lib] msbuild /t:build [/t:test]

msbuild /t:nugetpack,nugetadd [/t:nugetinstall]

Usage
-----
        // PKG_CONFIG='pkg-config --with-path=$PREFIX/lib/pkgconfig'
        
        // $PKG_CONFIG --cflags --libs <ffi-lib>

        open Introfs.Foreignc
        
        ...
        
        let n = 5L in
        
        let res = Classic.factI n in
        
        ...

Author/Copyright
----------------
Copyright (c) 2021 by thebridge0491 <thebridge0491-codelab@yahoo.com>

License
-------
Licensed under the Apache-2.0 License. See LICENSE for details.
