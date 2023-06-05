# snappy-for-Delphi-Lazarus
The Google snappy lib bindings for Delphi\Lazarus

Introduction
==============
This is bindigns for the Google Snappy library for Delphi and Lazarus. Tested in Delphi 11 Alexandria and Lazarus 2.2
Also available the prebuild versions of the libsnappy for Windows x64, Linux x64, MacOS x64, MacOS ARM64. The libs have been compiled from source of original snappy [repo](https://github.com/google/snappy): 
with the [PowerTech] clang(https://github.com/powertech-center/clang). 
Of course, you are not required to use the prebuild version of libs, and compile it yourself with Clang or GCC (I didn't test with MSVC, srry:().
The oldest version of Delphi, like Delphi 7 isn't support.


How to use bindings:
====================
Just add libsnappy.pas from the src directory to the your project. If you want use the the static load of library define SNAPPY_STATIC_INIT. By default I prefer to use the dinamic loading.
Examples are located in the demo directory.

How to use the prebuild version of the libs:
============================================
For Windows
==============
Just place libsnappy.dll near an app.

For Linux
==============
libsnappy.so is necessary to place in /usr/lib , and after then do: chmod 777 libsnappy.so.

For MacOS
=========
libsnappy.dylib is necessary to place in /usr/local/lib , and after then do ctrl + click on library, and choise open( for the security issues).

Demo:
=====
![alt text](/demo/screens/VCL.jpg?raw=true "VCL")
![alt text](/demo/screens/FMX.jpg?raw=true "FMX")
![alt text](/demo/screens/MacOS.png?raw=true "LCL MacOS")
![alt text](/demo/screens/LinuxLCL.jpg?raw=true "LCL Linux")
![alt text](/demo/screens/WinLCL.jpg?raw=true "LCL Windows")

What is Snappy
==============

Snappy is a compression/decompression library. It does not aim for maximum compression, or compatibility with any other compression library; instead, it aims for very high speeds and reasonable compression. For instance, compared to the fastest mode of zlib, Snappy is an order of magnitude faster for most inputs, but the resulting compressed files are anywhere from 20% to 100% bigger. (For more information, see "Performance", below.)

Snappy has the following properties:

    Fast: Compression speeds at 250 MB/sec and beyond, with no assembler code. See "Performance" below.
    Stable: Over the last few years, Snappy has compressed and decompressed petabytes of data in Google's production environment. The Snappy bitstream format is stable and will not change between versions.
    Robust: The Snappy decompressor is designed not to crash in the face of corrupted or malicious input.
    Free and open source software: Snappy is licensed under a BSD-type license. For more information, see the included COPYING file.

Snappy has previously been called "Zippy" in some Google presentations and the like.
