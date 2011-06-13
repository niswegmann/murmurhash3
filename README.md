MurmurHash3
===========

Synopsis
--------

32-bit non-cryptographic hashing using MurmurHash3.

Install
-------

Assuming you have installed the
[Haskell platform](http://hackage.haskell.org/platform/), use cabal:

    $ cabal install murmurhash3

Overview
--------

MurmurHash is a family of non-cryptographic hash functions suitable for
general hash-based lookup. This implementation uses MurmurHash3 and
generates 32-bit hash values.

The MurmurHash family of hash functions are described at the following
webpages:

  * <http://code.google.com/p/smhasher/>

  * <http://en.wikipedia.org/wiki/MurmurHash>

If you need to generate hashes from large pieces of data such as bytestreams
or if you need a cryptographic hash function this is the wrong package. If
you need to generate rapid hashes from small pieces of data, e.g. for
implementing hash-based lookup, this is the right package.

Bugs
----

Comments, bug reports, and patches will be much appreciated:

  * <niswegmann@gmail.com>

License
-------

This library is distributed under a CC0 1.0 Universal Public Domain Dedication:

  * <http://creativecommons.org/publicdomain/zero/1.0/>
