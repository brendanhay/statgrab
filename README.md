# Statgrab

Haskell bindings to the portable system statistics `libstatgrab` library.

## Table of Contents

* [Introduction](#introduction)
* [Compatibility](#compatibility)
* [Installation](#installation)
* [Examples](#examples)
* [Contributing](#contributing)
* [Licence](#licence)


## Introduction

`libstatgrab` is a library that provides cross platform access to statistics
about the system on which it's run. It's written in C and presents a selection
of useful interfaces which can be used to access key system statistics.
The current list of statistics includes CPU usage, memory utilisation, disk usage,
process counts, network traffic, disk I/O, and more.

The current list of supported and tested platforms includes OSX, FreeBSD, Linux,
NetBSD, OpenBSD, Solaris, DragonFly BSD, HP-UX and AIX.

Please see the `libstatgrab` [homepage](http://www.i-scream.org/libstatgrab/)
for more information.


## Compatibility

`libstatgrab 0.9.0` is required.

While the bindings should correctly build on the same platforms as supported by
the library, only OSX and Linux have currently been tested.


## Installation

Install `libstatgrab 0.9.0` from one of the mirrors: http://www.i-scream.org/libstatgrab/

```
cabal install
```


## Examples

> TODO


## Contributing

For any problems, comments or feedback please create an issue [here on GitHub](github.com/brendanhay/statgrab/issues).


## Licence

statgrab is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/)
