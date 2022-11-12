# SE Basic IV 4.2 Cordelia
A classic BASIC interpreter for the Z80 architecture

Copyright © 1999-2022 Source Solutions, Inc.

## Build tools
Building this software locally requires:
* [Java](https://www.java.com)
* [JQ](https://stedolan.github.io/jq/)
* [Perl](https://www.perl.org/)
* [RASM](https://github.com/EdouardBERGE/rasm)

## Emulator
On macOS and Windows, the preferred emulator for development is [ZEsarUX](https://github.com/chernandezba/zesarux). On Linux it is [Fuse](https://fuse-emulator.sourceforge.net/).

For Fuse builds, uncomment out the `slam` directives in `basic.asm` and `boot.asm`.