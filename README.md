kropaya
=======

[![Join the chat at https://gitter.im/flaviusb/kropaya](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/flaviusb/kropaya?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

A fantasy language, unhindered by reality.

Ur-Principle: Like [Atomish](https://github.com/flaviusb/Atomish), but starting from first class labels and row polymorphism instead of starting from message sends to activatable cells.

Does this even make sense? I am not yet sure.

The Principles
--------------

- First class labels and row polymorphic types.
- Reified Readers, Evallers, Printers, Universes, Runloops, and Mirrors.
- Parameterised modules a la Functors, with explicit support for mutual recusion. This allows, amongst other things, really good tree shaking and permgen trimming, as well as elegantly solving the conflicting dependencies problem.
- Generation of statically and dynamically linked binaries.
- Something like a logical extension of functional core/imperative shell.
- Use of staged computation, metaprogramming, generic programming, and tree rewriting for optimisation.
- No C or C++ anywhere (except as needed for platform/library linkage eg for Open GL support).
- Type-directed metaprogramming.
- Locatives, multiple typing.
- Explicit, extensive use of quantification.
- Both parametric (ie 'Theorems for Free') and non-parametric code, with clear edges to prevent breaking invariants.
- Swapping out the reader mid read/the evaller mid eval.
- A continuum of compile-type to run-time, rather than a binary.

Bibliography/Inspirations/Influences
------------------------------------

(TODO: Expand this out way more fully)


Current inspirations:


MetaOCaml/BER. Haskell. ISWIM.

Language-Oriented programming inspired by OMeta and the STEPS VPRI project, as well as Kuro/Shiro.

Staged computation inspired by the Shonan Challenge, and especially the papers [http://okmij.org/ftp/meta-programming/HPC.html](here).


Past inspirations:


Inspired by Cola/Pepsi/Idst/Maru, Ioke, Common Lisp and Haskell.

Other influences: Perl 5&6 (Guts on the outside, TMTWTDI, Moose, 6Model), Io (Mirrors), OCaml (module system), Potion.

Minor influences: Atomy, Atomo, Slate, Erlang, Snobol, MetaLua.


Is it any good?
---------------

At the moment, no. There is little running code, and basically no documentation.

Project Organisation
--------------------

Each subfolder has it's own `README.md` detailing relevant things.

The project is licensed under the GPLv3; details in `LICENSE`. Vendored files are available under their own licenses, which are listed either at the root of their vendored directory or at the top of each file.

Installing instractions will eventually be in `INSTALL.md`.

General project layout explanations are in `LAYOUT.md`.

My assorted noodling and ill-considered thoughts at any given time about the project are in `scratch.md`.

General syntax and semantics of the language are in `kropaya.md`.
