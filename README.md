# gtk-example
An example of how to use [GTK+](https://www.gtk.org/) (the GIMP Toolkit) with
[Haskell](https://www.haskell.org/) on 64-bit Windows, based on the example
'Hello World' program at the [haskell-gi repository](https://github.com/haskell-gi/haskell-gi).

## GTK+ and Haskell (the Glasgow Haskell Compiler)

The following is based, in part, on the advice on Windows at the
[haskell-gi wiki](https://github.com/haskell-gi/haskell-gi/wiki/Using-haskell-gi-in-Windows).

Unless otherwise stated, commands are issued in a Windows Command Prompt.

1. Install the [MSYS2 installer](http://www.msys2.org/) for 64-bit Windows.

2. Open a MSYS2 shell. Use the `pacman` command to install packages
`mingw64/mingw-w64-x86_64-pkg-config`
`mingw64/mingw-w64-x86_64-gobject-introspection`
`mingw64/mingw-w64-x86_64-gtksourceview3` `mingw64/mingw-w64-x86_64-webkitgtk3`

```
pacman -S mingw64/mingw-w64-x86_64-pkg-config mingw64/mingw-w64-x86_64-gobject-introspection mingw64/mingw-w64-x86_64-gtksourceview3 mingw64/mingw-w64-x86_64-webkitgtk3
```

3. Set Windows environment variables. `PKG_CONFIG_PATH` is a variable that
specifies additional folders where `pkg-config` will search for its metadata
files (`.pc` files). `XDG_DATA_DIRS` is a variable specified by the
[XDG Base Directory Specification](https://specifications.freedesktop.org/basedir-spec/latest/).

````
PKG_CONFIG_PATH=C:\msys64\mingw64\lib\pkgconfig
XDG_DATA_DIRS=C:\msys64\mingw64\share
````

4. Add `C:\msys64\mingw64\bin` to the path (environment variable `PATH`). It is
important that the versions of `pkg-config` and `zlib1.dll` first encountered on
the path are the ones in `C:\msys64\mingw64\bin`. In that regard, the `stack`
environment modifies the path with the effect of prioritising versions of
`zlib1.dll` that do not work with GTK+. They can be disabled by renaming them
(for example, `zlib1.dll.old`). The locations of `pkg-config` and `zlib1.dll` in
a `stack` environment can be checked with commands:

```
stack exec where -- pkg-config
stack exec where -- zlib1.dll
```

5. Create a new Haskell project (for example, `gtk-example`):

```
stack new gtk-example simple
cd gtk-example
```

6. A [bug in GHC 8.2.2](https://ghc.haskell.org/trac/ghc/ticket/14382) means
that it does not work with package `gi-gtk`. Edit the `stack.yaml` file to
specify a `resolver` that uses GHC 8.0.2.

## Overloaded Labels

`haskell-gi` has extensive support for overloading, using the
`OverloadedLabels` extension introduced in GHC 8.0.
<https://github.com/haskell-gi/haskell-gi/wiki/Overloading>

### OverloadedLabels
Package `base` exposes module `GHC.OverloadedLabels` which defines a class:

```haskell
class IsLabel (x :: Symbol) a where
  fromLabel :: Proxy# x -> a
```

In code, `#title` (for example) is replaced by:

```haskell
(fromLabel @"title" @alpha proxy#)
```

and gives rise to a constraint `(IsLabel "title" alpha)` which must be satisfied
by the context.

This makes use of kind signatures, type-level literals and visibile type
application.

### KindSignatures
The `KindSignatures` extension enables kind signatures in class declarations.

```haskell
class MyClass (a :: *) b where ...
```

### DataKinds
The `DataKinds` extension enables type-level string literals of kind `Symbol`.
<https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=implicit#type-level-literals>

Every relevant datatype is promoted to be a kind.

Package `base` exposes module `GHC.TypeLits`, which defines:

```haskell
data Symbol
```

This is the kind of type-level symbols.

### Proxies
Package `base` exposes module `GHC.Exts`, which exports:

```haskell
data Proxy# a
```

The type constructor `Proxy#` is used to bear witness to some type variable.
It is used when you want to pass around proxy values for doing things like
modelling type applications. A `Proxy#` is unboxed, has a polymorphic kind,
and has no runtime representation, being totally free.

```haskell
proxy# :: Proxy# a
proxy# = let x = x in x
```

Witness for an unboxed `Proxy#` value, which has no runtime representation.

### TypeApplications
The `TypeApplications` extension enables visible type application in
expressions.
<https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=implicit#visible-type-application>
