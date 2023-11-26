## Using Z3 from your Haskell Project

If your Haskell project wants to use the theorem prover Z3, you can use the Haskell package with the same name, `z3`: https://hackage.haskell.org/package/z3

* You of course first need to instal the theorem prover Z3 (check out Z3 site).

* Then you can `z3` in your project dependency (in your cabal-file).

* Then you can do `cabal build` on your project as usual.

##### Your project cannot find Z3 stuffs

A problem may arise when your project fails to find Z3's binary and header-files. [The hackage-site of `z3`](https://hackage.haskell.org/package/z3) should provide a clue how to deal with this. Quoting from there:

   _Unix-like:_ Just be sure to use the standard locations for dynamic libraries (`/usr/lib`) and header files (`/usr/include`), or else use the `--extra-lib-dirs` and `--extra-include-dirs` Cabal flags.

So, this means that your `cabal build` may have to pass the options mentioned above so that your project knows the location of Z3 stuffs.

##### Windows machine

On a Windows machine things can be a bit more complicated. It seems that the problem is that cabal, in Windows, does not recognize the above mentioned  `--extra-include-dirs` and `--extra-lib-dirs` options 😬.

Here is a workaround. From your console do these:

```
>  set LIBRARY_PATH="C:\workshop\tools\z3\z3-4.8.8\bin"
>  set C_INCLUDE_PATH="C:\workshop\tools\z3\z3-4.8.8\include"
>  set LD_LIBRARY_PATH="C:\workshop\tools\z3\z3-4.8.8\bin"
```

If you work with powershell, the syntax to set env-variable is not with `set var=value`, but `$Env:var=value`. So this is what I did:

```
> $Env:LIBRARY_PATH = "C:\workshop\tools\z3\z3-4.8.8\bin"
> $Env:C_INCLUDE_PATH = "C:\workshop\tools\z3\z3-4.8.8\include"
> $Env:LD_LIBRARY_PATH="C:\workshop\tools\z3\z3-4.8.8\bin"
```

After this you can do `cabal build`.

Note that if you close the console you will lose those env-variables setting. I leave it to you to figure out how to avoid having to types those command again.
