# nix-env-rebuild

Declaratively manage a nix user environment. 

`nix-env-rebuild` works similarly to how `nixos-rebuild` works for
system packages: it reads a set of *declared packages* from
configuration files and modifies the user profile such that it
contains exactly those declared packages.

The set of declared packages is determined by a Nix-expression read
from the *packages file* (option `--packages`, default
`~/.nixpkgs/packages.nix`) and, optionally, by a list of store-paths
(option `--store-path-list`, default
`~/.nixpkgs/store-path-install-list.txt`). Store paths override
entries in the packages file.

Packages file example:
```
# file ~/.nixpkgs/packages.nix
with import <nixpkgs> {};
[ emacs
  firefox
  owncloudclient
  cabal-install
]
```

Store path list example
```
# file ~/.nixpkgs/store-path-install-list.txt
#  Specify one store path per line.
#  Start comments with a hash ('#').
#  Blank lines are ignored.
/nix/store/k1i2i013n3p1y3bb6b48ljzz7iz29ajf-TeXLive-linkdir
/nix/store/fb0x101j8aprznhnjhyj2vvny81k6b15-git-2.6.3
/nix/store/wd4d4i3l2ywij3q8pzhzn4kxmxx29av3-cabal-install-1.22.7.0
```

Given the above files, running 

    nix-env-rebuild switch 

results in a user profile containing 

- the packages `emacs`, `firefox`, and `owncloudclient` according to nixpkgs, and 
- the packages `TeXLive-linkdir`, `git`, and `cabal-install` according to the given store-paths
- and nothing else

(note that in this example, the store-path
`/nix/store/wd4d4i3l2ywij3q8pzhzn4kxmxx29av3-cabal-install-1.22.7.0`
overrides the `cabal-install` entry in `packages.nix`)

## Commands

There are three commands:

- `dry-run`: Calculate and display a summary on how the selected
  profile would change when doing a rebuild. Does not actually install
  anything. See "Update output" below for an explanation of the summary.
- `build`: Install the declared packages into a *cache profile* (option `--cache-profile`, default `$NIX_USER_PROFILE_DIR/nix-env-rebuild-cache`). Does *not* modify the selected profile.
- `switch`: Install the declared packages into the selected profile.

## Update summary explained

Suppose we have a current profile with these packages installed.

````
▶ nix-env -q  -p demo/demo-profile 
cabal-install-1.22.7.0
evince-3.18.2
firefox-42.0
git-2.7.0
owncloud-client-1.7.1
````
Using `packages.nix` and `store-path-install-list.txt` from above,
`nix-env-rebuild dry-run` will report the following:

````
* Calculating updates
  - packages from [...]
  - store-paths from [...]
  - search path [...]


Updating:
  firefox (42.0 -> 44.0 Prebuilt)
  git (2.7.0 -> 2.6.3 Present)
 
Adding:
  TeXLive-linkdir
  emacs-24.5
 
Reinstalling from source:
  owncloud-client-1.7.1
 
Removing:
  evince-3.18.2
 
Updates through store-path packages:
  git (2.7.0 -> 2.6.3 Present)
 
Updates and reinstalls blocked by store-path packages:
  cabal-install (1.22.7.0 -> 1.22.8.0 Prebuilt)

Dry run. Not doing anything.
````

The summary has several categories:

- Updating: Packages that are updated because there is a newer version
  available in nixpkgs (here firefox and git).
- Adding: Packages that are not present in the current profile, but
  listed in `packages.nix` (emacs, TeXLive-linkdir).
- Removing: Packages that are currently installed but are not listed
  `packages.nix` or `store-paths.txt` (evince).
- Updates through store-path packages: the subset of updates that is
  triggered by `store-paths.txt`.
- Updates and reinstalls blocked by store-path packages: packages that
  would be updated according to nixpkgs but are kept at the currently
  installed version by an override in `store-paths.txt`.
