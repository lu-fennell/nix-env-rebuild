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
`~/.nixpkgs/store-path-install-list.txt`). 

Packages file example:
```
# file ~/.nixpkgs/packages.nix
with import <nixpkgs> {};
[ emacs
  firefox
]
```

Store path list example
```
# file ~/.nixpkgs/store-path-install-list.txt
#  Specify one store path per line.
#  Start comments with a hash (`#').
#  Blank lines are ignored.
/nix/store/k1i2i013n3p1y3bb6b48ljzz7iz29ajf-TeXLive-linkdir
/nix/store/4i32xkjrn50cjjjywncv2ala11cf40l6-openbox-3.6.1
```

Given the above file, running 

    nix-env-rebuild switch --packages ~/.nixpkgs/packages.nix --store-path-list ~/.nixpkgs/store-path-install-list.txt

results in a user profile containing 

- the packages `emacs` and `firefox` according to `<nixpkgs>`, and 
- the packages `TeXLive-linkdir` and `openbox` to which the given store-paths refer to
- and nothing else

## Commands

There are three commands:

- `dry-run`: Calculate and display a summary on how the selected
  profile would change when doing a rebuild. Does not actually install
  anything. See "Update output" below for an explanation of the summary.
- `build`: Install the declared packages into a *cache profile* (option `--cache-profile`, default `$NIX_USER_PROFILE_DIR/nix-env-rebuild-cache`). Does *not* modify the selected profile.
- `switch`: Install the declared packages into the selected profile.

## Update output
TODO

## Handling of the store-path list
TODO
