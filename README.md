# haskmorph

## Compiling

First install [Stack](https://docs.haskellstack.org) somewhere on your `PATH`.  For example, for `~/.local/bin`:

```
wget https://get.haskellstack.org/stable/linux-x86_64.tar.gz -O -|tar xpfz - -C /tmp
cp /tmp/stack-*/stack ~/.local/bin
rm -rf /tmp/stack-*
```

Then, while in the directory of this README file, run:

```
stack build
```

The first time this runs will take a while, 10 or 15 minutes, as it builds an entire Haskell environment from scratch.  Subsequent compilations are very fast.

## Running

Invoke the program using Stack.  To see available sub-commands, run:

```
stack exec -- haskseg -h
```

To see detailed help, run e.g.:

```
stack exec -- haskseg train -h
```
