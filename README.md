# haskseg

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

To train on the included data set from Goldwater, Griffiths, and Johnson 2009, run:

```
time zcat data/br-old-gold.txt.gz | perl -pe '$_=~s/ /\<GOLD\>/g;' | stack exec -- haskseg train --stateFile model.gz --iterations 3 --goldString "<GOLD>"
```

Note here the spaces are being replaced by a special string, to indicate boundaries to calculate F-score on (not necessary, but a nice way to track progress).  The model seems to converge after three iterations on this data set.  This takes about three minutes and achieves F-score of 0.61, somewhat higher than reported in Liang, Jordan and Klein 2010 (why is unclear).  To use the trained model to segment text, such as the training data set, run:

```
time zcat data/br-old-gold.txt.gz | perl -pe '$_=~s/ //g;' | stack exec -- haskseg segment --stateFile model.gz
```

Note that for this stage, spaces are simply removed, otherwise it treats whitespace as a static boundary (which may be what you want in other circumstances!).  The model takes about 4 seconds to segment the 9790 "words", about 2500 per second, though this stage is embarassingly parallel.  The output is in BPE format, i.e. with "@@" indicating the end of an internal morph.
