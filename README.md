# Git-backup

This project provides a library on top of git to store changes and power of helm to fetch backups easily.

## Dependencies

* git (>= 1.5) (binary)
* s (emacs library)

For emacs prior version 24.3 :

* cl-lib (emacs library)

## Documentation

## Tests

First you need to install cask :

```bash
curl -fsSkL https://raw.github.com/cask/cask/master/go | python
```

Add cask to your path :

```bash
export PATH=$PATH:/home/YOUR_USERNAME/.cask/bin
```

Download dependencies using cask :

```bash
make downloads
```

Run tests :

```
make test
```
