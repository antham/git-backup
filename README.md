# Git-backup [![CircleCI](https://circleci.com/gh/antham/git-backup.svg?style=svg)](https://circleci.com/gh/antham/git-backup) [![GitHub tag](https://img.shields.io/github/tag/antham/git-backup.svg)]() [![MELPA](http://melpa.org/packages/git-backup-badge.svg)](http://melpa.org/#/git-backup)

This library provides several helpers to implement a backup system on top of git.

## Dependencies

- git (>= 1.5) (binary)
- s (emacs library)

## Public functions documentation

See the `git-backup.el` internal documentation to get more details.

### git-backup-version-file

Records a file version in backup folder.

### git-backup-list-file-change-time

Returns an assoc list for a given file with the commit id as key and a string rendered using `git log pretty=format:<format-string>` with `format-string` a format string supported by `git log`.

### git-backup-remove-file-backups

Removes a file from the backup storage.

### git-backup-open-in-new-buffer

Opens a backup in a new buffer using a commit id given by `git-backup-list-file-change-time` and a filename.

### git-backup-replace-current-buffer

Replaces the current buffer with the content of the buffer found using a commit id given by `git-backup-list-file-change-time` and a filename.

### git-backup-create-ediff

Opens a backup with ediff using a commit id given by `git-backup-list-file-change-time` and a filename.

### git-backup-clean-repository

Runs manually `git gc` to reduce the size of the storage.

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
