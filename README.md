```
$ git clone https://github.com/sunaemon/arch-diff.git
$ cd arch-diff
$ cargo build --install && sudo -E target/release/arch-diff run --outdir .
$ bash -x backup.sh backup
$ bash -x restore.sh backup /mnt
```

![ScreenShot](ScreenShot.png)
