---
layout: post
title: Fixing Samba file permissions issues on my Raspberry Pi
---

This weekend I attempted to mount a usb drive into my Raspberry Pi and set up a Samba share so that I could dump files into it over the network. The problem was that the share was visible in windows explorer, but any attempts to enter it would encounter an "you do not sufficient permissions" error.

<!--end excerpt-->

The usb was formatted using the FAT file system, so I ensured that in /etc/fstab the mount had the settings `gid=pi, uid=pi` (pi being the default user). The `samba.conf` was set using values suggested in multiple tutorials on how to set up the share and have it public with no password need.

After several ours of searching, I found one suggestion to set `force user=root` in `samba.conf`. And like magic, it worked! But I was not satisfied, since all of my previous attempts were based on tutorials that seemed to work for most other people.

Some I had not paid attention to what the result of `sudo ls -ld /mnt/usbdrv` (`usbdrv` being the mount point for `sda1`). The results were something along the lines of

    drwxrw-rw- pi pi

So the owner is indeed `pi`, and has read/write/execute permissions. So in theory, since I'm logged in as `pi` I should be able `ls` the folder to view its contents. But it was not to be, all I would get were access denied messages. So from here, I figured that the problem was not a Samba one, but an issue with the file permissions on the pi itself.

After a little research, I figured out that the execute bit (the `x` in `rwx`) needed to be set to view the contents of a file, and that the user needed the execute bit *for every parent folder*. Performing `sudo ls -ld /mnt` showed me that something like

    drw-rw-r- root root

So the problem was that *only root has execute permissions for the entire folder*. The fix was simply adding execute permissions for everyone. Once I had done this, I was able to open the share in Windows with out any issues at all. 
