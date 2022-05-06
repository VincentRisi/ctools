#!/usr/bin/env python
import os, grp
groups = os.getgroups()
for g in groups:
  if g >= 1000:
    print grp.getgrgid(g)[0]