#!/usr/bin/env python3

from os import environ, listdir, makedirs
from os.path import join, isdir, exists
from sys import argv
from shutil import copy

build_dir = join(environ["MESON_BUILD_ROOT"], "src")
if "MESON_INSTALL_DESTDIR_PREFIX" in environ:
    install_dir = environ["MESON_INSTALL_DESTDIR_PREFIX"]
else:
    install_dir = environ["MESON_INSTALL_PREFIX"]

include_dir = argv[1] if len(argv) > 1 else "include"
module_dir = join(install_dir, include_dir)

modules = []
for d in listdir(build_dir):
    bd = join(build_dir, d)
    if isdir(bd):
        for f in listdir(bd):
            if f.endswith(".mod"):
                modules.append(join(bd, f))

lib_build_dir = build_dir = join(environ["MESON_BUILD_ROOT"], "lib")
for d in listdir(lib_build_dir):
    bd = join(lib_build_dir, d)
    if isdir(bd):
        for f in listdir(bd):
            if f.endswith(".mod"):
                modules.append(join(bd, f))

if not exists(module_dir):
    makedirs(module_dir)

for mod in modules:
    print("Installing", mod, "to", module_dir)
    copy(mod, module_dir)
