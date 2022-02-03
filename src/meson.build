sources = ['env.F90',
           'tape.F90',
           'nested.f90',
           'vari.F90',
           'test.f90',
           'var.f90',
           'grad.f90',
           'sin.f90',
           'cos.f90',
           'tan.f90',
           'asin.f90',
           'acos.f90',
           'atan.f90',
           'sqrt.f90',
           'inv.f90',
           'square.f90',
           'inv_square.f90',
           'inv_sqrt.f90',
           'exp.f90',
           'log.f90',
           'relational.F90',
           'add.f90',
           'sub.f90',
           'mul.f90',
           'div.f90',
           'pow.f90',
           'sum.f90',
           'dot_product.f90',
           'matmul.f90',
           'fazang.f90']

fazanglib = library('fazang', sources, install: true)

fazang_dep = declare_dependency(link_with: fazanglib)

meson.add_install_script(files('install-mod.py'), get_option('includedir')/'fazang')

pkg = import('pkgconfig')
pkg.generate(fazanglib, description: 'Fortran library for reverse-mode automatic differentiation', subdirs: ['fazang'])
