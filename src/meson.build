
sources = ['env.F90',
           'arith.F90',
           'tape.F90',
           'nested.F90',
           'vari.F90',
           'test.F90',
           'var.F90',
           'grad.F90',
           'sin.F90',
           'cos.F90',
           'tan.F90',
           'asin.F90',
           'acos.F90',
           'atan.F90',
           'sqrt.F90',
           'inv.F90',
           'square.F90',
           'inv_square.F90',
           'inv_sqrt.F90',
           'exp.F90',
           'log.F90',
           'logit.F90',
           'inv_logit.F90',
           'erf.F90',
           'erfc.F90',
           'relational.F90',
           'add.F90',
           'sub.F90',
           'mul.F90',
           'div.F90',
           'pow.F90',
           'sum.F90',
           'log_sum_exp.F90',
           'dot_product.F90',
           'matmul.F90',
           'fazang.F90']

fazanglib = library('fazang', sources, install: true)

fazang_dep = declare_dependency(link_with: fazanglib)

meson.add_install_script(files('install-mod.py'), get_option('includedir')/'fazang')

pkg = import('pkgconfig')
pkg.generate(fazanglib, description: 'Fortran library for reverse-mode automatic differentiation', subdirs: ['fazang'])
