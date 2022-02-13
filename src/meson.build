sources += files('env.F90',
                 'arith.F90',
                 'tape.F90',
                 'nested.F90',
                 'vari.F90',
                 'array_in_tape.F90',
                 'test.F90',
                 'var.F90',
                 'value_of.F90',
                 'grad.F90',
                 'abs.F90',
                 'sin.F90',
                 'cos.F90',
                 'tan.F90',
                 'asin.F90',
                 'acos.F90',
                 'atan.F90',
                 'sinh.F90',
                 'cosh.F90',
                 'tanh.F90',
                 'asinh.F90',
                 'acosh.F90',
                 'atanh.F90',
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
                 'digamma.F90',
                 'log_gamma.F90',
                 'relational.F90',
                 'add.F90',
                 'sub.F90',
                 'mul.F90',
                 'div.F90',
                 'pow.F90',
                 'hypot.F90',
                 'sum.F90',
                 'norm2.F90',
                 'log_sum_exp.F90',
                 'dot_product.F90',
                 'matmul.F90',
                 'fazang.F90')

# CVODES interface
sources += files('cvodes_model.F90', 'cvodes.F90')

subdir('prob')

fazanglib = library('fazang', sources,
                    install: true,
                    dependencies : libsundials_dep)

fazang_dep = declare_dependency(link_with: fazanglib)

meson.add_install_script(files('install-mod.py'), get_option('includedir')/'fazang')

pkg = import('pkgconfig')
pkg.generate(fazanglib, description: 'Fortran library for reverse-mode automatic differentiation', subdirs: ['fazang'])
