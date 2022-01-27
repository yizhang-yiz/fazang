project('autodiff', 'fortran', meson_version: '>=0.56')

# add_global_arguments('-DSOME_TOKEN=value', language : 'fortran')

subdir('src') 

t_debug = executable('debug', files('test/unit/debug_test.f90'), link_with : fazanglib)
test('debug', t_debug)

t_test = executable('test_test', files('test/unit/test_test.f90'), link_with : fazanglib)
test('test check routines', t_test)

t_adstack = executable('test_adstack', files('test/unit/adstack_test.f90'), link_with : fazanglib)
test('test AD adstack storage', t_adstack)

t_vari = executable('vari_test', files('test/unit/vari_test.f90'), link_with : fazanglib)
test('test vari type', t_vari)

# t_var = executable('test_var', files('test/unit/var_test.f90'), link_with : fazanglib)
# test('test var type', t_var)

sin_test = executable('sin_test', files('test/unit/sin_test.f90'), link_with : fazanglib)
test('test sin function', sin_test)
