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

sin_test = executable('sin_test', files('test/unit/sin_test.f90'), link_with : fazanglib)
test('test sin function', sin_test)

cos_test = executable('cos_test', files('test/unit/cos_test.f90'), link_with : fazanglib)
test('test cos function', cos_test)

tan_test = executable('tan_test', files('test/unit/tan_test.f90'), link_with : fazanglib)
test('test tan function', tan_test)

asin_test = executable('asin_test', files('test/unit/asin_test.f90'), link_with : fazanglib)
test('test asin function', asin_test)

acos_test = executable('acos_test', files('test/unit/acos_test.f90'), link_with : fazanglib)
test('test acos function', acos_test)

atan_test = executable('atan_test', files('test/unit/atan_test.f90'), link_with : fazanglib)
test('test atan function', atan_test)

sqrt_test = executable('sqrt_test', files('test/unit/sqrt_test.f90'), link_with : fazanglib)
test('test sqrt function', sqrt_test)

exp_test = executable('exp_test', files('test/unit/exp_test.f90'), link_with : fazanglib)
test('test exp function', exp_test)

log_test = executable('log_test', files('test/unit/log_test.f90'), link_with : fazanglib)
test('test log function', log_test)
