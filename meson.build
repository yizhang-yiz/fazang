project('Fazang', 'fortran', meson_version: '>=0.56')

add_global_arguments('-cpp', language : 'fortran')

subdir('src') 

t_debug = executable('debug', files('test/unit/debug_test.f90'), link_with : fazanglib)
test('debug', t_debug)

t_test = executable('test_test', files('test/unit/test_test.f90'), link_with : fazanglib)
test('test check routines', t_test)

t_vari = executable('vari_test', files('test/unit/vari_test.f90'), link_with : fazanglib)
test('test vari type', t_vari)

t_sin = executable('sin_test', files('test/unit/sin_test.f90'), link_with : fazanglib)
test('test sin function', t_sin)

t_cos = executable('cos_test', files('test/unit/cos_test.f90'), link_with : fazanglib)
test('test cos function', t_cos)

t_tan = executable('tan_test', files('test/unit/tan_test.f90'), link_with : fazanglib)
test('test tan function', t_tan)

t_asin = executable('asin_test', files('test/unit/asin_test.f90'), link_with : fazanglib)
test('test asin function', t_asin)

t_acos = executable('acos_test', files('test/unit/acos_test.f90'), link_with : fazanglib)
test('test acos function', t_acos)

t_atan = executable('atan_test', files('test/unit/atan_test.f90'), link_with : fazanglib)
test('test atan function', t_atan)

t_sqrt = executable('sqrt_test', files('test/unit/sqrt_test.f90'), link_with : fazanglib)
test('test sqrt function', t_sqrt)

t_exp = executable('exp_test', files('test/unit/exp_test.f90'), link_with : fazanglib)
test('test exp function', t_exp)

t_log = executable('log_test', files('test/unit/log_test.f90'), link_with : fazanglib)
test('test log function', t_log)

t_add = executable('add_test', files('test/unit/add_test.f90'), link_with : fazanglib)
test('test add overload', t_add)
