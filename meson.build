project('Fazang', 'fortran', version : '0.0.1', meson_version: '>=0.59.0')

# TODO: support Windows
if meson.get_compiler('fortran').get_id() == 'gcc'
  add_global_arguments('-cpp', '-ffree-line-length-none', language : 'fortran')

  # to test alloc 
  # add_global_arguments('-cpp', '-ffree-line-length-none', '-DFZ_INIT_STACK_LEN=4', language : 'fortran')
elif meson.get_compiler('fortran').get_id() == 'intel'
  add_global_arguments('-fpp', language : 'fortran')
endif

subdir('src')
subdir('test/unit')
