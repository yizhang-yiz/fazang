# we need c++/c compiler for SUNDIALS
project('Fazang', ['cpp','fortran'], version : '0.0.1', meson_version: '>=0.59.0', default_options: ['default_library=shared'])

# TODO: support Windows
if meson.get_compiler('fortran').get_id() == 'gcc'
  add_project_arguments('-ffree-line-length-none', language : 'fortran')
endif

# libsundials_dep = dependency('sundials', fallback : ['sundials', 'libsundials_dep'])

subdir('lib/sundials_6.0.0')
subdir('src')
subdir('test/unit')
subdir('test/unit/prob')
