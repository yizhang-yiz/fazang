# we need c++/c compiler for SUNDIALS
project('Fazang', ['fortran', 'c'], version : '0.1.0',
        meson_version: '>=0.59.0',
        default_options: ['default_library=shared', 'buildtype=debug'])

# TODO: support Windows
if meson.get_compiler('fortran').get_id() == 'gcc'
  add_project_arguments('-ffree-line-length-none', language : 'fortran')
endif

# libsundials_dep = dependency('sundials', fallback : ['sundials', 'libsundials_dep'])

sources = files()

# subdir('lib/sundials-6.1.1')
subdir('src')
subdir('test/unit')
subdir('test/unit/prob')
subdir('example/ode')
