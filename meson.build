# we need c++/c compiler for SUNDIALS
project('Fazang', ['fortran', 'c'], version : '0.1.0',
        meson_version: '>=0.59.0',
        default_options: ['default_library=shared', 'buildtype=debug'])

# TODO: support Windows
if meson.get_compiler('fortran').get_id() == 'gcc'
  add_project_arguments('-ffree-line-length-none', language : 'fortran')
endif

if get_option('sundials')
  add_project_arguments('-DUSE_SUNDIALS', language : 'fortran')
endif

# libsundials_dep = dependency('sundials', fallback : ['sundials', 'libsundials_dep'])

sources = files()

if get_option('sundials')
  subdir('lib/sundials-6.1.1')
endif

subdir('src')
subdir('test/unit')
subdir('test/unit/prob')

# ODE solver example using SUNDIALS::CVODES
if get_option('sundials')
  subdir('example/ode')
endif
