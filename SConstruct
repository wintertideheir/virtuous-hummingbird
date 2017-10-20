import subprocess

if not COMMAND_LINE_TARGETS:
    print("\nThe Aikaterine Project\n" "\n"
          "All mentions to \"the project\" are references to both the library "
          "and application combined.\n"
          "Installing a component necessitates building it first.\n" "\n"
          "SCons Aliases:\n"
          " * build         Build the project.\n"
          " * build-lib     Build the library.\n"
          " * build-app     Build the application.\n"
          " * install       Install the library.\n"
          " * install-lib   Install the library.\n")
    Exit(0)

env = Environment(CPPPATH=['/usr/include/', 'src/lib/'])
env.Append(CFLAGS='-std=c11')
env.VariantDir('bin', 'src', duplicate=0)

lib = env.SharedLibrary(target='bin/lib/aikaterine',
                        source='bin/lib/aikaterine.c',
                        LIBS=['glib-2.0'],
                        CCFLAGS=subprocess.check_output(
                            ['pkg-config', '--cflags', '--libs', 'glib-2.0'])
                        .strip())
app = env.Program(target='bin/app/aikaterine_desktop',
                  source=['bin/app/desktop.c', 'bin/app/drawing.c',
                          'bin/app/shaders.c'],
                  LIBS=['glfw', 'GL', 'GLEW', 'm', lib])
libi = env.Command('log/libinstall', [env.Install('/usr/local/lib', lib),
                                      env.Install('/usr/local/include',
                                                  'src/lib/aikaterine.h')],
                   '/sbin/ldconfig > $TARGET 2>&1')

env.Alias('build', [lib, app])
env.Alias('build-lib', lib)
env.Alias('build-app', app)
env.Alias('install', libi)
env.Alias('install-lib', libi)
