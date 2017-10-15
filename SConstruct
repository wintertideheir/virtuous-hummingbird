import subprocess

env = Environment(CPPPATH='/usr/include/')
env.Append(CFLAGS='-std=c11')
env.VariantDir('bin', 'src', duplicate=0)
lib = env.SharedLibrary(target='bin/lib/aikaterine',
                        source='bin/lib/aikaterine.c',
                        LIBS=['glib-2.0'],
                        CCFLAGS=subprocess.check_output(
                            ['pkg-config', '--cflags', '--libs', 'glib-2.0'])
                        .strip())
env.Program(target='bin/app/aikaterine_desktop',
            source=env.Depends(['bin/app/desktop.c', 'bin/app/drawing.c'],
                               '#/log/libinstall'),
            LIBS=['glfw', 'GL', 'GLEW', 'm', 'aikaterine'])
env.Command('#/log/libinstall', [env.Install('/usr/local/lib', lib),
                                 env.Install('/usr/local/include',
                                             '#/src/lib/aikaterine.h')],
            '/sbin/ldconfig > $TARGET 2>&1')
