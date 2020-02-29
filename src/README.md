# src

The `src` directory holds the code of the Aikaterine project. The code base is organized into modules. The name of the module is the same as the base name of the compilation unit with the greatest importance. Other files act either as auxiliary files or as interfaces with other modules. Modules preferentially interact by calls to interfaces from the primary compilation unit. In this way, the primary compilation unit makes the module concrete and comprehensible. When including headers from other modules, explicitly name the module through the path.
