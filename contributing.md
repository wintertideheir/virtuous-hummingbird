# Rules and Guidelines for Constributors 

## Documentation

All JavaDoc documentation _must_ begin with `/**`.

All JavaDoc documentation _must_ continue leading asterisks aligned with the second asterisk of the beginning of the JavaDoc comment block for every line between the beginning and end of that JavaDoc comment block.

All JavaDoc documentation _must_ end with `*/` on a newline, with it's asterisk aligned with the second asterisk at the beginning of the JavaDoc comment.

All JavaDoc documentation _must_ seperate the initial line, comment body, parameters, return value, exceptions, and notes from each other.

## Model-View-Presenter

All functional code, except for the main application class, which shall be named `Application.java`, _must_ be part of either the model, the view, or the controller.

No part of the model shall have direct access to any part of the view, and vice versa.

Neither the model nor the view shall communicate with the controller except through messages.
